use std::io;
use std::mem;
use std::ptr;
use std::ffi::CStr;
use std::path::Path;
use std::str::Lines;
use std::fs::{self, File};
use std::time::SystemTime;
use std::process::ExitCode;
use std::sync::{Arc, Mutex};
use std::collections::VecDeque;
use std::os::fd::{FromRawFd, IntoRawFd};

use memmap2::Mmap;
use rayon::prelude::*;
use dashmap::{DashMap, DashSet};
use fxhash::{FxHashMap, FxHashSet};

type StrHashSet<'a> = FxHashSet::<&'a str>;
type StrHashMap<'a, T> = FxHashMap::<&'a str, T>;

type Graph<'a> = StrHashMap::<'a, Arc::<StrHashSet<'a>>>;
type TransitiveDeps<'a> = StrHashMap::<'a, Arc::<StrHashSet<'a>>>;

const RUSH_FILE_NAME: &str = "build.rush";

const SHELL_CSTR: &CStr = &unsafe { CStr::from_bytes_with_nul_unchecked(b"/bin/sh\0") };
const ARG_C_CSTR: &CStr = &unsafe { CStr::from_bytes_with_nul_unchecked(b"-c\0") };
const ENV_PATH_CSTR: &CStr = &unsafe { CStr::from_bytes_with_nul_unchecked(b"PATH=/usr/bin:/bin\0") };

#[inline(always)]
fn to_str(bytes: &[u8]) -> &str {
    unsafe { std::str::from_utf8_unchecked(bytes) }
}

fn read_rush() -> Option::<Mmap> {
    if let Some(f) = fs::read_dir(".")
        .expect("could not read cwd")
        .filter_map(|res| res.map(|e| e.path()).ok())
        .find(|path| {
            path.file_name()
                .map_or(false, |name| name.to_string_lossy() == RUSH_FILE_NAME)
        })
    {
        let file = File::open(f).ok()?;
        Some(unsafe { Mmap::map(&file) }.ok()?)
    } else {
        None
    }
}

#[derive(PartialEq)]
#[cfg_attr(feature = "dbg", derive(Debug))]
enum TemplateChunk<'a> {
    Static(&'a str),
    Placeholder(&'a str),
}

#[derive(Default, PartialEq)]
#[cfg_attr(feature = "dbg", derive(Debug))]
struct Template<'a> {
    statics_len: usize,
    chunks: Vec::<TemplateChunk<'a>>,
}

impl Template<'_> {
    fn compile(&self, job: &Job, context: &Parsed) -> String {
        self.chunks.iter().flat_map(|c| {
            let s = match c {
                TemplateChunk::Static(s) => s,
                TemplateChunk::Placeholder(placeholder) => match *placeholder {
                    "in" => job.inputs_wo_rule_str,
                    "out" => job.target,
                    _ => if let Some(def) = context.defs.get(placeholder) {
                        def.value
                    } else {
                        panic!("undefined: {placeholder}")
                    }
                }
            };
            [s, " "]
        }).collect()
    }
}

#[derive(Default, PartialEq)]
#[cfg_attr(feature = "dbg", derive(Debug))]
struct Rule<'a> {
    command: &'a str,
    template: Template<'a>,
}

impl<'a> Rule<'a> {
    #[inline]
    fn new(command: &'a str) -> Self {
        Self {command, template: Self::template(command)}
    }

    fn template(command: &str) -> Template {
        let mut start = 0;
        let mut statics_len = 0;
        let mut chunks = Vec::new();

        while let Some(i) = command[start..].find('$') {
            let i = start + i;

            if i > start && !command[start..i].trim().is_empty() {
                let trimmed_static = command[start..i].trim();
                if !trimmed_static.is_empty() {
                    statics_len += trimmed_static.len();
                    chunks.push(TemplateChunk::Static(trimmed_static))
                }
            }

            let placeholder_start = i + 1;
            let placeholder_end = command[placeholder_start..]
                .find(|c: char| !c.is_alphanumeric() && c != '_')
                .map(|end| placeholder_start + end)
                .unwrap_or_else(|| command.len());

            if placeholder_start < placeholder_end {
                chunks.push(TemplateChunk::Placeholder(&command[placeholder_start..placeholder_end]));
            } else {
                panic!("empty placeholder")
            }

            start = placeholder_end
        }

        if start < command.len() {
            let trimmed_static = command[start..].trim();
            if !trimmed_static.is_empty() {
                statics_len += trimmed_static.len();
                chunks.push(TemplateChunk::Static(trimmed_static));
            }
        }

        Template { chunks, statics_len }
    }
}

#[derive(Default, PartialEq)]
#[cfg_attr(feature = "dbg", derive(Debug))]
struct Job<'a> {
    target: &'a str,
    rule: &'a str,
    inputs: Vec::<&'a str>,
    inputs_wo_rule_str: &'a str,
    deps: Vec::<&'a str>
}

#[repr(transparent)]
#[derive(Default, PartialEq)]
#[cfg_attr(feature = "dbg", derive(Debug))]
struct Def<'a> {
    value: &'a str
}

#[derive(Default)]
#[cfg_attr(feature = "dbg", derive(Debug))]
struct Parsed<'a> {
    jobs: StrHashMap::<'a, Job<'a>>,
    defs: StrHashMap::<'a, Def<'a>>,
    rules: StrHashMap::<'a, Rule<'a>>,
}

#[derive(Default, PartialEq)]
#[cfg_attr(feature = "dbg", derive(Debug))]
enum Context<'a> {
    #[default]
    Global,
    Rule(&'a str),
}

#[derive(Default)]
#[cfg_attr(feature = "dbg", derive(Debug))]
struct Parser<'a> {
    cursor: usize,
    parsed: Parsed<'a>,
    context: Context<'a>
}

impl<'a> Parser<'a> {
    fn parse_line(&mut self, line: &'a str) {
        if line.is_empty() && matches!(self.context, Context::Rule(..)) {
            self.context = Context::Global;
            return
        }

        let Some(first_space) = line.chars().position(|c| c.is_ascii_whitespace()) else {
            return
        };

        let Some(second_space) = line[first_space..].chars()
            .position(|c| !c.is_ascii_whitespace())
            .map(|p| p + first_space) else {
                return
            };

        let ref first_token = line[..first_space];

        match &self.context {
            Context::Rule(name) => {
                if first_token != "command" {
                    if line.is_empty() || line == "\n" {
                        self.context = Context::Global;
                        return
                    }
                }

                let command = line[second_space + 1 + 1..].trim();

                let rule = Rule::new(command);
                self.parsed.rules.insert(name, rule);
            },
            Context::Global => {
                match first_token {
                    "rule" => {
                        self.context = Context::Rule(line[second_space..].trim_end())
                    },
                    "build" => {
                        let colon_idx = line.chars().position(|c| c == ':').unwrap();
                        let post_colon = line[colon_idx + 1..].trim();
                        let target = line[first_space..colon_idx].trim();
                        let or_idx = post_colon.chars().position(|c| c == '|');
                        let (inputs_str, deps) = if let Some(or_idx) = or_idx {
                            let inputs_str = post_colon[..or_idx].trim_end();
                            let deps = post_colon[or_idx + 1..].split_ascii_whitespace().collect();
                            (inputs_str, deps)
                        } else {
                            (post_colon, Vec::new())
                        };
                        let mut input_tokens = inputs_str.split_ascii_whitespace();
                        let Some(rule) = input_tokens.next() else { return };
                        let rule_len = rule.len();
                        let inputs = input_tokens.collect();
                        let inputs_wo_rule_str = inputs_str[rule_len + 1..].trim_end();
                        let job = Job {target, rule, inputs, inputs_wo_rule_str, deps};
                        self.parsed.jobs.insert(target, job);
                    },
                    _ => {
                        let name = first_token;
                        let value = line[second_space + 1..].trim();
                        let def = Def { value };
                        self.parsed.defs.insert(name, def);
                    }
                };
            },
        };
    }

    fn handle_newline_escape(&mut self, line: &'a str, lines: &mut Lines<'a>) -> &'a str {
        let mut full_line = line.trim_end();
        while full_line.as_bytes().last() == Some(&b'$') {
            let trimmed = full_line[..full_line.len() - 1].trim_end();
            let Some(next_line) = lines.next() else { break };
            self.cursor += 1;
            let next_trimmed = next_line.trim();
            let concat = format!("{trimmed} {next_trimmed}");
            full_line = Box::leak(concat.into_boxed_str());
        } full_line.trim_start()
    }

    #[inline]
    fn parse(content: &'a str) -> Parsed<'a> {
        let mut parser = Self::default();
        let mut lines = content.lines();
        while let Some(line) = lines.next() {
            parser.cursor += 1;
            let line = parser.handle_newline_escape(line, &mut lines);
            parser.parse_line(line)
        } parser.parsed
    }
}

fn build_dependency_graph<'a>(
    parsed: &'a Parsed,
    visited: &mut StrHashSet<'a>,
    transitive_deps: &mut TransitiveDeps<'a>
) -> Graph<'a> {
    fn collect_deps<'a>(
        node: &'a str,
        parsed: &'a Parsed,
        graph: &mut Graph<'a>,
        visited: &mut StrHashSet<'a>,
        transitive_deps: &mut TransitiveDeps<'a>
    ) -> Arc::<StrHashSet<'a>> {
        if visited.contains(node) {
            return transitive_deps.get(node).cloned().unwrap_or_default();
        }

        visited.insert(node);

        let mut deps = parsed.jobs.get(node).map(|job| {
            job.inputs.iter()
                .chain(job.deps.iter())
                .cloned()
                .collect::<StrHashSet>()
        }).unwrap_or_default();

        let mut transitive = Vec::with_capacity(deps.len());
        for dep in deps.iter() {
            transitive.extend(collect_deps(dep, parsed, graph, visited, transitive_deps).iter().cloned())
        }

        deps.extend(transitive);

        let deps = Arc::new(deps);
        graph.insert(node, Arc::clone(&deps));
        transitive_deps.insert(node, Arc::clone(&deps));
        deps
    }

    let mut graph = StrHashMap::with_capacity(parsed.jobs.len());

    for target in parsed.jobs.keys() {
        collect_deps(target, parsed, &mut graph, visited, transitive_deps);
    } graph
}

fn topological_sort_levels<'a>(graph: &Graph<'a>) -> Vec::<Vec::<&'a str>> {
    let mut levels = Vec::new();
    let mut in_degree = StrHashMap::<i64>::with_capacity(graph.len());

    for (node, deps) in graph.iter() {
        in_degree.entry(node).or_insert(0);
        for dep in deps.iter() {
            *in_degree.entry(dep).or_insert(0) += 1
        }
    }

    let mut queue = in_degree.iter()
        .filter(|(.., degree)| **degree == 0)
        .map(|(node, ..)| *node)
        .collect::<VecDeque::<_>>();

    while !queue.is_empty() {
        let n = queue.len();
        let mut curr_level = Vec::with_capacity(n);
        for _ in 0..n {
            let node = unsafe { queue.pop_front().unwrap_unchecked() };
            curr_level.push(node);

            let Some(deps) = graph.get(node) else { continue };
            for dep in deps.iter() {
                let e = unsafe { in_degree.get_mut(dep).unwrap_unchecked() };
                *e -= 1;
                if *e == 0 {
                    queue.push_back(*dep)
                }
            }
        }

        levels.push(curr_level)
    }

    if cfg!(feature = "dbg") && in_degree.values().any(|d| d.is_positive()) {
        panic!("[FATAL] cycle has been detected in the dependency graph")
    }

    levels.reverse();
    levels
}

#[derive(Copy, Clone, PartialEq, PartialOrd)]
struct Metadata {
    mtime: SystemTime
}

struct MetadataCache<'a> {
    files: DashMap::<&'a str, Metadata>
}

impl<'a> MetadataCache<'a> {
    #[inline]
    fn new(files_count: usize) -> Self {
        Self {files: DashMap::with_capacity(files_count)}
    }

    #[inline]
    fn mtime(&self, f: &'a str) -> io::Result::<Metadata> {
        if let Some(mtime) = self.files.get(f) {
            Ok(*mtime)
        } else {
            let p: &Path = f.as_ref();
            let m = Metadata { mtime: p.metadata()?.modified()? };
            self.files.insert(f, m);
            Ok(m)
        }
    }

    #[inline]
    fn needs_rebuild(&self, job: &Job<'a>, transitive_deps: &TransitiveDeps<'a>) -> bool {
        // TODO: do something here if dependent file does not exist
        let mtimes = unsafe {
            transitive_deps.get(job.target).unwrap_unchecked()
        }.par_iter().filter_map(|dep| self.mtime(dep).ok()).collect::<Vec::<_>>();

        let Ok(target_mtime) = self.mtime(job.target) else {
            return true
        };

        mtimes.into_par_iter().any(|src_mtime| src_mtime > target_mtime)
    }
}

#[repr(transparent)]
struct Command(String);

// Custom implementation of `Command` to avoid fork + exec overhead
impl Command {
    fn create_pipe() -> io::Result::<(File, File)> {
        let mut fds = [0; 2];
        if unsafe { libc::pipe(fds.as_mut_ptr()) } != 0 {
            return Err(io::Error::last_os_error())
        }
        let r = unsafe { File::from_raw_fd(fds[0]) };
        let w = unsafe { File::from_raw_fd(fds[1]) };
        Ok((r, w))
    }

    fn execute(self) -> io::Result::<Output> {
        let Self(command) = self;

        let (mut stdout_reader, stdout_writer) = Self::create_pipe()?;
        let (mut stderr_reader, stderr_writer) = Self::create_pipe()?;

        let cmd = unsafe { CStr::from_ptr(command.as_ptr() as *const _) };
        let args = [SHELL_CSTR.as_ptr(), ARG_C_CSTR.as_ptr(), cmd.as_ptr(), ptr::null()];

        let stdout_writer_fd = stdout_writer.into_raw_fd();
        let stderr_writer_fd = stderr_writer.into_raw_fd();

        let mut file_actions = unsafe { mem::zeroed() };
        unsafe {
            libc::posix_spawn_file_actions_init(&mut file_actions);
            libc::posix_spawn_file_actions_adddup2(&mut file_actions, stdout_writer_fd, libc::STDOUT_FILENO);
            libc::posix_spawn_file_actions_adddup2(&mut file_actions, stderr_writer_fd, libc::STDERR_FILENO);
        }

        let mut attr = unsafe { mem::zeroed() };
        unsafe {
            libc::posix_spawnattr_init(&mut attr);
        }

        let env = [ENV_PATH_CSTR.as_ptr(), ptr::null()];

        let mut pid = 0;
        let ret = unsafe {
            libc::posix_spawn(
                &mut pid,
                SHELL_CSTR.as_ptr(),
                &file_actions,
                &attr,
                args.as_ptr() as *const *mut _,
                env.as_ptr() as *const *mut _
            )
        };

        if ret != 0 {
            return Err(io::Error::last_os_error())
        }

        unsafe {
            libc::close(stdout_writer_fd);
            libc::close(stderr_writer_fd);
        }

        let stdout = io::read_to_string(&mut stdout_reader)?;
        let stderr = io::read_to_string(&mut stderr_reader)?;

        let mut status = 0;
        unsafe {
            libc::waitpid(pid, &mut status, 0);
        }

        unsafe {
            libc::posix_spawn_file_actions_destroy(&mut file_actions);
            libc::posix_spawnattr_destroy(&mut attr);
        }

        Ok(Output {command, stdout, stderr})
    }
}

struct Output {
    stdout: String,
    stderr: String,
    command: String,
}

type Outputs = Mutex::<Vec::<Output>>;

struct CommandBuilder<'a> {
    parsed: &'a Parsed<'a>,
    compiled: DashSet::<&'a str>,
    metadata_cache: MetadataCache<'a>,    
    transitive_deps: &'a TransitiveDeps<'a>
}

impl<'a> CommandBuilder<'a> {
    #[inline]
    fn new(parsed: &'a Parsed, transitive_deps: &'a TransitiveDeps) -> Self {
        let n = parsed.jobs.len();
        Self {
            parsed,
            compiled: DashSet::with_capacity(n),
            metadata_cache: MetadataCache::new(n),
            transitive_deps
        }
    }

    fn _resolve_and_run(&self, job: &Job<'a>, outputs: &Outputs) {
        if self.compiled.contains(job.target) || !self.metadata_cache.needs_rebuild(job, &self.transitive_deps) {
            println!("{target} is already built", target = job.target);
            return
        }

        for input in job.inputs.iter() {
            if let Some(dep_job) = self.parsed.jobs.get(input) {
                self._resolve_and_run(dep_job, outputs)
            } else if !self.compiled.contains(input) {
                println!("dependency {input} is assumed to exist")
            }
        }

        if let Some(rule) = self.parsed.rules.get(job.rule) {
            self.compiled.insert(job.target);
            let command = Command(rule.template.compile(job, self.parsed));
            let output = match command.execute() {
                Ok(ok) => ok,
                Err(e) => {
                    eprintln!{
                        "could not execute job: {target}: {e}",
                        target = job.target
                    };
                    return
                }
            };
            outputs.lock().unwrap().push(output);
        } else {
            eprintln!("no rule found for job: {target}", target = job.target)
        }
    }

    fn resolve_and_run(self, graph: &Graph) {
        let levels = topological_sort_levels(graph);

        #[cfg(feature = "dbg")]
        levels.iter().enumerate().for_each(|(i, level)| {
            println!("{i}: {level:?}")
        });

        levels.into_iter().for_each(|level| {
            let outputs = Mutex::new(Vec::with_capacity(level.len()));
            level.into_par_iter().filter_map(|t| self.parsed.jobs.get(t)).for_each(|job| {
                self._resolve_and_run(job, &outputs)
            });
            for Output { stdout, stderr, command } in outputs.lock().unwrap().iter() {
                println!("{command}");
                print!("{stdout}");
                print!("{stderr}");
            }
        });
    }
}

fn main() -> ExitCode {
    let Some(rush) = read_rush() else {
        eprintln!("no rush file found in cwd");
        return ExitCode::FAILURE
    };

    let content = to_str(&rush[..]);
    let parsed = Parser::parse(content);

    let n = parsed.jobs.len();
    let mut visited = StrHashSet::with_capacity(n);
    let mut transitive_deps = StrHashMap::with_capacity(n);
    let graph = build_dependency_graph(&parsed, &mut visited, &mut transitive_deps);

    let cmd_builder = CommandBuilder::new(&parsed, &transitive_deps);
    cmd_builder.resolve_and_run(&graph);

    ExitCode::SUCCESS
}
