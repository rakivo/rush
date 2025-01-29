use std::path::Path;
use std::fs::{self, File};
use std::time::SystemTime;
use std::sync::{Arc, Mutex};
use std::collections::VecDeque;
use std::process::{Command, ExitCode};

use memmap2::Mmap;
use rayon::prelude::*;
use dashmap::{DashMap, DashSet};
use fxhash::{FxHashMap, FxHashSet};

type StrHashSet<'a> = FxHashSet::<&'a str>;
type StrHashMap<'a, T> = FxHashMap::<&'a str, T>;

type Graph<'a> = StrHashMap::<'a, StrHashSet<'a>>;
type TransitiveDeps<'a> = StrHashMap::<'a, StrHashSet<'a>>;

const RUSH_FILE_NAME: &str = "build.rush";

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
    fn new(command: &'a str) -> Self {
        Self {
            command,
            template: Self::template(command)
        }
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
        let start = line.chars().take_while(|c| c.is_ascii_whitespace()).count();

        if start == 0 && matches!(self.context, Context::Rule(..)) {
            self.context = Context::Global;
            return
        }

        let Some(first_space) = line[start..].chars()
            .position(|c| c.is_ascii_whitespace())
            .map(|p| p + start) else {
                return
            };

        let Some(second_space) = line[first_space..].chars()
            .position(|c| !c.is_ascii_whitespace())
            .map(|p| p + first_space) else {
                return
            };

        let first_token = line[start..first_space].trim();

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
                        self.context = Context::Rule(&line[second_space..].trim())
                    },
                    "build" => {
                        let colon_idx = line.chars().position(|c| c == ':').unwrap();
                        let post_colon = line[colon_idx + 1..].trim();
                        let target = line[first_space..colon_idx].trim();
                        let or_idx = post_colon.chars().position(|c| c == '|');
                        let (inputs_str, deps) = if let Some(or_idx) = or_idx {
                            let inputs_str = post_colon[..or_idx].trim();
                            let deps = post_colon[or_idx + 1..].split_ascii_whitespace().collect();
                            (inputs_str, deps)
                        } else {
                            (post_colon, Vec::new())
                        };
                        let mut input_tokens = inputs_str.split_ascii_whitespace();
                        let Some(rule) = input_tokens.next() else { return };
                        let rule_len = rule.len();
                        let inputs = input_tokens.collect();
                        let ref inputs_wo_rule_str = inputs_str[rule_len..];
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

    fn parse(content: &'a str) -> Parsed<'a> {
        let mut parser = Self::default();
        for line in content.lines() {
            parser.cursor += 1;
            parser.parse_line(line)
        }
        parser.parsed
    }
}

// TODO: clone less here
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
    ) -> StrHashSet<'a> {
        if visited.contains(node) {
            return transitive_deps.get(node).cloned().unwrap_or_default();
        }

        visited.insert(node);

        let mut deps = parsed.jobs.get(node).map(|job| {
            job.inputs
                .iter()
                .chain(job.deps.iter())
                .cloned()
                .collect::<StrHashSet>()
        }).unwrap_or_default();

        let mut transitive = Vec::with_capacity(deps.len());
        for dep in deps.iter() {
 transitive.extend(collect_deps(dep, parsed, graph, visited, transitive_deps))
        }

        deps.extend(transitive);
        graph.insert(node, deps.iter().cloned().collect());
        transitive_deps.insert(node, deps.clone());

        deps
    }

    let mut graph = StrHashMap::default();
    graph.reserve(parsed.jobs.len());

    for target in parsed.jobs.keys() {
        collect_deps(target, parsed, &mut graph, visited, transitive_deps);
    } graph
}

fn topological_sort_levels<'a>(graph: &Graph<'a>) -> Vec::<Vec::<&'a str>> {
    let mut levels = Vec::new();
    let mut in_degree = StrHashMap::default();
    in_degree.reserve(graph.len());

    for (node, deps) in graph {
        in_degree.entry(node).or_insert(0);
        for dep in deps {
            *in_degree.entry(dep).or_insert(0) += 1
        }
    }

    let mut queue = in_degree.iter()
        .filter(|(.., degree)| **degree == 0)
        .map(|(node, ..)| *node)
        .collect::<VecDeque::<_>>();

    while !queue.is_empty() {
        let mut curr_level = Vec::with_capacity(queue.len());
        for _ in 0..queue.len() {
            let node = unsafe { queue.pop_front().unwrap_unchecked() };
            curr_level.push(node);

            let Some(deps) = graph.get(node) else { continue };
            for dep in deps {
                let e = unsafe { in_degree.get_mut(dep).unwrap_unchecked() };
                *e -= 1;
                if *e == 0 {
                    queue.push_back(*dep)
                }
            }
        }

        levels.push(curr_level)
    }

    #[cfg(feature = "dbg")]
    if in_degree.values().any(|&degree| degree > 0) {
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
        Self {
            files: DashMap::with_capacity(files_count),
        }
    }

    fn needs_rebuild(&self, job: &Job<'a>, transitive_deps: &StrHashMap::<StrHashSet::<'a>>) -> bool {
        #[inline]
        fn mtime<'a>(f: &'a str, cache: &MetadataCache<'a>) -> std::io::Result::<Metadata> {
            if let Some(mtime) = cache.files.get(f) {
                Ok(*mtime)
            } else {
                let p: &Path = f.as_ref();
                let m = Metadata { mtime: p.metadata()?.modified()? };
                cache.files.insert(f, m);
                Ok(m)
            }
        }

        // TODO: do something here if dependent file does not exist
        let mtimes = unsafe {
            transitive_deps.get(job.target).unwrap_unchecked()
        }.par_iter().filter_map(|dep| mtime(dep, self).ok()).collect::<Vec::<_>>();

        let Ok(target_mtime) = mtime(job.target, self) else {
            return true
        };

        mtimes.into_par_iter().any(|src_mtime| src_mtime > target_mtime)
    }
}

struct Commands(Arc::<Mutex::<Vec::<String>>>);

impl Commands {
    fn execute(&self) -> ExitCode {
        for command in self.0.lock().unwrap().iter() {
            println!("{command}");
            let Ok(out) = Command::new("sh")
                .arg("-c")
                .arg(command)
                .output() else {
                    return ExitCode::FAILURE
                };

            let out = if out.status.success() {
                unsafe { std::str::from_utf8_unchecked(&out.stdout) }
            } else {
                unsafe { std::str::from_utf8_unchecked(&out.stderr) }
            };
            print!("{out}")
        } ExitCode::SUCCESS
    }
}

struct CommandBuilder<'a> {
    parsed: &'a Parsed<'a>,
    built: DashSet::<&'a str>,
    commands: Arc::<Mutex::<Vec::<String>>>,
    metadata_cache: MetadataCache<'a>,
    transitive_deps: &'a StrHashMap::<'a, StrHashSet::<'a>>
}

impl<'a> CommandBuilder<'a> {
    fn new(parsed: &'a Parsed<'a>, transitive_deps: &'a StrHashMap::<StrHashSet::<'a>>) -> Self {
        let n = parsed.jobs.len();
        Self {
            parsed,
            built: DashSet::with_capacity(n),
            commands: Arc::new(Mutex::new(Vec::with_capacity(n))),
            metadata_cache: MetadataCache::new(n),
            transitive_deps
        }
    }

    fn _resolve_and_build(&self, job: &Job<'a>) {
        if self.built.contains(job.target) || !self.metadata_cache.needs_rebuild(job, &self.transitive_deps) {
            println!("{target} is already built", target = job.target);
            return
        }

        for input in job.inputs.iter() {
            if let Some(dep_job) = self.parsed.jobs.get(input) {
                self._resolve_and_build(dep_job)
            } else if !self.built.contains(input) {
                println!("dependency {input} is assumed to exist")
            }
        }

        if let Some(rule) = self.parsed.rules.get(job.rule) {
            let command = rule.template.compile(job, self.parsed);
            self.built.insert(job.target);
            self.commands.lock().unwrap().push(command)
        } else {
            eprintln!("no rule found for job: {target}", target = job.target)
        }
    }

    fn build(self, graph: &Graph) -> Commands {
        let levels = topological_sort_levels(graph);

        #[cfg(feature = "dbg")]
        levels.iter().enumerate().for_each(|(i, level)| {
            println!("{i}: {level:?}")
        });

        for level in levels.into_iter() {
            level.into_par_iter().for_each(|target| {
                if let Some(job) = self.parsed.jobs.get(target) {
                    self._resolve_and_build(job)
                }
            });
        }

        Commands(self.commands)
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

    let mut visited = StrHashSet::default(); visited.reserve(n);
    let mut transitive_deps = StrHashMap::default(); transitive_deps.reserve(n);
    let graph = build_dependency_graph(&parsed, &mut visited, &mut transitive_deps);

    let cmd_builder = CommandBuilder::new(&parsed, &transitive_deps);
    let commands = cmd_builder.build(&graph);
    commands.execute();

    ExitCode::SUCCESS
}
