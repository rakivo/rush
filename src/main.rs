use std::mem;
use std::ptr;
use std::path::Path;
use std::ffi::CString;
use std::fs::{self, File};
use std::time::SystemTime;
use std::process::ExitCode;
use std::str::{self, Lines};
use std::sync::{Arc, Mutex};
use std::collections::VecDeque;
use std::io::{self, Write, Stdout};
use std::os::fd::{FromRawFd, IntoRawFd};

use memmap2::Mmap;
use rayon::prelude::*;
use dashmap::{DashMap, DashSet};
use fxhash::{FxHashMap, FxHashSet, FxBuildHasher};

type StrHashSet<'a> = FxHashSet::<&'a str>;
type StrHashMap<'a, T> = FxHashMap::<&'a str, T>;

type StrDashSet<'a> = DashSet::<&'a str, FxBuildHasher>;
type StrDashMap<'a, T> = DashMap::<&'a str, T, FxBuildHasher>;

type Graph<'a> = StrHashMap::<'a, Arc::<StrHashSet<'a>>>;
type TransitiveDeps<'a> = StrHashMap::<'a, Arc::<StrHashSet<'a>>>;

const RUSH_FILE_NAME: &str = "build.rush";

#[inline]
fn read_rush() -> Option::<Mmap> {
    if let Some(path) = fs::read_dir(".")
        .expect("could not read cwd")
        .filter_map(|res| res.map(|e| e.path()).ok())
        .find(|path| {
            path.file_name()
                .map_or(false, |name| name.to_string_lossy() == RUSH_FILE_NAME)
        })
    {
        let file = File::open(path).ok()?;
        Some(unsafe { Mmap::map(&file) }.ok()?)
    } else {
        None
    }
}

#[repr(transparent)]
#[derive(Copy, Clone, Default)]
#[cfg_attr(feature = "dbg", derive(Debug))]
struct Loc(usize);

impl Loc {
    #[inline]
    #[cfg_attr(feature = "dbg", track_caller)]
    fn report(literal: &str) -> ! {
        #[cfg(feature = "dbg")] {
            panic!("{literal}")
        } #[cfg(not(feature = "dbg"))] {
            eprintln!("{literal}");
            std::process::exit(1)
        }
    }
}

macro_rules! report_fmt {
    ($loc: expr, $($arg:tt)*) => {
        format!{
            "{RUSH_FILE_NAME}:{row}: {msg}\n",
            row = $loc.0,
            msg = std::fmt::format(format_args!($($arg)*))
        }
    };
    ($loc: expr, $lit: literal) => {
        format!("{RUSH_FILE_NAME}:{row}: {msg}\n", row = $loc.0, msg = $lit)
    }
}

macro_rules! report {
    ($loc: expr, $($arg:tt)*) => { Loc::report(&report_fmt!($loc, $($arg)*)) }
}

#[derive(Clone)]
#[cfg_attr(feature = "dbg", derive(Debug))]
enum TemplateChunk<'a> {
    Static(&'a str),
    JoinedStatic(&'a str),
    Placeholder(&'a str),
}

#[derive(Clone, Default)]
#[cfg_attr(feature = "dbg", derive(Debug))]
struct Template<'a> {
    loc: Loc,
    chunks: Vec::<TemplateChunk<'a>>,
}

impl Template<'_> {
    const CONSTANT_PLACEHOLDERS: &'static [&'static str] = &["in", "out"];

    fn new(s: &str, loc: Loc) -> Template {
        let mut start = 0;
        let mut chunks = Vec::new();

        while let Some(i) = s[start..].find('$') {
            let i = start + i;
            if i > start && !s[start..i].trim().is_empty() {
                let trimmed_static = s[start..i].trim();
                if !trimmed_static.is_empty() {
                    chunks.push(TemplateChunk::Static(trimmed_static));
                }
            }

            let placeholder_start = i + 1;
            let placeholder_end = s[placeholder_start..]
                .find(|c: char| c != '_' && !c.is_alphanumeric())
                .map(|end| placeholder_start + end)
                .unwrap_or_else(|| s.len());

            if placeholder_start < placeholder_end {
                chunks.push(TemplateChunk::Placeholder(&s[placeholder_start..placeholder_end]));
            } else {
                report!(loc, "empty placeholder")
            }

            let suffix_start = placeholder_end;
            let suffix_end = s[suffix_start..]
                .find(|c: char| c == '$' || c.is_whitespace())
                .map(|end| suffix_start + end)
                .unwrap_or_else(|| s.len());

            if suffix_start < suffix_end {
                let suffix = &s[suffix_start..suffix_end];
                if !suffix.trim().is_empty() {
                    let mut suffix_iter = suffix.splitn(2, |c: char| c == '$');
                    if let Some(static_part) = suffix_iter.next() {
                        if !static_part.is_empty() {
                            chunks.push(TemplateChunk::JoinedStatic(static_part))
                        }
                    }
                    if let Some(remaining_suffix) = suffix_iter.next() {
                        start = suffix_end - remaining_suffix.len();
                        continue
                    }
                }
            }

            start = suffix_end;
        }

        if start < s.len() {
            let trimmed_static = s[start..].trim();
            if !trimmed_static.is_empty() {
                chunks.push(TemplateChunk::Static(trimmed_static))
            }
        }

        Template { loc, chunks }
    }

    fn check(&self, defs: &Defs) -> Result::<(), String> {
        for placeholder in self.chunks.iter().filter_map(|c| {
            match c {
                TemplateChunk::Placeholder(p) if !Self::CONSTANT_PLACEHOLDERS.contains(p) => Some(p),
                _ => None
            }
        }) {
            if !defs.contains_key(placeholder) {
                return Err(report_fmt!(self.loc, "undefined variable: {placeholder}"))
            }
        } Ok(())
    }

    fn compile_(&self, job: &CompiledJob, defs: &Defs) -> Result::<String, String> {
        let mut ret = String::new();
        for (i, c) in self.chunks.iter().enumerate() {
            let s = match c {
                TemplateChunk::Static(s) |
                TemplateChunk::JoinedStatic(s) => Ok(*s),
                TemplateChunk::Placeholder(placeholder) => match *placeholder {
                    "in" => Ok(job.inputs_str),
                    "out" => Ok(job.target),
                    _ => job.shadows
                        .as_ref()
                        .and_then(|shadows| shadows.get(placeholder).map(|shadow| shadow.value))
                        .or_else(|| defs.get(placeholder).map(|def| def.value))
                        .ok_or(report_fmt!(self.loc, "undefined variable: {placeholder}"))
                },
            }?;
            ret.push_str(s);
            if !matches!(self.chunks.get(i + 1), Some(TemplateChunk::JoinedStatic(..))) {
                ret.push(' ')
            }
        }
        _ = ret.pop();
        Ok(ret)
    }

    fn compile(&self, job: &Job, defs: &Defs) -> Result::<String, String> {
        let mut ret = String::new();
        for (i, c) in self.chunks.iter().enumerate() {
            let s = match c {
                TemplateChunk::Static(s) |
                TemplateChunk::JoinedStatic(s) => Ok(*s),
                TemplateChunk::Placeholder(placeholder) => match *placeholder {
                    "in" => Ok(job.inputs_wo_rule_str),
                    "out" => Ok(job.target),
                    _ => job.shadows
                        .as_ref()
                        .and_then(|shadows| shadows.get(placeholder).map(|shadow| shadow.value))
                        .or_else(|| defs.get(placeholder).map(|def| def.value))
                        .ok_or(report_fmt!(self.loc, "undefined variable: {placeholder}"))
                },
            }?;
            ret.push_str(s);
            if !matches!(self.chunks.get(i + 1), Some(TemplateChunk::JoinedStatic(..))) {
                ret.push(' ')
            }
        }
        _ = ret.pop();
        Ok(ret)
    }
}

#[derive(Default)]
#[cfg_attr(feature = "dbg", derive(Debug))]
struct Rule<'a> {
    command: Template<'a>,
    depfile: Option::<Template<'a>>,
    description: Option::<Template<'a>>,
}

impl<'a> Rule<'a> {
    #[inline]
    fn new(
        command: &'a str,
        description: Option::<&'a str>,
        depfile_path: Option::<&'a str>,
        command_loc: Loc,
        description_loc: Loc,
        depfile_path_loc: Loc,
    ) -> Self {
        Self {
            depfile: depfile_path.map(|dp| Template::new(dp, depfile_path_loc)),
            command: Template::new(command, command_loc),
            description: description.map(|d| Template::new(d, description_loc)),
        }
    }
}

#[derive(Clone, Default)]
#[cfg_attr(feature = "dbg", derive(Debug))]
struct Job<'a> {
    loc: Loc,
    target: &'a str,
    rule: &'a str,
    target_template: Template<'a>,
    inputs: Vec::<&'a str>,
    inputs_templates: Vec::<Template<'a>>,
    shadows: Option::<Arc::<Defs<'a>>>,
    inputs_wo_rule_str: &'a str,
    deps: Vec::<&'a str>,
    deps_templates: Vec::<Template<'a>>,
}

#[derive(Clone, Default)]
#[cfg_attr(feature = "dbg", derive(Debug))]
struct CompiledJob<'a> {
    loc: Loc,
    rule: &'a str,
    target: &'a str,
    shadows: Option::<Arc::<Defs<'a>>>,
    inputs_str: &'a str,
    inputs: Vec::<&'a str>,
    deps: Vec::<&'a str>,
}

#[repr(transparent)]
#[derive(Clone, Default)]
#[cfg_attr(feature = "dbg", derive(Debug))]
struct Def<'a> {
    value: &'a str
}

type Defs<'a> = StrHashMap::<'a, Def<'a>>;

#[cfg_attr(feature = "dbg", derive(Debug))]
struct Preprocessed<'a> {
    defs: Defs<'a>,
    jobs: StrHashMap::<'a, Job<'a>>,
    rules: StrHashMap::<'a, Rule<'a>>,
}

impl<'a> Preprocessed<'a> {
    #[inline(always)]
    fn job_mut(&mut self, target: &str) -> &mut Job<'a> {
        unsafe { self.jobs.get_mut(target).unwrap_unchecked() }
    }

    #[inline(always)]
    fn rule_mut(&mut self, name: &str) -> &mut Rule<'a> {
        unsafe { self.rules.get_mut(name).unwrap_unchecked() }
    }

    fn into_processed(self) -> Processed<'a> {
        let Preprocessed { defs, jobs, rules } = self;
        let jobs = jobs.iter().filter_map(|(.., job)| {
            if rules.contains_key(job.rule) {
                let target = match job.target_template.compile(&job, &defs) {
                    Ok(ok) => ok.leak() as &_,
                    Err(e) => report!(job.loc, "{e}")
                };

                let mut inputs_str = String::with_capacity(job.inputs_wo_rule_str.len() + 10);
                let inputs = job.inputs_templates.iter()
                    .zip(job.inputs.iter())
                    .map(|(template, ..)| {
                        match template.compile(&job, &defs) {
                            Ok(ok) => {
                                let compiled = ok.leak() as &_;
                                inputs_str.push_str(compiled);
                                inputs_str.push(' ');
                                compiled
                            },
                            Err(e) => report!(job.loc, "{e}")
                        }
                    }).collect::<Vec::<_>>();

                if !inputs_str.is_empty() { _ = inputs_str.pop() }
                let inputs_str = inputs_str.leak();

                let deps = job.deps_templates.iter()
                    .zip(job.deps.iter())
                    .map(|(template, ..)| {
                        match template.compile(&job, &defs) {
                            Ok(ok) => ok.leak() as &_,
                            Err(e) => report!(job.loc, "{e}")
                        }
                    }).collect::<Vec::<_>>();

                let job = CompiledJob {
                    deps,
                    target,
                    inputs,
                    inputs_str,

                    loc: job.loc,
                    rule: job.rule,
                    shadows: job.shadows.as_ref().map(Arc::clone)
                };
                Some((target, job))
            } else {
                None
            }
        }).collect::<StrHashMap::<_>>();

        Processed {jobs, rules, defs}
    }
}

#[cfg_attr(feature = "dbg", derive(Debug))]
struct Processed<'a> {
    defs: Defs<'a>,
    jobs: StrHashMap::<'a, CompiledJob<'a>>,
    rules: StrHashMap::<'a, Rule<'a>>,
}

#[derive(Default)]
#[cfg_attr(feature = "dbg", derive(Debug))]
enum Context<'a> {
    #[default]
    Global,
    Job {
        target: &'a str,
        shadows: Option::<Defs<'a>>,
    },
    Rule {
        name: &'a str,
        already_inserted: bool,
        depfile_path: Option::<&'a str>,
        depfile_path_loc: Option::<Loc>,
        description: Option::<&'a str>,
        description_loc: Option::<Loc>,
    }
}

#[cfg_attr(feature = "dbg", derive(Debug))]
struct Parser<'a> {
    cursor: usize,
    parsed: Preprocessed<'a>,
    context: Context<'a>
}

impl<'a> Parser<'a> {
    fn finish_shadows(&mut self) {
        match &mut self.context {
            Context::Job { target, shadows } => {
                let job = self.parsed.job_mut(target);
                if let Some(shadows) = shadows.take() {
                    job.shadows.replace(Arc::new(shadows));
                }
            },
            _ => {}
        };
    }

    fn parse_line(&mut self, line: &'a str) {
        let first = line.find(['\t', '\n', '\x0C', '\r', ' ']).map(|first_space| {
            (first_space, &line[..first_space])
        });

        if matches!(self.context, Context::Job {..} | Context::Rule {..}) {
            if line.is_empty() {
                self.finish_shadows();
                self.context = Context::Global;
                return
            }

            if matches!(first, Some((.., "build" | "rule"))) {
                self.finish_shadows();
                self.context = Context::Global
            }
        } else if line.is_empty() {
            return
        }

        let Some((first_space, first_token)) = first else {
            report!(Loc(self.cursor), "undefined token: {line}")
        };

        let Some(second_space) = line[first_space..].find(|c: char| !c.is_ascii_whitespace())
            .map(|p| p + first_space) else {
                return
            };
        
        let parse_def = || -> Def<'a> {
            let check_start = first_space;
            let check_end = (second_space + 1 + 1).min(line.len());
            if !line[check_start..check_end].contains('=') {
                report!(Loc(self.cursor), "expected `=` in variable definition")
            }
            let value = line[second_space + 1..].trim();
            Def { value }
        };

        match &mut self.context {
            Context::Job { shadows, .. } => {
                let name = first_token;
                let def = parse_def();
                match shadows.as_mut() {
                    Some(shadows) => { shadows.insert(name, def); },
                    None => {
                        let mut _shadows = StrHashMap::with_capacity(4);
                        _shadows.insert(name, def);
                        shadows.replace(_shadows);
                    },
                }
            },
            Context::Rule { name, depfile_path, depfile_path_loc, already_inserted, description_loc, description } => {
                match first_token {
                    "command" => {
                        let command = line[second_space + 1 + 1..].trim();
                        let command_loc = self.cursor;
                        let rule = Rule::new(
                            command,
                            *description,
                            *depfile_path,
                            Loc(command_loc),
                            description_loc.unwrap_or(Loc(command_loc)),
                            depfile_path_loc.unwrap_or(Loc(command_loc))
                        );
                        self.parsed.rules.insert(name, rule);
                        self.context = Context::Rule {
                            name,
                            depfile_path: *depfile_path,
                            already_inserted: true,
                            description: *description,
                            description_loc: *description_loc,
                            depfile_path_loc: *depfile_path_loc
                        }
                    },
                    "depfile" => {
                        let Def {value: depfile_path} = parse_def();
                        if *already_inserted {
                            let rule = self.parsed.rule_mut(name);
                            let depfile = Template::new(depfile_path, Loc(self.cursor));
                            rule.depfile = Some(depfile)
                        } else {
                            self.context = Context::Rule {
                                name,
                                already_inserted: true,
                                depfile_path_loc: Some(Loc(self.cursor)),
                                description: *description,
                                depfile_path: Some(depfile_path),
                                description_loc: *description_loc
                            }
                        }
                    },
                    "description" => {
                        let description_loc = Loc(self.cursor);
                        let description_str = line[second_space + 1 + 1..].trim();
                        if *already_inserted {
                            let rule = self.parsed.rule_mut(name);
                            rule.description = Some(Template::new(description_str, description_loc));
                        } else {
                            let description = Some(description_str);
                            self.context = Context::Rule {
                                name,
                                depfile_path: *depfile_path,
                                depfile_path_loc: *depfile_path_loc,
                                already_inserted: *already_inserted,
                                description_loc: Some(description_loc),
                                description
                            }
                        }
                    },
                    _ => if line.chars().next() != Some('#') {
                        report!{
                            Loc(self.cursor),
                            "undefined property: `{first_token}`, existing properties are: `command`, `description`"
                        }
                    }
                }
            },
            Context::Global => {
                match first_token {
                    "rule" => {
                        self.context = Context::Rule {
                            depfile_path: None,
                            depfile_path_loc: None,
                            name: line[second_space..].trim_end(),
                            already_inserted: false,
                            description: None,
                            description_loc: None,
                        }
                    },
                    "build" => {
                        let Some(colon_idx) = line.find(':') else {
                            report!(Loc(self.cursor), "expected colon after build target")
                        };
                        let post_colon = line[colon_idx + 1..].trim();
                        let target = line[first_space..colon_idx].trim();
                        let or_idx = post_colon.find('|');
                        let (inputs_str, deps) = if let Some(or_idx) = or_idx {
                            let inputs_str = post_colon[..or_idx].trim_end();
                            let deps = post_colon[or_idx + 1..].split_ascii_whitespace().collect();
                            (inputs_str, deps)
                        } else {
                            (post_colon, Vec::new())
                        };
                        let mut input_tokens = inputs_str.split_ascii_whitespace();
                        let Some(rule) = input_tokens.next() else { return };
                        let inputs = input_tokens.collect::<Vec::<_>>();
                        let inputs_wo_rule_str = inputs_str[rule.len() + 1..].trim_end();
                        let loc = Loc(self.cursor);
                        let target_template = Template::new(target, loc);
                        let inputs_templates = inputs.iter().map(|input| Template::new(input, loc)).collect();
                        let deps_templates = deps.iter().map(|input| Template::new(input, loc)).collect();
                        let job = Job {loc, target, deps_templates, inputs_templates, target_template, rule, inputs, inputs_wo_rule_str, deps, shadows: None};
                        self.parsed.jobs.insert(target, job);
                        self.context = Context::Job {target, shadows: None}
                    },
                    _ => {
                        let name = first_token;
                        let def = parse_def();
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
            let next_trimmed = next_line.trim();
            let concat = format!("{trimmed} {next_trimmed}");
            full_line = Box::leak(concat.into_boxed_str())
        } full_line.trim_start()
    }

    #[inline]
    fn parse(content: &'a str) -> Preprocessed<'a> {
        let mut parser = Self {
            cursor: 0,
            parsed: Preprocessed {
                defs: Defs::with_capacity(32),
                jobs: StrHashMap::with_capacity(32),
                rules: StrHashMap::with_capacity(32)
            },
            context: Context::default()
        };
        let mut lines = content.lines();
        while let Some(line) = lines.next() {
            parser.cursor += 1;
            if line.as_bytes().first() == Some(&b'#') { continue }
            let line = parser.handle_newline_escape(line, &mut lines);
            parser.parse_line(line)
        }
        #[cfg(feature = "dbg")] {
            println!("{:#?}", parser.parsed)
        } parser.parsed
    }
}

fn build_dependency_graph<'a>(
    parsed: &'a Processed,
    transitive_deps: &mut TransitiveDeps<'a>
) -> Graph<'a> {
    fn collect_deps<'a>(
        node: &'a str,
        parsed: &'a Processed,
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

        if let Some(job) = parsed.jobs.get(node) {
            if let Some(rule) = parsed.rules.get(job.rule) {
                if let Some(ref depfile_template) = rule.depfile {
                    if let Ok(depfile_path) = depfile_template.compile_(job, &parsed.defs) {
                        if let Ok(depfile) = fs::read_to_string(&depfile_path) {
                            let depfile = Box::leak(depfile.into_boxed_str());
                            let colon_idx = depfile.find(':').unwrap();
                            let depfile_deps = depfile[colon_idx + 1..]
                                .split_ascii_whitespace()
                                .filter(|f| *f != "\\");
                            deps.extend(depfile_deps);
                        }
                    }
                }
            }
        }

        #[inline(always)]
        fn is_system_header(path: &str) -> bool {
            path.starts_with("/usr/include/") || path.starts_with("/usr/lib/")
        }

        let transitive = deps.iter()
            .filter(|dep| !is_system_header(dep))
            .fold(Vec::with_capacity(deps.len()), |mut transitive, dep|
        {
            let deps = collect_deps(&dep, parsed, graph, visited, transitive_deps);
            transitive.extend(deps.iter().map(|h| *h));
            transitive
        });

        deps.extend(transitive);

        let deps = Arc::new(deps);

        {
            graph.insert(node, Arc::clone(&deps));
            transitive_deps.insert(node, Arc::clone(&deps));
        }

        deps
    }

    let n = parsed.jobs.len();
    let mut graph = StrHashMap::with_capacity(n);
    let mut visited = StrHashSet::with_capacity(n);

    for target in parsed.jobs.keys() {
        collect_deps(target, parsed, &mut graph, &mut visited, transitive_deps);
    } graph
}

fn topological_sort_levels<'a>(graph: &Graph<'a>) -> Vec::<Vec::<&'a str>> {
    let mut levels = Vec::new();
    let mut in_degree = StrHashMap::<i64>::with_capacity(graph.len());

    for (node, deps) in graph.iter() {
        in_degree.entry(node).or_insert(0);
        for dep in deps.iter() {
            *in_degree.entry(*dep).or_insert(0) += 1
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
                let e = unsafe { in_degree.get_mut(&*dep).unwrap_unchecked() };
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

#[cfg_attr(feature = "dbg", derive(Debug))]
#[derive(Copy, Clone, PartialEq, PartialOrd)]
struct Metadata {
    mtime: SystemTime
}

#[cfg_attr(feature = "dbg", derive(Debug))]
struct MetadataCache<'a> {
    files: StrDashMap::<'a, Metadata>
}

impl<'a> MetadataCache<'a> {
    #[inline]
    fn new(files_count: usize) -> Self {
        Self {files: DashMap::with_capacity_and_hasher(files_count, FxBuildHasher::default())}
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
    fn needs_rebuild(&self, job: &CompiledJob<'a>, transitive_deps: &TransitiveDeps<'a>) -> bool {
        // TODO: do something here if dependent file does not exist
        let mtimes = unsafe {
            transitive_deps.get(job.target).unwrap_unchecked()
        }.iter().filter_map(|dep| {
            self.mtime(*dep).ok()
        }).collect::<Vec::<_>>();

        let Ok(target_mtime) = self.mtime(job.target) else {
            return true
        };

        mtimes.into_par_iter().any(|src_mtime| src_mtime > target_mtime)
    }
}

#[cfg_attr(feature = "dbg", derive(Debug))]
struct Command {
    command: String,
    description: Option::<String>
}

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

    fn execute(self) -> io::Result::<CommandOutput> {
        let Self { command, description } = self;

        let (mut stdout_reader, stdout_writer) = Self::create_pipe()?;
        let (mut stderr_reader, stderr_writer) = Self::create_pipe()?;

        let cmd = CString::new(command.as_bytes())?;
        let args = [c"/bin/sh".as_ptr(), c"-c".as_ptr(), cmd.as_ptr(), ptr::null()];

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

        let env = [c"PATH=/usr/bin:/bin".as_ptr(), ptr::null()];

        let mut pid = 0;
        let ret = unsafe {
            libc::posix_spawn(
                &mut pid,
                c"/bin/sh".as_ptr(),
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

        Ok(CommandOutput {command, stdout, stderr, description})
    }
}

struct CommandOutput {
    stdout: String,
    stderr: String,
    command: String,
    description: Option::<String>
}

#[cfg_attr(feature = "dbg", derive(Debug))]
struct CommandBuilder<'a> {
    context: &'a Processed<'a>,
    stdout: Mutex::<Stdout>,
    processed: StrDashSet::<'a>,
    metadata_cache: MetadataCache<'a>,
    transitive_deps: TransitiveDeps<'a>
}

impl<'a> CommandBuilder<'a> {
    #[inline]
    fn new(
        context: &'a Processed,
        transitive_deps: TransitiveDeps<'a>,
    ) -> Self {
        let n = context.jobs.len();
        Self {
            context,
            stdout: Mutex::new(io::stdout()),
            processed: DashSet::with_capacity_and_hasher(n, FxBuildHasher::default()),
            metadata_cache: MetadataCache::new(n),
            transitive_deps
        }
    }

    #[inline(always)]
    fn print(&self, s: &str) -> io::Result::<()> {
        self.print_bytes(s.as_bytes())
    }

    #[inline(always)]
    fn print_bytes(&self, bytes: &[u8]) -> io::Result::<()> {
        #[cfg(feature = "dbg")] {
            self.stdout.lock().unwrap().write_all(bytes)
        } #[cfg(not(feature = "dbg"))] unsafe {
            self.stdout.lock().unwrap_unchecked().write_all(bytes)
        }
    }

    fn _resolve_and_run(&self, job: &CompiledJob<'a>) {
        // TODO: reserve total amount of jobs here
        let mut stack = vec![job];
        while let Some(job) = stack.pop() {
            if self.processed.contains(job.target) { continue }

            let mut all_deps_resolved = true;
            for input in job.inputs.iter() {
                if !self.processed.contains(input) {
                    if let Some(dep_job) = self.context.jobs.get(input) {
                        stack.push(job);
                        stack.push(dep_job);
                        all_deps_resolved = false;
                        break
                    } else {
                        #[cfg(feature = "dbg")] {
                            let msg = format!("dependency {input} is assumed to exist\n");
                            _ = self.print(&msg);
                        }
                    }
                }
            }

            if !all_deps_resolved { continue }

            if let Some(rule) = self.context.rules.get(job.rule) {
                self.processed.insert(job.target);

                if self.metadata_cache.needs_rebuild(job, &self.transitive_deps) {
                    let Some(command) = rule.command.compile_(job, &self.context.defs).map_err(|e| {
                        _ = self.print(&e);
                        rule.description.as_ref().and_then(|d| d.check(&self.context.defs).err()).map(|err| {
                            _ = self.print(&err)
                        });
                    }).ok() else {
                        continue
                    };

                    let description = rule.description.as_ref().and_then(|d| d.compile_(&job, &self.context.defs).map_err(|e| {
                        _ = self.print(&e)
                    }).ok());

                    let command = Command { command, description };
                    let CommandOutput {
                        stdout,
                        stderr,
                        command,
                        description,
                    } = match command.execute() {
                        Ok(ok) => ok,
                        Err(e) => {
                            let err = format!{
                                "could not execute job: {target}: {e}\n",
                                target = job.target
                            };
                            _ = self.print(&err);
                            continue
                        }
                    };

                    let cap = stdout.len() + stderr.len() + description.as_ref().map_or(command.len(), |d| d.len()) + 1;
                    let mut output = Vec::with_capacity(cap);
                    let command = description.unwrap_or(command);
                    output.extend(command.as_bytes());
                    output.push(b'\n');
                    output.extend(stdout.as_bytes());
                    output.extend(stderr.as_bytes());
                    _ = self.print_bytes(&output);
                } else {
                    let mut any_err = false;
                    if let Err(err) = rule.command.check(&self.context.defs) {
                        _ = self.print(&err);
                        any_err = true
                    }

                    if let Some(Err(err)) = rule.description.as_ref().map(|d| d.check(&self.context.defs)) {
                        _ = self.print(&err);
                        any_err = true
                    }

                    if !any_err {
                        let msg = format!("{target} is already built\n", target = job.target);
                        _ = self.print(&msg)
                    }
                }
            } else {
                let err = report_fmt!{
                    job.loc,
                    "no rule named: {rule} found for job {target}\n",
                    rule = job.rule,
                    target = job.target
                };
                _ = self.print(&err);
            }
        }
    }

    fn resolve_and_run(self, graph: &Graph) {
        let levels = topological_sort_levels(graph);

        #[cfg(feature = "dbg")]
        levels.iter().enumerate().for_each(|(i, level)| {
            println!("{i}: {level:?}")
        });

        levels.into_iter().for_each(|level| {
            level.into_par_iter().filter_map(|t| self.context.jobs.get(t)).for_each(|job| {
                self._resolve_and_run(job)
            });
        });
    }
}

fn main() -> ExitCode {
    let Some(rush) = read_rush() else {
        eprintln!("no rush file found in cwd");
        return ExitCode::FAILURE
    };

    let content = unsafe { str::from_utf8_unchecked(&rush[..]) };
    let processed = Parser::parse(content).into_processed();

    let n = processed.jobs.len();
    let mut transitive_deps = StrHashMap::with_capacity(n);
    let graph = build_dependency_graph(&processed, &mut transitive_deps);
    CommandBuilder::new(&processed, transitive_deps).resolve_and_run(&graph);

    ExitCode::SUCCESS
}
