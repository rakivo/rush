use std::path::Path;
use std::fmt::Display;
use std::fs::{self, File};
use std::time::SystemTime;
use std::process::{Command, ExitCode};

use memmap2::Mmap;
use dashmap::DashMap;
use rayon::prelude::*;
use fxhash::{FxHashMap, FxHashSet};

type StrHashSet<'a> = FxHashSet::<&'a str>;
type StrHashMap<'a, T> = FxHashMap::<&'a str, T>;

type Graph<'a> = StrHashMap::<'a, StrHashSet<'a>>;

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

#[derive(Debug, PartialEq)]
enum TemplateChunk<'a> {
    Static(&'a str),
    Placeholder(&'a str),
}

#[derive(Debug, Default, PartialEq)]
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

#[derive(Debug, Default, PartialEq)]
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
        let mut chunks = Vec::new();
        let mut start = 0;
        let mut statics_len = 0;

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

#[derive(Debug, Default, PartialEq)]
struct Job<'a> {
    target: &'a str,
    rule: &'a str,
    inputs: Vec::<&'a str>,
    inputs_wo_rule_str: &'a str,
    deps: Vec::<&'a str>
}

#[derive(Debug, Default, PartialEq)]
struct Def<'a> {
    value: &'a str
}

#[derive(Debug, Default)]
struct Parsed<'a> {
    jobs: StrHashMap::<'a, Job<'a>>,
    defs: StrHashMap::<'a, Def<'a>>,
    rules: StrHashMap::<'a, Rule<'a>>,
}

#[derive(Debug, Default, PartialEq)]
enum Context<'a> {
    #[default]
    Global,
    Rule(&'a str),
}

#[derive(Debug, Default)]
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
    transitive_deps: &mut StrHashMap::<'a, Vec::<&'a str>>,
) -> Graph<'a> {
    fn collect_deps<'a>(
        node: &'a str,
        parsed: &'a Parsed,
        visited: &mut StrHashSet<'a>,
        graph: &mut Graph<'a>,
        transitive_deps: &mut StrHashMap::<'a, Vec::<&'a str>>,
    ) -> Vec::<&'a str> {
        if visited.contains(node) {
            return transitive_deps.get(node).cloned().unwrap_or_default();
        }

        visited.insert(node);

        let mut deps = parsed.jobs.get(node)
            .map(|job| {
                job.inputs
                    .iter()
                    .chain(job.deps.iter())
                    .cloned()
                    .collect::<Vec::<_>>()
            }).unwrap_or_default();

        let mut transitive = Vec::new();
        for dep in &deps {
            transitive.extend(collect_deps(dep, parsed, visited, graph, transitive_deps));
        }

        deps.extend(transitive);
        deps.dedup();

        graph.insert(node, deps.iter().cloned().collect());
        transitive_deps.insert(node, deps.clone());

        deps
    }

    let mut graph = StrHashMap::default();
    graph.reserve(parsed.jobs.len());

    for target in parsed.jobs.keys() {
        collect_deps(target, parsed, visited, &mut graph, transitive_deps);
    } graph
}

#[cfg(debug_assertions)]
fn is_dag(graph: &Graph) -> bool {
    let n = graph.len();
    let mut stack = StrHashSet::default(); stack.reserve(n);
    let mut visited = StrHashSet::default(); visited.reserve(n);

    fn dfs<'a>(node: &'a str, graph: &Graph<'a>, visited: &mut StrHashSet<'a>, stack: &mut StrHashSet<'a>) -> bool {
        if stack.contains(node) { return false }
        if visited.contains(node) { return true }

        stack.insert(node);
        visited.insert(node);

        if let Some(dependencies) = graph.get(node) {
            for dep in dependencies {
                if !dfs(dep, graph, visited, stack) {
                    return false
                }
            }
        }

        stack.remove(node);
        true
    }

    for node in graph.keys() {
        if !dfs(node, graph, &mut visited, &mut stack) {
            return false
        }
    }

    true
}

struct BuildOrder<'a>(Vec::<&'a str>);

impl Display for BuildOrder<'_> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0[0])?;
        for b in self.0[1..].iter() { write!(f, " -> {b}")? }
        Ok(())
    }
}

fn topological_sort<'a>(graph: &Graph<'a>) -> BuildOrder<'a> {
    let n = graph.len();
    let mut ret = Vec::with_capacity(n);
    let mut visited = StrHashSet::default(); visited.reserve(n);

    fn dfs<'a>(node: &'a str, graph: &Graph<'a>, visited: &mut StrHashSet<'a>, ret: &mut Vec::<&'a str>) {
        if visited.contains(node) { return }

        visited.insert(node);

        graph.get(node).map(|deps| {
            deps.iter().for_each(|dep| dfs(dep, graph, visited, ret));
        });

        ret.push(node)
    }

    for node in graph.keys() {
        dfs(node, graph, &mut visited, &mut ret)
    }

    BuildOrder(ret)
}

#[derive(Copy, Clone, PartialEq, PartialOrd)]
struct Metadata {
    mtime: SystemTime,
}

struct MetadataCache<'a> {
    files: DashMap::<&'a str, Metadata>,
}

impl<'a> MetadataCache<'a> {
    #[inline]
    fn new(files_count: usize) -> Self {
        Self {
            files: DashMap::with_capacity(files_count),
        }
    }

    fn needs_rebuild(&mut self, job: &Job<'a>, transitive_deps: &StrHashMap::<Vec::<&'a str>>) -> bool {
        #[inline]
        fn mtime<'a>(f: &'a str, cache: &MetadataCache<'a>) -> std::io::Result<Metadata> {
            if let Some(mtime) = cache.files.get(f) {
                Ok(*mtime)
            } else {
                let p: &Path = f.as_ref();
                let metadata = p.metadata()?;
                let mtime = metadata.modified()?;
                let m = Metadata { mtime };
                cache.files.insert(f, m);
                Ok(m)
            }
        }

        let mtimes = transitive_deps.get(job.target)
            .unwrap()
            .par_iter()
            .filter_map(|dep| mtime(dep, self).ok())
            .collect::<Vec::<_>>();

        let Ok(target_mtime) = mtime(job.target, self) else {
            return true
        };

        mtimes.into_par_iter().any(|src_mtime| src_mtime > target_mtime)
    }
}

fn resolve_and_build<'a>(
    job: &Job<'a>,
    parsed: &Parsed<'a>,
    built: &mut StrHashSet<'a>,
    commands: &mut Vec::<String>,
    metadata_cache: &mut MetadataCache<'a>,
    transistive_deps: &StrHashMap::<Vec::<&'a str>>
) {
    if built.contains(job.target) || !metadata_cache.needs_rebuild(job, transistive_deps) {
        println!("{target} is already built", target = job.target);
        return
    }

    for input in job.inputs.iter() {
        if let Some(dep_job) = parsed.jobs.get(input) {
            resolve_and_build(dep_job, parsed, built, commands, metadata_cache, transistive_deps)
        } else if !built.contains(input) {
            println!("dependency {input} is assumed to exist")
        }
    }

    if let Some(rule) = parsed.rules.get(job.rule) {
        let command = rule.template.compile(job, parsed);
        built.insert(job.target);
        commands.push(command)
    } else {
        eprintln!("no rule found for job: {target}", target = job.target)
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

    #[cfg(debug_assertions)]
    if !is_dag(&graph) {
        eprintln!("[FATAL] dependency graph contains cycles");
        return ExitCode::FAILURE
    }

    let build_order = topological_sort(&graph);
    println!("build order:\n{build_order}");

    let BuildOrder(build_order) = build_order;
    let mut built = StrHashSet::default(); built.reserve(n);
    let mut commands = Vec::with_capacity(n);
    let mut metadata_cache = MetadataCache::new(n);

    for target in build_order {
        if let Some(job) = parsed.jobs.get(target) {
            resolve_and_build(job, &parsed, &mut built, &mut commands, &mut metadata_cache, &transitive_deps)
        }
    }

    for command in commands.iter() {
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
    }

    ExitCode::SUCCESS
}
