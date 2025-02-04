use crate::loc::Loc;
use crate::template::Template;
use crate::types::{StrHashMap, StrHashSet};
use crate::consts::syntax::{RULE, BUILD, PHONY, DEPFILE, COMMAND, COMMENT, DESCRIPTION, LINE_ESCAPE};

use std::sync::Arc;
use std::str::Lines;
use std::fs::{self, File};

use memmap2::Mmap;

pub const RUSH_FILE_NAME: &str = "build.rush";

#[inline]
pub fn read_rush() -> Option::<Mmap> {
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

#[derive(Default)]
#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Rule<'a> {
    pub command: Template<'a>,
    pub depfile: Option::<Template<'a>>,
    pub description: Option::<Template<'a>>,
}

impl<'a> Rule<'a> {
    #[inline]
    fn new(
        command: &'a str,
        command_loc: Loc,
        description: Option::<Description<'a>>,
        depfile_path: Option::<DepfilePath<'a>>
    ) -> Self {
        Self {
            command: Template::new(command, command_loc),
            depfile: depfile_path.map(|dp| Template::new(dp.path, dp.loc)),
            description: description.map(|d| Template::new(d.description, d.loc)),
        }
    }
}

pub enum Phony<'a> {
    Phony {
        command: String,
    },
    NotPhony {
        inputs: Vec::<&'a str>,
        inputs_str: &'a str,
        deps: Vec::<&'a str>,
    }
}

pub enum PreprocessedPhony<'a> {
    Phony {
        command: Template<'a>,
    },
    NotPhony {
        inputs: Vec::<&'a str>,
        inputs_str: &'a str,
        inputs_templates: Vec::<Template<'a>>,

        deps: Vec::<&'a str>,
        deps_templates: Vec::<Template<'a>>
    }
}

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct PreprocessedJob<'a> {
    pub loc: Loc,

    pub rule: Option::<&'a str>,

    pub shadows: Option::<Arc::<Defs<'a>>>,

    pub target: &'a str,
    pub target_template: Template<'a>,

    pub phony: PreprocessedPhony<'a>
}

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Job<'a> {
    pub loc: Loc,

    pub rule: Option::<&'a str>,

    pub target: &'a str,

    pub shadows: Option::<Arc::<Defs<'a>>>,

    pub phony: Phony<'a>
}

#[repr(transparent)]
#[derive(Clone, Default)]
#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Def<'a> { pub value: &'a str }

pub type Defs<'a> = StrHashMap::<'a, Def<'a>>;

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Parsed<'a> {
    defs: Defs<'a>,
    phonys: StrHashSet<'a>,
    jobs: StrHashMap::<'a, PreprocessedJob<'a>>,
    rules: StrHashMap::<'a, Rule<'a>>,
}

impl<'a> Parsed<'a> {
    #[inline(always)]
    fn job_mut(&mut self, target: &str) -> &mut PreprocessedJob<'a> {
        unsafe { self.jobs.get_mut(target).unwrap_unchecked() }
    }

    #[inline(always)]
    fn rule_mut(&mut self, name: &str) -> &mut Rule<'a> {
        unsafe { self.rules.get_mut(name).unwrap_unchecked() }
    }

    pub fn into_processed(self) -> Processed<'a> {
        let Parsed { defs, jobs, rules, phonys } = self;
        let jobs = jobs.iter().filter_map(|(.., job)| {
            if job.rule.as_ref().map_or(false, |rule| !rules.contains_key(rule)) { return None }

            let target = match job.target_template.compile(&job, &defs) {
                Ok(ok) => ok.leak() as &_,
                Err(e) => report!(job.loc, "{e}")
            };

            let job = match &job.phony {
                PreprocessedPhony::Phony { command } => {
                    if !phonys.contains(job.target) {
                        report!{
                            job.loc,
                            "mark {target} as phony for it to have a command",
                            target = job.target
                        }
                    }

                    let command = match command.compile(job, &defs) {
                        Ok(ok) => ok,
                        Err(e) => report!(job.loc, "{e}")
                    };

                    Job {
                        target,
                        loc: job.loc,
                        rule: job.rule,
                        shadows: job.shadows.as_ref().map(Arc::clone),
                        phony: Phony::Phony { command }
                    }
                }
                PreprocessedPhony::NotPhony {
                    inputs,
                    inputs_templates,
                    inputs_str: inputs_wo_rule_str,
                    deps,
                    deps_templates
                } => {
                    let mut inputs_str = String::with_capacity(inputs_wo_rule_str.len() + 10);
                    let inputs = inputs_templates.iter()
                        .zip(inputs.iter())
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

                    let deps = deps_templates.iter()
                        .zip(deps.iter())
                        .map(|(template, ..)| {
                            match template.compile(&job, &defs) {
                                Ok(ok) => ok.leak() as &_,
                                Err(e) => report!(job.loc, "{e}")
                            }
                        }).collect::<Vec::<_>>();

                    Job {
                        target,
                        loc: job.loc,
                        rule: job.rule,
                        shadows: job.shadows.as_ref().map(Arc::clone),
                        phony: Phony::NotPhony {
                            deps,
                            inputs,
                            inputs_str
                        }
                    }
                }
            };

            Some((target, job))
        }).collect::<StrHashMap::<_>>();

        Processed {jobs, rules, defs}
    }
}

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Processed<'a> {
    pub defs: Defs<'a>,
    pub rules: StrHashMap::<'a, Rule<'a>>,
    pub jobs: StrHashMap::<'a, Job<'a>>,
}

#[repr(packed)]
#[derive(Copy, Clone)]
#[cfg_attr(feature = "dbg", derive(Debug))]
struct DepfilePath<'a> { loc: Loc, path: &'a str }

#[repr(packed)]
#[derive(Copy, Clone)]
#[cfg_attr(feature = "dbg", derive(Debug))]
struct Description<'a> { loc: Loc, description: &'a str }

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

        depfile_path: Option::<DepfilePath<'a>>,
        description: Option::<Description<'a>>,
    }
}

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Parser<'a> {
    cursor: usize,
    parsed: Parsed<'a>,
    context: Context<'a>
}

impl<'a> Parser<'a> {
    fn finish_job(&mut self) {
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
                self.finish_job();
                self.context = Context::Global;
                return
            }

            if matches!(first, Some((.., BUILD | PHONY | RULE))) {
                self.finish_job();
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

        // TODO: we should move context here and this match should return another updated context
        match &mut self.context {
            Context::Job { target, shadows, .. } => {
                match first_token {
                    COMMAND => {
                        let command_ = line[second_space + 1 + 1..].trim();
                        let command_loc = Loc(self.cursor);
                        let command = Template::new(command_, command_loc);
                        let job = self.parsed.job_mut(target);
                        job.phony = PreprocessedPhony::Phony { command };
                    },
                    name => {
                        let def = parse_def();
                        match shadows.as_mut() {
                            Some(shadows) => { shadows.insert(name, def); },
                            None => {
                                let mut _shadows = StrHashMap::with_capacity(4);
                                _shadows.insert(name, def);
                                shadows.replace(_shadows);
                            },
                        }
                    }
                }
            },
            Context::Rule { name, depfile_path, already_inserted, description } => {
                match first_token {
                    RULE => {
                        let command = line[second_space + 1 + 1..].trim();
                        let command_loc = self.cursor;
                        let rule = Rule::new(
                            command,
                            Loc(command_loc),
                            *description,
                            *depfile_path,
                        );
                        self.parsed.rules.insert(name, rule);
                        self.context = Context::Rule {
                            name,
                            already_inserted: true,
                            description: *description,
                            depfile_path: *depfile_path,
                        }
                    },
                    DEPFILE => {
                        let Def {value: path} = parse_def();
                        let loc = Loc(self.cursor);
                        let depfile_path = DepfilePath {path, loc};
                        if *already_inserted {
                            let rule = self.parsed.rule_mut(name);
                            let depfile = Template::new(depfile_path.path, depfile_path.loc);
                            rule.depfile = Some(depfile)
                        } else {
                            self.context = Context::Rule {
                                name,
                                already_inserted: true,
                                description: *description,
                                depfile_path: Some(depfile_path),
                            }
                        }
                    },
                    DESCRIPTION => {
                        let loc = Loc(self.cursor);
                        let description = line[second_space + 1 + 1..].trim();
                        let description = Description { loc, description };
                        if *already_inserted {
                            let rule = self.parsed.rule_mut(name);
                            rule.description = Some(Template::new(description.description, description.loc));
                        } else {
                            self.context = Context::Rule {
                                name,
                                description: Some(description),
                                depfile_path: *depfile_path,
                                already_inserted: *already_inserted,
                            }
                        }
                    },
                    _ => if line.chars().next() != Some(COMMENT) {
                        report!{
                            Loc(self.cursor),
                            "undefined property: `{first_token}`, existing properties are: `command`, `description`"
                        }
                    }
                }
            },
            Context::Global => {
                match first_token {
                    PHONY => {
                        let phony = line[first_space..].trim();
                        self.parsed.phonys.insert(phony);
                    },
                    RULE => {
                        self.context = Context::Rule {
                            description: None,
                            depfile_path: None,
                            name: line[second_space..].trim_end(),
                            already_inserted: false,
                        }
                    },
                    BUILD => {
                        let Some(colon_idx) = line.find(':') else {
                            report!(Loc(self.cursor), "expected colon after build target")
                        };
                        let post_colon = line[colon_idx + 1..].trim();
                        let or_idx = post_colon.find('|');

                        let (inputs_str, deps) = if let Some(or_idx) = or_idx {
                            let inputs_str = post_colon[..or_idx].trim_end();
                            let deps = post_colon[or_idx + 1..].split_ascii_whitespace().collect();
                            (inputs_str, deps)
                        } else {
                            (post_colon, Vec::new())
                        };

                        let mut input_tokens = inputs_str.split_ascii_whitespace();
                        let rule = input_tokens.next();

                        let target = line[first_space..colon_idx].trim();

                        let loc = Loc(self.cursor);
                        let (inputs, inputs_templates, inputs_wo_rule_str) = if let Some(rule_len) = rule.map(|r| r.len()) {
                            let inputs = input_tokens.collect::<Vec::<_>>();
                            let inputs_templates = inputs.iter().map(|input| Template::new(input, loc)).collect();
                            let inputs_wo_rule_str = inputs_str[rule_len + 1..].trim_end();
                            (inputs, inputs_templates, inputs_wo_rule_str)
                        } else {
                            (Vec::new(), Vec::new(), "")
                        };

                        let target_template = Template::new(target, loc);

                        let deps_templates = deps.iter().map(|input| Template::new(input, loc)).collect();
                        let job = PreprocessedJob {
                            loc,
                            rule,
                            target,
                            target_template,
                            phony: PreprocessedPhony::NotPhony {
                                inputs,
                                inputs_templates,
                                inputs_str: inputs_wo_rule_str,
                                deps,
                                deps_templates
                            },
                            shadows: None,
                        };

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
        while full_line.as_bytes().last() == Some(&(LINE_ESCAPE as _)) {
            let trimmed = full_line[..full_line.len() - 1].trim_end();
            let Some(next_line) = lines.next() else { break };
            self.cursor += 1;
            let next_trimmed = next_line.trim();
            full_line = format!("{trimmed} {next_trimmed}").leak()
        } full_line.trim_start()
    }

    #[inline]
    pub fn parse(content: &'a str) -> Parsed<'a> {
        let mut parser = Self {
            cursor: 0,
            parsed: Parsed {
                defs: Defs::with_capacity(32),
                jobs: StrHashMap::with_capacity(32),
                rules: StrHashMap::with_capacity(32),
                phonys: StrHashSet::with_capacity(32),
            },
            context: Context::default()
        };
        let mut lines = content.lines();
        while let Some(line) = lines.next() {
            parser.cursor += 1;
            if line.as_bytes().first() == Some(&(COMMENT as _)) { continue }
            let line = parser.handle_newline_escape(line, &mut lines);
            parser.parse_line(line)
        } parser.parsed
    }
}
