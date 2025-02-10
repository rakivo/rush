use crate::loc::Loc;
use crate::consts::syntax::*;
use crate::template::Template;
use crate::consts::PHONY_TARGETS;
use crate::types::{StrHashMap, StrHashSet};

use std::mem;
use std::sync::Arc;
use std::fs::{self, File};

use memmap2::Mmap;
use bumpalo::Bump;
#[cfg(feature = "dbg")]
use tramer::tramer;

#[inline]
pub fn read_file(path: &str) -> Option::<Mmap> {
    if fs::read_dir(".")
        .expect("could not read cwd")
        .filter_map(|res| res.map(|e| e.path()).ok())
        .find(|_path| {
            _path.file_name()
                .map_or(false, |name| name.to_string_lossy() == path)
        }).is_some()
    {
        let file = File::open(path).ok()?;
        Some(unsafe { Mmap::map(&file) }.ok()?)
    } else {
        None
    }
}

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Rule<'a> {
    pub command: Template<'a>,
    pub depfile: Option::<Template<'a>>,
    pub description: Option::<Template<'a>>,
}

impl<'a> Rule<'a> {
    #[inline]
    #[cfg_attr(feature = "dbg", track_caller)]
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

pub type Aliases = Vec::<String>;
pub type DefaultTarget = Option::<String>;
pub type DefaultJob<'a> = Option::<&'a comp::Job<'a>>;
pub type Shadows<'a> = Option::<Arc::<StrHashMap::<'a, &'a str>>>;

pub mod prep {
    use super::*;

    #[repr(packed)]
    #[cfg_attr(feature = "dbg", derive(Debug))]
    pub struct Target<'a> {
        pub t: &'a str,
        pub loc: Loc
    }

    pub type DefaultTarget<'a> = Option::<Target<'a>>;

    #[cfg_attr(feature = "dbg", derive(Debug))]
    pub enum Phony<'a> {
        Phony {
            command: Option::<Template<'a>>,
            aliases: Vec::<&'a str>,
            aliases_templates: Vec::<Template<'a>>,
        },
        NotPhony {
            rule: Option::<&'a str>,

            inputs: Vec::<&'a str>,
            inputs_str: &'a str,
            inputs_templates: Vec::<Template<'a>>,

            deps: Vec::<&'a str>,
            deps_templates: Vec::<Template<'a>>
        }
    }

    #[cfg_attr(feature = "dbg", derive(Debug))]
    pub struct Job<'a> {
        pub loc: Loc,
        pub phony: Phony<'a>,
        pub shadows: Shadows<'a>,

        pub target: &'a str,
        pub target_template: Template<'a>,
    }

    impl<'a> Job<'a> {
        #[inline]
        #[cfg_attr(feature = "dbg", track_caller)]
        pub fn inputs_str(&self, panic: bool) -> &'a str {
            match self.phony {
                Phony::Phony { .. } => if panic {
                    report_panic!(self.loc, "$in is not supported in phony jobs yet")
                } else {
                    ""
                },
                Phony::NotPhony { inputs_str, .. } => inputs_str,
            }
        }

        #[inline]
        pub(super) fn into_phony(&mut self, command_: Option::<Template<'a>>) -> Phony<'a> {
            match &mut self.phony {
                Phony::NotPhony { rule, inputs, deps, .. } => {
                    let mut aliases = Vec::with_capacity(inputs.len() + deps.len() + 1);
                    if let Some(rule) = rule { aliases.push(*rule) }
                    aliases.extend(mem::take(inputs).into_iter());
                    aliases.extend(mem::take(deps).into_iter());
                    Phony::Phony {
                        command: command_,
                        aliases_templates: aliases.iter().map(|a| Template::new(a, self.loc)).collect(),
                        aliases,
                    }
                },
                Phony::Phony { aliases, aliases_templates, command } => {
                    Phony::Phony {
                        command: command_.or(mem::take(command)),
                        aliases: mem::take(aliases),
                        aliases_templates: mem::take(aliases_templates)
                    }
                }
            }
        }
    }

    #[repr(transparent)]
    #[cfg_attr(feature = "dbg", derive(Debug))]
    pub struct Def<'a>(pub Template<'a>);

    #[repr(transparent)]
    #[cfg_attr(feature = "dbg", derive(Debug))]
    pub struct Defs<'a>(pub StrHashMap::<'a, Def<'a>>);

    impl<'a> Defs<'a> {
        #[inline]
        pub fn compile(&self) -> comp::Defs<'a> {
            let mut compiled_defs = comp::Defs::new();
            let mut compiling = StrHashSet::with_capacity(128);
            for (name, def) in self.0.iter() {
                Template::compile_def_recursive(name, def, self, &mut compiling, &mut compiled_defs)
            } compiled_defs
        }
    }
}

pub mod comp {
    use super::*;

    #[cfg_attr(feature = "dbg", derive(Debug))]
    pub enum Phony<'a> {
        Phony {
            command: Option::<String>,
            aliases: Aliases
        },
        NotPhony {
            rule: Option::<&'a str>,
            inputs: Vec::<&'a str>,
            inputs_str: &'a str,
            deps: Vec::<&'a str>,
        }
    }

    #[cfg_attr(feature = "dbg", derive(Debug))]
    pub struct Job<'a> {
        pub loc: Loc,
        pub phony: Phony<'a>,
        pub target: &'a str,
        pub shadows: Shadows<'a>,
    }

    impl<'a> Job<'a> {
        #[inline(always)]
        pub fn aliases(&self) -> Option::<&Aliases> {
            if let Phony::Phony { aliases, .. } = &self.phony { Some(aliases) }
            else { None }
        }

        #[inline]
        #[cfg_attr(feature = "dbg", track_caller)]
        pub fn inputs_str(&self, panic: bool) -> &'a str {
            match self.phony {
                Phony::Phony { .. } => if panic {
                    report_panic!(self.loc, "$in is not supported in phony jobs yet")
                } else {
                    ""
                },
                Phony::NotPhony { inputs_str, .. } => inputs_str,
            }
        }

        #[inline]
        pub fn rule(&self) -> Option::<&'a str> {
            match self.phony {
                Phony::Phony { .. } => None,
                Phony::NotPhony { rule, .. } => rule,
            }
        }
    }

    #[repr(transparent)]
    #[cfg_attr(feature = "dbg", derive(Debug))]
    pub struct Def(pub String);

    pub type Defs<'a> = StrHashMap::<'a, Def>;
}

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Parsed<'a> {
    defs: prep::Defs<'a>,
    phonys: StrHashSet<'a>,
    default_target: prep::DefaultTarget<'a>,
    pub jobs: StrHashMap::<'a, prep::Job<'a>>,
    rules: StrHashMap::<'a, Rule<'a>>,
}

impl<'a> Parsed<'a> {
    #[inline(always)]
    fn job_mut(&mut self, target: &str) -> &mut prep::Job<'a> {
        unsafe { self.jobs.get_mut(target).unwrap_unchecked() }
    }

    #[inline(always)]
    fn rule_mut(&mut self, name: &str) -> &mut Rule<'a> {
        unsafe { self.rules.get_mut(name).unwrap_unchecked() }
    }

    #[inline]
    pub fn guess_preallocation(&self) -> usize {
        self.jobs.values().map(|j| {
            j.target_template.guess_compiled_size() + match &j.phony {
                prep::Phony::Phony { .. } => 0,
                prep::Phony::NotPhony { inputs_templates, deps_templates, .. } => {
                    inputs_templates.iter()
                        .chain(deps_templates.iter())
                        .map(|t| t.guess_compiled_size())
                        .sum::<usize>()
                }
            }
        }).sum()
    }

    #[cfg_attr(feature = "dbg", tramer("nanos"))]
    pub fn compile(self, arena: &'a Bump) -> Compiled<'a> {
        let Parsed { defs, jobs, rules, phonys, default_target } = self;

        let defs = defs.compile();
        let jobs = jobs.values().filter_map(|job| {
            if let prep::Phony::NotPhony { rule, .. } = &job.phony {
                if let Some(ref rule) = rule {
                    if !rules.contains_key(rule) {
                        report_panic!(job.loc, "undefined rule: {rule}")
                    }
                }
            }

            let target = match job.target_template.compile_prep(&job, &defs) {
                Ok(ok) => arena.alloc_str(&ok) as &_,
                Err(e) => panic!("{e}\n")
            };

            let job = match &job.phony {
                prep::Phony::Phony { command, aliases_templates, aliases } => {
                    if  !phonys.contains(job.target) {
                        report_panic!{
                            job.loc,
                            "mark {target} as phony for it to have a command\n",
                            target = job.target
                        }
                    }

                    let command = command.as_ref().map(|c| match c.compile_prep(job, &defs) {
                        Ok(ok) => ok,
                        Err(e) => panic!("{e}\n")
                    });

                    let aliases = aliases_templates.iter()
                        .zip(aliases.iter())
                        .map(|(template, ..)| {
                            match template.compile_prep(&job, &defs) {
                                Ok(ok) => ok,
                                Err(e) => panic!("{e}\n")
                            }
                        }).collect::<Vec::<_>>();

                    comp::Job {
                        target,
                        loc: job.loc,
                        shadows: job.shadows.as_ref().map(Arc::clone),
                        phony: comp::Phony::Phony { command, aliases }
                    }
                }
                prep::Phony::NotPhony {
                    rule,
                    inputs,
                    inputs_str,
                    inputs_templates,
                    deps,
                    deps_templates
                } => {
                    let mut inputs_str = String::with_capacity(inputs_str.len() + 10);
                    let inputs = inputs_templates.iter()
                        .zip(inputs.iter())
                        .map(|(template, ..)| {
                            match template.compile_prep(&job, &defs) {
                                Ok(ok) => {
                                    inputs_str.push_str(&ok);
                                    inputs_str.push(' ');
                                    arena.alloc_str(&ok) as &_
                                },
                                Err(e) => panic!("{e}\n")
                            }
                        }).collect::<Vec::<_>>();

                    if !inputs_str.is_empty() { _ = inputs_str.pop() }
                    let inputs_str = arena.alloc_str(&inputs_str) as &_;

                    let deps = deps_templates.iter()
                        .zip(deps.iter())
                        .map(|(template, ..)| {
                            match template.compile_prep(&job, &defs) {
                                Ok(ok) => arena.alloc_str(&ok) as &_,
                                Err(e) => panic!("{e}\n")
                            }
                        }).collect::<Vec::<_>>();

                    comp::Job {
                        target,
                        loc: job.loc,
                        shadows: job.shadows.as_ref().map(Arc::clone),
                        phony: comp::Phony::NotPhony {
                            deps,
                            inputs,
                            inputs_str,
                            rule: *rule,
                        }
                    }
                }
            };

            Some((target, job))
        }).collect::<StrHashMap::<_>>();

        let default_target = default_target.map(|dt| {
            Template::new(dt.t, dt.loc).compile_def(&defs)
        });

        Compiled {jobs, rules, defs, default_target}
    }
}

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Compiled<'a> {
    pub defs: comp::Defs<'a>,
    pub default_target: DefaultTarget,
    pub rules: StrHashMap::<'a, Rule<'a>>,
    pub jobs: StrHashMap::<'a, comp::Job<'a>>,
}

impl Compiled<'_> {
    #[inline]
    pub fn pretty_print_targets(&self) -> String {
        let mut buf = String::with_capacity(self.jobs.len() * 24);

        let mut targets = self.jobs.iter().collect::<Vec::<_>>();
        targets.sort_unstable_by(|(_, ja), (_, jb)| ja.loc.0.cmp(&jb.loc.0));

        targets.first().map(|(t, _)| buf.push_str(t));
        targets.iter().skip(1).for_each(|(t, _)| {
            buf.push_str(", ");
            buf.push_str(t);
        });
        buf
    }
}

#[repr(packed)]
#[derive(Copy, Clone)]
#[cfg_attr(feature = "dbg", derive(Debug))]
struct DepfilePath<'a> {
    loc: Loc,
    path: &'a str
}

#[repr(packed)]
#[derive(Copy, Clone)]
#[cfg_attr(feature = "dbg", derive(Debug))]
struct Description<'a> {
    loc: Loc,
    description: &'a str
}

#[cfg_attr(feature = "dbg", derive(Debug))]
enum Context<'a> {
    Global,
    Job {
        target: &'a str,
        shadows: Option::<StrHashMap::<'a, &'a str>>,
    },
    Rule {
        name: &'a str,

        already_inserted: bool,

        depfile: Option::<DepfilePath<'a>>,
        description: Option::<Description<'a>>,
    }
}

type EscapedIndexes = Vec::<(usize, usize)>;

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Parser<'a> {
    cursor: usize,
    context: Context<'a>,
    pub parsed: Parsed<'a>,
}

impl<'a> Parser<'a> {
    pub const RUSH_FILE_PATH: &'static str = "build.rush";

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

    fn parse_line(&mut self, not_trimmed: &'a str, trimmed: &'a str) {
        if matches!(&self.context, Context::Job { .. } | Context::Rule { .. }) {
            if (trimmed.is_empty() || not_trimmed.as_bytes().first().map_or(false, |c| c.is_ascii_alphanumeric()))
 || not_trimmed.find(|c: char| c.is_ascii_whitespace()).map(|first_space| {
                    (first_space, &not_trimmed[..first_space])
                }).map_or(false, |(_, first_token)| matches!(first_token, BUILD | PHONY | RULE))
            {
                self.finish_job();
                self.context = Context::Global
            }
        }

        if trimmed.is_empty() { return }

        let line = trimmed;
        let first = line.find(|c: char| c.is_ascii_whitespace()).map(|first_space| {
            (first_space, &line[..first_space])
        });

        let Some((first_space, first_token)) = first else {
            report_panic!(Loc(self.cursor), "undefined token: {line}")
        };

        let Some(second_space) = line[first_space..].find(|c: char| !c.is_ascii_whitespace())
            .map(|p| p + first_space) else {
                return
            };
        
        let parse_shadow = || -> &'a str {
            let check_start = first_space;
            let check_end = (second_space + 1 + 1).min(line.len());
            if !line[check_start..check_end].contains('=') {
                report_panic!(Loc(self.cursor), "expected `=` in variable definition")
            }
            line[second_space + 1..].trim()
        };

        let parse_def = || -> prep::Def<'a> {
            prep::Def(Template::new(parse_shadow(), Loc(self.cursor)))
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
                        job.phony = job.into_phony(Some(command));
                    },
                    name => {
                        let def = parse_shadow();
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
            Context::Rule { name, depfile, already_inserted, description } => {
                match first_token {
                    COMMAND => {
                        let command_start = second_space + 1 + 1;
                        let command = if command_start < line.len() - 1 {
                            line[command_start..].trim()
                        } else {
                            ""
                        };
                        let command_loc = self.cursor;
                        let rule = Rule::new(
                            command,
                            Loc(command_loc),
                            *description,
                            *depfile,
                        );
                        self.parsed.rules.insert(name, rule);
                        self.context = Context::Rule {
                            name,
                            already_inserted: true,
                            description: *description,
                            depfile: *depfile,
                        }
                    },
                    DEPFILE => {
                        let path = parse_shadow();
                        let loc = Loc(self.cursor);
                        let depfile = DepfilePath {loc, path};
                        if *already_inserted {
                            let rule = self.parsed.rule_mut(name);
                            let depfile = Template::new(path, loc);
                            rule.depfile = Some(depfile)
                        } else {
                            self.context = Context::Rule {
                                name,
                                already_inserted: true,
                                description: *description,
                                depfile: Some(depfile),
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
                                depfile: *depfile,
                                already_inserted: *already_inserted,
                            }
                        }
                    },
                    _ => if line.chars().next() != Some(COMMENT) {
                        report_panic!{
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
                        if phony.as_bytes().last() == Some(&b':') {
                            report_panic!{
                                Loc(self.cursor),
                                "you can define phony job following way:\n  phony {target}\n  build {target}:\n    ...\n",
                                target = &phony[..(phony.len() - 1).max(1)]
                            }
                        }
                        self.parsed.phonys.insert(phony);
                        if let Some(job) = self.parsed.jobs.get_mut(phony) {
                            job.phony = job.into_phony(None)
                        }
                    },
                    DEFAULT => {
                        self.parsed.default_target = Some(prep::Target {
                            t: line[first_space..].trim(),
                            loc: Loc(self.cursor)
                        });
                    },
                    RULE => {
                        self.context = Context::Rule {
                            depfile: None,
                            description: None,
                            name: line[second_space..].trim_end(),
                            already_inserted: false,
                        }
                    },
                    BUILD => {
                        let Some(colon_idx) = line.find(':') else {
                            report_panic!(Loc(self.cursor), "expected colon after build target")
                        };
                        let post_colon = line[colon_idx + 1..].trim();
                        let or_idx = post_colon.find('|');

                        let loc = Loc(self.cursor);
                        let target = line[first_space..colon_idx].trim();
                        let target_template = Template::new(target, loc);

                        let job = if self.parsed.phonys.contains(target) {
                            let aliases_str = if let Some(or_idx) = or_idx {
                                post_colon[..or_idx].trim_end()
                            } else {
                                post_colon.trim_end()
                            };

                            let aliases_tokens = aliases_str.split_ascii_whitespace();
                            let aliases = aliases_tokens.collect::<Vec::<_>>();
                            let aliases_templates = aliases.iter().map(|alias| Template::new(alias, loc)).collect();

                            prep::Job {
                                loc,
                                target,
                                target_template,
                                phony: prep::Phony::Phony {
                                    command: None,
                                    aliases,
                                    aliases_templates
                                },
                                shadows: None,
                            }
                        } else {
                            let (inputs_str, deps) = if let Some(or_idx) = or_idx {
                                let inputs_str = post_colon[..or_idx].trim_end();
                                let deps = post_colon[or_idx + 1..].split_ascii_whitespace().collect();
                                (inputs_str, deps)
                            } else {
                                (post_colon, Vec::new())
                            };

                            let mut input_tokens = inputs_str.split_ascii_whitespace();
                            let rule = input_tokens.next();

                            let (inputs, inputs_str, inputs_templates) = if let Some(rule_len) = rule.map(|r| r.len()) {
                                if rule_len + 1 >= inputs_str.len() {
                                    (Vec::new(), "", Vec::new())
                                } else {
                                    let inputs = input_tokens.collect::<Vec::<_>>();
                                    let inputs_str = inputs_str[rule_len + 1..].trim_end();
                                    let inputs_templates = inputs.iter().map(|input| Template::new(input, loc)).collect();
                                    (inputs, inputs_str, inputs_templates)
                                }
                            } else {
                                (Vec::new(), "", Vec::new())
                            };

                            let deps_templates = deps.iter().map(|input| Template::new(input, loc)).collect();
                            prep::Job {
                                loc,
                                target,
                                target_template,
                                phony: prep::Phony::NotPhony {
                                    rule,
                                    inputs,
                                    inputs_str,
                                    inputs_templates,
                                    deps,
                                    deps_templates
                                },
                                shadows: None,
                            }
                        };

                        self.parsed.jobs.insert(target, job);
                        self.context = Context::Job {target, shadows: None}
                    },
                    _ => {
                        let name = first_token;
                        let def = parse_def();
                        self.parsed.defs.0.insert(name, def);
                    }
                };
            },
        };
    }

    #[cfg_attr(feature = "dbg", tramer("nanos"))]
    pub fn handle_newline_escapes(input: &str) -> (String, EscapedIndexes) {
        let mut ret = String::with_capacity(input.len());
        let mut indexes = Vec::with_capacity(32);

        let mut lines = input.lines().enumerate().peekable();
        while let Some((index, line)) = lines.next() {
            let mut escaped_lines = 0;
            let mut curr_line = line.trim_end();
            while curr_line.as_bytes().last() == Some(&(LINE_ESCAPE as u8)) {
                escaped_lines += 1;
                curr_line = curr_line[..curr_line.len() - 1].trim_end();

                if let Some((.., next_line)) = lines.peek() {
                    let next_trimmed = next_line.trim_start();
                    ret.push_str(curr_line);
                    ret.push(' ');
                    curr_line = next_trimmed;
                    lines.next();
                } else {
                    break
                }
            }

            if escaped_lines > 0 {
                indexes.push((index, escaped_lines))
            }

            ret.push_str(curr_line);
            ret.push('\n');
        } (ret, indexes)
    }

    #[cfg_attr(feature = "dbg", tramer("millis"))]
    pub fn parse(escaped: &'a str, escaped_indexes: &EscapedIndexes) -> Parsed<'a> {
        let mut parser = Self {
            cursor: 0,
            parsed: Parsed {
                defs: prep::Defs(StrHashMap::with_capacity(32)),
                jobs: StrHashMap::with_capacity(32),
                rules: StrHashMap::with_capacity(32),
                phonys: {
                    let mut phonys = StrHashSet::from_iter(PHONY_TARGETS.iter().cloned());
                    _ = phonys.try_reserve(32);
                    phonys
                },
                default_target: None,
            },
            context: Context::Global
        };

        let mut escaped_index = 0;
        for line in escaped.lines() {
            parser.cursor += 1;
            if escaped_index < escaped_indexes.len() && escaped_indexes[escaped_index].0 <= parser.cursor {
                parser.cursor += escaped_indexes[escaped_index].1;
                escaped_index += 1
            }
            let trimmed = line.trim_start();
            if trimmed.as_bytes().first() == Some(&(COMMENT as u8)) { continue }
            parser.parse_line(line, trimmed)
        }

        parser.finish_job();
        parser.parsed
    }
}
