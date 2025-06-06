use crate::util;
use crate::db::Db;
use crate::loc::Loc;
use crate::flags::Flags;
use crate::command::Command;
use crate::consts::syntax::*;
use crate::template::Template;
use crate::dbg_unwrap::DbgUnwrap;
use crate::ux::did_you_mean_compiled;
use crate::types::{StrHashMap, StrHashSet};
use crate::consts::{CLEAN_TARGET, PHONY_TARGETS, BUILD_DIR_VARIABLE};

use std::mem;
use std::sync::Arc;
use std::borrow::Cow;
use std::fs::{self, File};
use std::hash::{Hash, Hasher};
use std::collections::HashSet;
use std::path::{Path, PathBuf};

use memmap2::Mmap;
use bumpalo::Bump;
#[cfg(feature = "dbg")]
use tramer::tramer;
use fxhash::FxBuildHasher;

#[inline(always)]
pub fn read_file<P>(path: P) -> Option::<Mmap>
where
    P: AsRef::<Path>
{
    let file = File::open(path).ok()?;
    Some(unsafe { Mmap::map(&file) }.ok()?)
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
        command_loc: Loc<'a>,
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

pub type Id = usize;
pub type Aliases = Vec::<String>;
pub type Shadows<'a> = Option::<Arc::<StrHashMap::<'a, &'a str>>>;

pub mod prep {
    use super::*;

    #[repr(packed)]
    #[cfg_attr(feature = "dbg", derive(Debug))]
    pub struct Target<'a> {
        pub loc: Loc<'a>,
        pub target: &'a str
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
    pub struct Edge<'a> {
        pub loc: Loc<'a>,
        pub phony: Phony<'a>,
        pub shadows: Shadows<'a>,

        pub target: &'a str,
        pub target_template: Template<'a>,
    }

    impl<'a> Edge<'a> {
        #[inline]
        #[cfg_attr(feature = "dbg", track_caller)]
        pub fn inputs_str(&self, panic: bool) -> &'a str {
            match self.phony {
                Phony::Phony { .. } => if panic {
                    report_panic!(self.loc, "$in is not supported in phony edges yet")
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
    pub struct Target<'a> {
        pub loc: Loc<'a>,
        pub target: String
    }

    pub type DefaultTarget<'a> = Option::<Target<'a>>;

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
            depfile: Option::<String>,
        }
    }

    #[cfg_attr(feature = "dbg", derive(Debug))]
    pub struct Edge<'a> {
        pub id: Id,
        pub loc: Loc<'a>,
        pub phony: Phony<'a>,
        pub target: &'a str,
        pub shadows: Shadows<'a>,
    }

    impl<'a> Edge<'a> {
        #[inline(always)]
        pub fn depfile(&self) -> Option::<&String> {
            if let Phony::NotPhony { depfile, .. } = &self.phony { depfile.as_ref() }
            else { None }
        }

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
                    report_panic!(self.loc, "$in is not supported in phony edges yet")
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
    pub edges: StrHashMap::<'a, prep::Edge<'a>>,
    rules: StrHashMap::<'a, Rule<'a>>,
}

impl<'a> Parsed<'a> {
    #[inline(always)]
    fn edge_mut(&mut self, target: &str) -> &mut prep::Edge<'a> {
        self.edges.get_mut(target).unwrap_dbg()
    }

    #[inline(always)]
    fn rule_mut(&mut self, name: &str) -> &mut Rule<'a> {
        self.rules.get_mut(name).unwrap_dbg()
    }

    #[cfg_attr(feature = "dbg", tramer("nanos"))]
    pub fn compile(self, arena: &'a Bump) -> Compiled<'a> {
        let Parsed { defs, edges, rules, phonys, default_target } = self;

        let defs = defs.compile();
        let edges = edges.values().enumerate().filter_map(|(id, edge)| {
            let target = match edge.target_template.compile_prep(&edge, &defs) {
                Ok(ok) => arena.alloc_str(&ok) as &_,
                Err(e) => panic!("{e}\n")
            };

            let edge = match &edge.phony {
                prep::Phony::Phony { command, aliases_templates, aliases } => {
                    if edge.target != CLEAN_TARGET && !phonys.contains(edge.target) {
                        report_panic!{
                            edge.loc,
                            "mark {target} as phony for it to have a command",
                            target = edge.target
                        }
                    }

                    let command = command.as_ref().map(|c| match c.compile_prep(edge, &defs) {
                        Ok(ok) => ok,
                        Err(e) => panic!("{e}\n")
                    });

                    let aliases = aliases_templates.iter()
                        .zip(aliases.iter())
                        .map(|(template, ..)| {
                            match template.compile_prep(&edge, &defs) {
                                Ok(ok) => ok,
                                Err(e) => panic!("{e}\n")
                            }
                        }).collect::<Vec::<_>>();

                    comp::Edge {
                        id,
                        target,
                        loc: edge.loc,
                        shadows: edge.shadows.as_ref().map(Arc::clone),
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
                            match template.compile_prep(&edge, &defs) {
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
                            match template.compile_prep(&edge, &defs) {
                                Ok(ok) => arena.alloc_str(&ok) as &_,
                                Err(e) => panic!("{e}\n")
                            }
                        }).collect::<Vec::<_>>();

                    let mut edge = comp::Edge {
                        id,
                        target,
                        loc: edge.loc,
                        shadows: edge.shadows.as_ref().map(Arc::clone),
                        phony: comp::Phony::NotPhony {
                            deps,
                            inputs,
                            inputs_str,
                            rule: *rule,
                            depfile: None
                        }
                    };

                    if let Some(Some(rule)) = edge.rule().map(|rule| rules.get(rule)) {
                        if let Some(ref depfile_template) = rule.depfile {
                            let depfile_path = match depfile_template.compile(&edge, &defs) {
                                Ok(ok) => ok,
                                Err(e) => panic!("{e}\n")
                            };

                            let comp::Phony::NotPhony { depfile, .. } = &mut edge.phony else {
                                unsafe { std::hint::unreachable_unchecked() }
                            };

                            *depfile = Some(depfile_path)
                        }
                    }

                    edge
                }
            };

            Some((target, edge))
        }).collect::<StrHashMap::<_>>();

        edges.values().for_each(|edge| {
            if let comp::Phony::NotPhony { rule, .. } = &edge.phony {
                if let Some(ref rule) = rule {
                    if !rules.contains_key(rule) {
                        let mut msg = report_fmt!(edge.loc, "undefined rule: {rule}");

                        if let Some(compiled) = did_you_mean_compiled(
                            rule,
                            &edges,
                            &rules
                        ) {
                            let msg_ = format!("\nnote: did you mean: {compiled}?");
                            msg.push_str(&msg_)
                        }

                        report_panic!("{msg}")
                    }
                }
            }
        });

        struct EdgeInput<'a> {
            edge: &'a comp::Edge<'a>,
            input: &'a str
        }

        impl Eq for EdgeInput<'_> {}

        impl PartialEq for EdgeInput<'_> {
            #[inline(always)]
            fn eq(&self, other: &Self) -> bool {
                self.input.eq(other.input)
            }
        }

        impl Hash for EdgeInput<'_> {
            #[inline(always)]
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.input.hash(state);
            }
        }

        edges.values().filter_map(|edge| {
            if let comp::Phony::NotPhony { inputs, .. } = &edge.phony {
                Some(inputs.iter().map(|input| EdgeInput { edge, input }))
            } else {
                None
            }
        }).flatten().collect::<HashSet::<_, FxBuildHasher>>().iter().for_each(|EdgeInput {edge, input}| {
            if !edges.contains_key(input) && !fs::exists::<&Path>(input.as_ref()).unwrap_or(false) {
                let mut msg = report_fmt!{
                    edge.loc,
                    "undefined edge: {input}, {target} depends on it",
                    target = edge.target
                };

                if let Some(compiled) = did_you_mean_compiled(input, &edges, &rules) {
                    let msg_ = format!("\nnote: did you mean: {compiled}?");
                    msg.push_str(&msg_)
                }

                report_panic!("{msg}")
            }
        });

        let default_target = default_target.map(|prep::Target { loc, target }| {
            comp::Target {
                loc,
                target: Template::new(target, loc).compile_def(&defs)
            }
        });

        let cache_file_path = if let Some(comp::Def(build_dir)) = defs.get(BUILD_DIR_VARIABLE) {
            let mut path_buf = PathBuf::from(build_dir);
            path_buf.push(Db::RUSH_FILE_NAME);
            Cow::Owned(path_buf.to_string_lossy().into_owned())
        } else {
            Cow::Borrowed(Db::RUSH_FILE_NAME)
        };

        Compiled {edges, rules, defs, default_target, cache_file_path}
    }
}

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Compiled<'a> {
    pub defs: comp::Defs<'a>,
    pub cache_file_path: Cow::<'a, str>,
    pub rules: StrHashMap::<'a, Rule<'a>>,
    pub default_target: comp::DefaultTarget<'a>,
    pub edges: StrHashMap::<'a, comp::Edge<'a>>,
}

impl Compiled<'_> {
    pub fn generate_clean_edge(&self, flags: &Flags) -> Command<'_> {
        let mut targets = self.edges.values()
            .filter(|j| matches!(j.phony, comp::Phony::NotPhony { .. }))
            .fold(Vec::with_capacity(64), |mut files, j| {
                if fs::exists::<&Path>(j.target.as_ref()).unwrap_or(false) {
                    files.push(j.target)
                }

                if let Some(dp) = j.depfile() {
                    if fs::exists::<&Path>(dp.as_ref()).unwrap_or(false) {
                        files.push(dp)
                    }
                } files
            });

        if fs::exists::<&Path>(Db::RUSH_FILE_NAME.as_ref()).unwrap_or(false) {
            targets.push(Db::RUSH_FILE_NAME)
        }

        let count = targets.len().to_string();

        let (command, description) = if flags.verbose() {
            let targets_str = targets.join(" ");
            let command = format!("rm -f {targets_str}");
            let mut description = String::with_capacity(
                (1 + "deleted".len() + targets.len() + 1 + 1) * 24 +
                1 + "cleaned".len() + 20 + "files".len() + 1
            );
            for target in targets.iter() {
                description.push_str("[cleaned ");
                description.push_str(target);
                description.push_str("]\n");
            }
            description.push_str("[cleaned ");
            description.push_str(&count);
            description.push_str(" files]");
            (command, description)
        } else {
            let targets_str = targets.join(" ");
            let command = format!("rm -f {targets_str}");
            let description = format!("[cleaned {count} files]");
            (command, description)
        };

        Command {
            command: Cow::Owned(command),
            target: Cow::Borrowed(CLEAN_TARGET),
            description: Some(Cow::Owned(description))
        }
    }

    #[inline]
    pub fn pretty_print_rules(&self) -> String {
        let mut rules = self.rules.iter().collect::<Vec::<_>>();
        rules.sort_unstable_by(|(_, ra), (_, rb)| ra.command.loc.row.cmp(&rb.command.loc.row));
        let rules = rules.iter().map(|(s, _)| s).collect::<Vec::<_>>();
        util::pretty_print_slice(&rules, ", ")
    }

    #[inline]
    pub fn pretty_print_targets(&self) -> String {
        let mut targets = self.edges.iter().collect::<Vec::<_>>();
        targets.sort_unstable_by(|(_, ea), (_, eb)| ea.loc.row.cmp(&eb.loc.row));
        let targets = targets.iter().map(|(s, _)| s).collect::<Vec::<_>>();
        util::pretty_print_slice(&targets, ", ")
    }
}

#[repr(packed)]
#[derive(Copy, Clone)]
#[cfg_attr(feature = "dbg", derive(Debug))]
struct DepfilePath<'a> {
    loc: Loc<'a>,
    path: &'a str
}

#[repr(packed)]
#[derive(Copy, Clone)]
#[cfg_attr(feature = "dbg", derive(Debug))]
struct Description<'a> {
    loc: Loc<'a>,
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
        loc: Loc<'a>,

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
    arena: &'a Bump,
    file_path: &'a str,
    context: Context<'a>,
    pub parsed: Parsed<'a>,
}

impl<'a> Parser<'a> {
    pub const RUSH_FILE_PATH: &'static str = "build.rush";

    #[inline]
    #[cfg_attr(feature = "dbg", track_caller)]
    fn finish_rule(&self) {
        match &self.context {
            Context::Rule { loc, name, already_inserted, .. } => {
                if !already_inserted {
                    report_panic!(loc, "rule {name} without a command")
                }
            },
            _ => {}
        };
    }

    #[inline]
    fn finish_edge(&mut self) {
        match &mut self.context {
            Context::Job { target, shadows } => {
                let edge = self.parsed.edge_mut(target);
                if let Some(shadows) = shadows.take() {
                    edge.shadows.replace(Arc::new(shadows));
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
                self.finish_edge();
                self.finish_rule();
                self.context = Context::Global
            }
        }

        if trimmed.is_empty() { return }

        let line = trimmed;
        let first = line.find(|c: char| c.is_ascii_whitespace()).map(|first_space| {
            (first_space, &line[..first_space])
        });

        let get_loc = || -> Loc<'_> {
            Loc::new(self.cursor, self.file_path)
        };

        let new_loc = |row: usize| -> Loc<'_> {
            Loc::new(row, self.file_path)
        };

        let Some((first_space, first_token)) = first else {
            report_panic!(get_loc(), "undefined token: {line}")
        };

        let Some(second_space) = line[first_space..].find(|c: char| !c.is_ascii_whitespace())
            .map(|p| p + first_space) else {
                return
            };

        let second_token = line[first_space..].trim();

        let parse_shadow = || -> &'a str {
            let check_start = first_space;
            let check_end = (second_space + 1 + 1).min(line.len());
            if !line[check_start..check_end].contains('=') {
                report_panic!(get_loc(), "expected `=` in variable definition")
            }
            line[second_space + 1..].trim()
        };

        let parse_def = || -> prep::Def<'a> {
            prep::Def(Template::new(parse_shadow(), get_loc()))
        };

        // TODO: we should move context here and this match should return another updated context
        match &mut self.context {
            Context::Job { target, shadows, .. } => {
                match first_token {
                    COMMAND => {
                        let command_ = line[second_space + 1 + 1..].trim();
                        let command_loc = get_loc();
                        let command = Template::new(command_, command_loc);
                        let edge = self.parsed.edge_mut(target);
                        edge.phony = edge.into_phony(Some(command));
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
            Context::Rule { loc, name, depfile, already_inserted, description } => {
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
                            new_loc(command_loc),
                            *description,
                            *depfile,
                        );
                        self.parsed.rules.insert(name, rule);
                        self.context = Context::Rule {
                            name,
                            loc: *loc,
                            already_inserted: true,
                            description: *description,
                            depfile: *depfile,
                        }
                    },
                    DEPFILE => {
                        let path = parse_shadow();
                        let depfile = DepfilePath {path, loc: get_loc()};
                        if *already_inserted {
                            let rule = self.parsed.rule_mut(name);
                            let depfile = Template::new(path, depfile.loc);
                            rule.depfile = Some(depfile)
                        } else {
                            self.context = Context::Rule {
                                name,
                                loc: *loc,
                                depfile: Some(depfile),
                                description: *description,
                                already_inserted: *already_inserted
                            }
                        }
                    },
                    DESCRIPTION => {
                        let description = line[second_space + 1 + 1..].trim();
                        let description = Description { description, loc: get_loc() };
                        if *already_inserted {
                            let rule = self.parsed.rule_mut(name);
                            rule.description = Some(Template::new(description.description, description.loc));
                        } else {
                            self.context = Context::Rule {
                                name,
                                loc: *loc,
                                depfile: *depfile,
                                description: Some(description),
                                already_inserted: *already_inserted
                            }
                        }
                    },
                    _ => if line.chars().next() != Some(COMMENT) {
                        report_panic!{
                            get_loc(),
                            "undefined property: `{first_token}`, existing properties are: `command`, `description`"
                        }
                    }
                }
            },
            Context::Global => {
                match first_token {
                    SUBRUSH => {
                        let path = second_token;

                        let content = match util::read_file_into_arena_str(self.arena, path) {
                            Ok(ok) => ok,
                            Err(e) => report_panic!{
                                get_loc(),
                                "could not read: {path}: {e}"
                            }
                        };

                        // TODO: preprocess allocated string in-place, without additional allocation
                        let buf = self.arena.alloc_slice_fill_default(content.len());

                        let mut string = unsafe { String::from_raw_parts(buf.as_mut_ptr(), 0, buf.len()) };

                        let escaped_indexes = Parser::preprocess_content(content, &mut string);

                        let len = string.len();
                        let ptr = string.as_ptr();

                        let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
                        let content = unsafe { std::str::from_utf8_unchecked(&slice) };

                        // this memory belongs to arena -> it is going to be deallocated
                        mem::forget(string);

                        let old_file_path = self.file_path;
                        self.file_path = path;

                        _ = self.parse_lines(content, &escaped_indexes);

                        self.file_path = old_file_path
                    },
                    PHONY => {
                        let phony = second_token;
                        if phony.as_bytes().last() == Some(&b':') {
                            report_panic!{
                                get_loc(),
                                "you can define phony edge following way:\n  phony {target}\n  build {target}:\n    ...",
                                target = &phony[..(phony.len() - 1).max(1)]
                            }
                        }
                        self.parsed.phonys.insert(phony);
                        if let Some(edge) = self.parsed.edges.get_mut(phony) {
                            edge.phony = edge.into_phony(None)
                        }
                    },
                    DEFAULT => {
                        self.parsed.default_target = Some(prep::Target {
                            loc: get_loc(),
                            target: second_token
                        });
                    },
                    RULE => {
                        self.context = Context::Rule {
                            depfile: None,
                            description: None,
                            loc: get_loc(),
                            already_inserted: false,
                            name: line[second_space..].trim_end(),
                        }
                    },
                    BUILD => {
                        let Some(colon_idx) = line.find(':') else {
                            report_panic!(get_loc(), "expected colon after build target")
                        };
                        let post_colon = line[colon_idx + 1..].trim();
                        let or_idx = post_colon.find('|');

                        let loc = get_loc();
                        let target = line[first_space..colon_idx].trim();
                        let target_template = Template::new(target, loc);

                        let edge = if self.parsed.phonys.contains(target) {
                            let aliases_str = if let Some(or_idx) = or_idx {
                                post_colon[..or_idx].trim_end()
                            } else {
                                post_colon.trim_end()
                            };

                            let aliases = aliases_str.split_ascii_whitespace().collect::<Vec::<_>>();
                            let aliases_templates = aliases.iter().map(|alias| Template::new(alias, loc)).collect();

                            prep::Edge {
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
                            prep::Edge {
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

                        self.parsed.edges.insert(target, edge);
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
    pub fn preprocess_content(input: &str, buf: &mut String) -> EscapedIndexes {
        let mut indexes = Vec::with_capacity(32);

        let mut lines = input.lines().enumerate().peekable();
        while let Some((index, line)) = lines.next() {
            if line.trim_start().as_bytes().first() == Some(&(COMMENT as u8)) {
                indexes.push((index, 1));
                continue
            }

            let line = if let Some(comment_idx) = line.rfind(COMMENT) {
                &line[..comment_idx]
            } else {
                line
            };

            let mut escaped_lines = 0;
            let mut curr_line = line.trim_end();
            while curr_line.as_bytes().last() == Some(&(LINE_ESCAPE as u8)) {
                escaped_lines += 1;
                curr_line = curr_line[..curr_line.len() - 1].trim_end();

                if let Some((.., next_line)) = lines.peek() {
                    let next_trimmed = next_line.trim_start();
                    buf.push_str(curr_line);
                    buf.push(' ');
                    curr_line = next_trimmed;
                    _ = lines.next()
                } else {
                    break
                }
            }

            if escaped_lines > 0 {
                indexes.push((index, escaped_lines))
            }

            buf.push_str(curr_line);
            buf.push('\n')
        } indexes
    }

    #[inline]
    fn parse_lines(&mut self, content: &'a str, escaped_indexes: &EscapedIndexes) {
        let mut escaped_index = 0;
        for line in content.lines() {
            self.cursor += 1;
            if escaped_indexes.get(escaped_index).map_or(false, |(row, _)| *row + 1 <= self.cursor) {
                self.cursor += escaped_indexes[escaped_index].1;
                escaped_index += 1
            }
            self.parse_line(line, line.trim_start())
        }
    }

    #[cfg_attr(feature = "dbg", tramer("millis"))]
    pub fn parse(arena: &'a Bump, content: &'a str, file_path: &'a str, escaped_indexes: &EscapedIndexes) -> Parsed<'a> {
        let mut parser = Self {
            arena,
            cursor: 0,
            file_path,
            parsed: Parsed {
                default_target: None,
                defs: prep::Defs(StrHashMap::with_capacity(32)),
                edges: {
                    let mut edges = StrHashMap::from_iter([(CLEAN_TARGET, prep::Edge {
                        loc: Loc::new(420, "aboba"),
                        target: CLEAN_TARGET,
                        shadows: Shadows::None,
                        target_template: Template::new(CLEAN_TARGET, Loc::new(420, "aboba")),
                        phony: prep::Phony::Phony {
                            command: None,
                            aliases: const { Vec::new() },
                            aliases_templates: const { Vec::new() }
                        }
                    })].into_iter());
                    _ = edges.try_reserve(32);
                    edges
                },
                rules: StrHashMap::with_capacity(32),
                phonys: {
                    let mut phonys = StrHashSet::from_iter(PHONY_TARGETS.into_iter().cloned());
                    _ = phonys.try_reserve(32);
                    phonys
                },
            },
            context: Context::Global
        };

        parser.parse_lines(content, escaped_indexes);
        parser.finish_edge();
        parser.finish_rule();
        parser.parsed
    }
}
