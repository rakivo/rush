use crate::ux;
use crate::util::unreachable;
use crate::types::StrDashSet;
use crate::db::{Db, Metadata};
use crate::dbg_unwrap::DbgUnwrap;
use crate::parser::{Id, Rule, Compiled};
use crate::parser::comp::{Edge, Phony};
use crate::command::{Command, MetadataCache};
use crate::consts::{CLEAN_TARGET, PHONY_TARGETS};
use crate::graph::{Graph, Levels, DefaultEdge, topological_sort};

use std::borrow::Cow;
use std::fs::{self, File};
use std::io::{self, Write};
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::collections::VecDeque;
use std::sync::{Arc, RwLock, OnceLock};
use std::sync::atomic::{Ordering, AtomicUsize};

use dashmap::DashMap;
use rayon::prelude::*;
use fxhash::FxBuildHasher;

#[derive(PartialEq)]
#[cfg_attr(feature = "dbg", derive(Debug))]
enum ExecutorFlow { Ok, Stop }

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct CommandRunner<'a> {
    ran: Arc::<RwLock::<VecDeque::<Id>>>,
    cmds: Arc::<DashMap::<Id, String>>,
    finished: Arc::<DashMap::<Id, String>>,

    context: &'a Compiled<'a>,

    graph: Graph<'a>,
    transitive_deps: Graph<'a>,

    clean: OnceLock::<Command<'a>>,

    default_edge: DefaultEdge<'a>,

    failed_edges_count: AtomicUsize,

    executed_edges: StrDashSet<'a>,

    db_read: Option::<Db<'a>>,
    db_write: Db<'a>,

    not_up_to_date: Option::<&'a str>,
    metadata_cache: MetadataCache<'a>
}

impl<'a> CommandRunner<'a> {
    fn run_levels(&self, levels: &Levels<'a>) {
        for level in levels.into_iter() {
            #[cfg(feature = "dbg")] {
                println!("RUNNING LEVEL: {level:#?}")
            }

            level.into_par_iter()
                .filter_map(|t| self.context.edges.get(t))
                .for_each(|edge| self.resolve_and_run(edge))
        }
    }

    #[inline(always)]
    fn stdout_loop(
        ran: Arc::<RwLock::<VecDeque::<Id>>>,
        cmds: Arc::<DashMap::<Id, String>>,
        finished: Arc::<DashMap::<Id, String>>
    ) {
        let mut waiting_for_output = false;
        loop {
            let Some(out) = ({
                let ran = ran.read().unwrap();
                let Some(id) = ran.front() else {
                    continue
                };
                if !waiting_for_output {
                    // it exists in `ran` -> it exists in `cmds`
                    let command = cmds.get(&id).unwrap_dbg();
                    println!("{}", *command);
                    waiting_for_output = true
                }
                finished.get(id)
            }) else {
                continue
            };

            print!("{}", *out);
            drop(out); // not holding reference into DashMap for too long

            waiting_for_output = false;
            _ = ran.write().unwrap().pop_front()

            /* NOTE:
                might as well remove id from `finished` and `cmds` here,
                but why sacrifice speed for memory in 2025?
            */

            // NOTE: sleep here?
        }
    }

    fn new(
        context: &'a Compiled<'a>,
        graph: Graph<'a>,
        transitive_deps: Graph<'a>,
        db_read: Option::<Db<'a>>,
        default_edge: DefaultEdge<'a>
    ) -> Self {
        let n = context.edges.len();

        let ran = Arc::new(RwLock::new(VecDeque::with_capacity(n)));
        let cmds = Arc::new(DashMap::with_capacity(n));
        let finished = Arc::new(DashMap::with_capacity(n));

        _ = rayon::spawn({
            let ran = Arc::clone(&ran);
            let cmds = Arc::clone(&cmds);
            let finished = Arc::clone(&finished);
            move || Self::stdout_loop(ran, cmds, finished)
        });

        Self {
            ran,
            cmds,
            graph,
            context,
            db_read,
            finished,
            default_edge,
            transitive_deps,

            not_up_to_date: None,
            db_write: Db::write(),
            clean: OnceLock::new(),
            metadata_cache: MetadataCache::new(n),
            failed_edges_count: AtomicUsize::new(0),
            executed_edges: StrDashSet::with_capacity_and_hasher(n, FxBuildHasher::default())
        }
    }

    #[inline]
    fn finish(self) -> Db<'a> {
        if self.context.flags.check_is_up_to_date() {
            match self.not_up_to_date {
                None => println!("[up to date]"),
                Some(target) => println!("[{target} is not up to date]")
            }
        } self.db_write
    }

    #[inline]
    fn build_subgraph(&self, target: &'a str) -> Graph<'a> {
        let deps = self.transitive_deps.get(target).cloned().unwrap_or_default();
        let mut subgraph = Graph::with_capacity(deps.len() + 1);

        subgraph.insert(target, Arc::clone(&deps));
        for dep in deps.iter() {
            if let Some(deps_of_dep) = self.graph.get(dep) {
                _ = subgraph.insert(&dep, Arc::clone(deps_of_dep))
            }
        } subgraph
    }

    #[inline]
    fn resolve_and_run_target(&self, edge: &Edge<'a>) {
        let levels = {
            let subgraph = self.build_subgraph(edge.target);
            topological_sort(&subgraph, self.context)
        };
        _ = self.run_levels(&levels)
    }

    pub fn run(
        context: &'a Compiled,
        graph: Graph<'a>,
        transitive_deps: Graph<'a>,
        db_read: Option::<Db<'a>>,
        default_edge: DefaultEdge<'a>,
    ) -> Db<'a> {
        let cr = Self::new(
            context,
            graph,
            transitive_deps,
            db_read,
            default_edge
        );

        let levels = if let Some(edge) = default_edge {
            if PHONY_TARGETS.contains(&edge.target) {
                cr.run_phony(edge);
            } else {
                cr.resolve_and_run_target(edge);
            }
            return cr.finish()
        } else {
            topological_sort(&cr.graph, context)
        };

        _ = cr.run_levels(&levels);
        cr.finish()
    }

    fn execute_command(&self, command: &Command, target: &'a str, id: usize) -> io::Result::<()> {
        if !self.context.flags.quiet() {
            if self.context.flags.print_commands() {
                let output = command.to_string(&self.context.flags);

                print!("{output}");
                return Ok(())
            }


            let command = command.to_string(&self.context.flags);
            self.cmds.insert(id, command);
            self.ran.write().unwrap().push_back(id);
        }

        let out = command.execute()
            .inspect_err(|e| {
                eprintln!("[could not execute edge: {target}: {e}]");
            }).map(|out| {
                if out.status == 0 { return out }
                if let Some(&max) = self.context.flags.max_fail_count() {
                    self.failed_edges_count.fetch_add(1, Ordering::Relaxed);
                    let failed_edges_count = self.failed_edges_count.load(Ordering::Relaxed);
                    if failed_edges_count >= max {
                        eprintln!{
                            "[{failed_edges_count} {job} exited with non-zero code, aborting..]",
                            job = if failed_edges_count > 1 { "jobs are" } else { "job" }
                        };
                        std::process::exit(1)
                    }
                } out
            })?;

        let out = out.to_string(&self.context.flags);

        _ = self.finished.insert(id, out);

        Ok(())
    }

    #[inline(always)]
    fn execute_clean(&self) {
        println!("[cleaning..]");
        _ = self.execute_command(&self.clean(), CLEAN_TARGET, /* TODO: tf is that */ 420)
    }

    #[inline(always)]
    fn clean(&self) -> &Command {
        self.clean.get_or_init(|| {
            self.context.generate_clean_edge(&self.context.flags)
        })
    }

    fn run_phony(&self, edge: &'a Edge<'a>) {
        if edge.target == CLEAN_TARGET {
            self.execute_clean();
            return
        }

        let Phony::Phony { command, aliases, .. } = &edge.phony else {
            unreachable()
        };

        aliases.par_iter().filter_map(|_edge| {
            match self.context.edges.get(_edge.as_str()) {
                Some(j) => Some(j),
                None => {
                    let mut msg = report_fmt!{
                        edge.loc,
                        "undefined edge: {target}\nNOTE: in phony jobs you can only alias jobs\n",
                        target = _edge
                    };

                    if let Some(compiled) = ux::did_you_mean_compiled(
                        _edge,
                        &self.context.edges,
                        &self.context.rules
                    ) {
                        let msg_ = format!("\nnote: did you mean: {compiled}?");
                        msg.push_str(&msg_)
                    }

                    report_panic!("{msg}")
                }
            }
        }).for_each(|edge| self.resolve_and_run_target(edge));

        _ = command.as_ref().map(|command| {
            let target = Cow::Borrowed(edge.target);
            let command = Cow::Borrowed(command.as_str());
            let command = Command {command, target, description: None};
            self.execute_command(&command, edge.target, edge.id)
        });
    }

    fn execute_edge(&self, edge: &Edge<'a>, rule: &Rule) -> ExecutorFlow {
        #[inline(always)]
        fn hash(command: &str) -> u64 {
            let mut hasher = fnv::FnvHasher::default();
            command.hash(&mut hasher);
            hasher.finish()
        }

        #[inline]
        fn compile_command<'a>(_self: &CommandRunner<'a>, edge: &Edge<'a>, rule: &Rule) -> Option::<String> {
            rule.command.compile(edge, &_self.context.defs).map_err(|e| {
                println!("{e}");
                rule.description.as_ref()
                    .and_then(|d| d.check(&edge.shadows, &_self.context.defs).err())
                    .map(|err| println!("{err}"));
            }).map(|command| {
                _self.db_write.metadata_write(edge.target, Metadata {
                    command_hash: hash(&command)
                }); command
            }).ok()
        }

        #[inline]
        fn needs_rebuild<'a>(_self: &CommandRunner<'a>, edge: &Edge<'a>, command: &str) -> bool {
            // in `check_is_up_to_date` mode `always_build` is disabled
            // TODO: make that happen in the `Mode` struct
            if _self.context.flags.always_build() && !_self.context.flags.check_is_up_to_date() {
                return true
            }
            _self.db_read.as_ref().map_or(false, |db| {
                db.metadata_read(edge.target).map_or(false, |md| {
                    md.command_hash != hash(command)
                })
            }) || _self.metadata_cache.needs_rebuild(edge, &_self.transitive_deps)
        }

        #[inline]
        fn create_dirs_if_needed(path: &str) -> io::Result::<()> {
            let path: &Path = path.as_ref();
            if let Some(parent) = path.parent() {
                if !parent.exists() {
                    _ = fs::create_dir_all(parent)?
                }

                let mut path_buf = PathBuf::from(parent);
                path_buf.push(".gitignore");

                if let Ok(mut file) = File::create_new(path_buf) {
                    _ = file.write(b"*")
                }
            } Ok(())
        }

        if edge.target == CLEAN_TARGET {
            self.execute_clean();
            return ExecutorFlow::Ok
        }

        let Some(command) = compile_command(self, edge, rule) else {
            return ExecutorFlow::Ok
        };

        if needs_rebuild(self, edge, &command) {
            if self.context.flags.check_is_up_to_date() {
                // self.not_up_to_date = Some(edge.target);
                return ExecutorFlow::Stop
            }

            if let Err(e) = create_dirs_if_needed(edge.target) {
                println!{
                    "[could not create build directory for target: {target}: {e}]",
                    target = edge.target
                };
                return ExecutorFlow::Ok
            }

            let description = rule.description.as_ref()
                .and_then(|d| {
                    d.compile(&edge, &self.context.defs)
                        .map_err(|e| {
                            println!("{e}");
                        }).ok()
                });

            let target = Cow::Borrowed(edge.target);
            let command = Cow::Borrowed(command.as_ref());
            let description = description.as_deref().map(Into::into);

            let command = Command {target, command, description};
            _ = self.execute_command(&command, edge.target, edge.id)
        } else {
            let mut any_err = false;
            if let Err(err) = rule.command.check(&edge.shadows, &self.context.defs) {
                println!("{err}");
                any_err = true
            }

            if let Some(Err(err)) = rule.description.as_ref().map(|d| d.check(&edge.shadows, &self.context.defs)) {
                println!("{err}");
                any_err = true
            }

            if !any_err && self.context.flags.verbose() ||
                self.default_edge.as_ref().map_or(false, |def| {
                    def.target == edge.target || def.aliases().map_or(false, |aliases| {
                        aliases.iter().any(|a| a == edge.target)
                    })
                }) && !self.context.flags.check_is_up_to_date()
            {
                println!("[{target} is already built]", target = edge.target)
            }
        } ExecutorFlow::Ok
    }

    fn resolve_and_run(&self, edge: &'a Edge<'a>) {
        // TODO: reserve total amount of edges here
        let mut stack = vec![edge];
        while let Some(edge) = stack.pop() {
            if self.executed_edges.contains(edge.target) { continue }

            let Phony::NotPhony { rule, inputs, .. } = &edge.phony else {
                self.run_phony(edge);
                return
            };

            let mut all_deps_resolved = true;
            for input in inputs.iter() {
                if !self.executed_edges.contains(input) {
                    if let Some(dep_edge) = self.context.edges.get(input) {
                        stack.push(edge);
                        stack.push(dep_edge);
                        all_deps_resolved = false;
                        break
                    } else {
                        #[cfg(feature = "dbg")] {
                            println!("[dependency {input} is assumed to exist]")
                        }
                    }
                }
            }

            if !all_deps_resolved { continue }

            let Some(ref edge_rule) = rule else { continue };
            if let Some(rule) = self.context.rules.get(edge_rule) {
                self.executed_edges.insert(edge.target);
                if self.execute_edge(edge, rule) == ExecutorFlow::Stop { break }
            } else {
                report_panic!{
                    edge.loc,
                    "no rule named: {rule} found for job {target}\n",
                    rule = edge_rule,
                    target = edge.target
                }
            }
        }
    }
}
