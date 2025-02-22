use crate::flags::Flags;
use crate::types::StrHashSet;
use crate::db::{Db, Metadata};
use crate::ux::did_you_mean_compiled;
use crate::parser::comp::{Edge, Phony};
use crate::command::{Command, MetadataCache};
use crate::parser::{Rule, Compiled, DefaultEdge};
use crate::consts::{CLEAN_TARGET, PHONY_TARGETS};
use crate::poll::{Poller, FdSender, PollingThread};
use crate::graph::{Graph, Levels, topological_sort};

use std::sync::Arc;
use std::{fs, thread};
use std::time::Duration;
use std::io::{self, Write};
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::sync::atomic::{Ordering, AtomicBool, AtomicUsize};

use dashmap::DashMap;

#[derive(PartialEq)]
#[cfg_attr(feature = "dbg", derive(Debug))]
enum ExecutorFlow { Ok, Stop }

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct CommandRunner<'a> {
    context: &'a Compiled<'a>,

    clean: Command<'a>,

    graph: Graph<'a>,
    transitive_deps: Graph<'a>,

    poller: Arc::<Poller>,
    polling_thread: PollingThread,

    fd_sender: FdSender,

    executed_jobs: usize,

    not_up_to_date: Option::<&'a str>,

    db_read: Option::<Db<'a>>,
    db_write: Db<'a>,

    default_edge: DefaultEdge<'a>,

    executed: StrHashSet<'a>,

    metadata_cache: MetadataCache<'a>,
}

impl std::ops::Deref for CommandRunner<'_> {
    type Target = Poller;
    #[inline(always)]
    fn deref(&self) -> &Self::Target { &self.poller }
}

impl<'a> CommandRunner<'a> {
    fn run_levels(&mut self, levels: &Levels<'a>) {
        'outer: for level in levels.into_iter() {
            #[cfg(feature = "dbg")] {
                println!("RUNNING LEVEL: {level:#?}")
            }

            {
                self.jobs_done.store(0, Ordering::Relaxed);
                self.executed_jobs = 0
            }

            for edge in level.into_iter().filter_map(|t| self.context.edges.get(t)) {
                self.resolve_and_run(edge);
                if self.stop.load(Ordering::Relaxed) { break 'outer }
            }

            loop {
                let jobs_done = self.jobs_done.load(Ordering::Relaxed);
                if jobs_done >= self.executed_jobs { break }
                #[cfg(feature = "dbg")] {
                    println!("{jobs_done} < {e}", e = self.executed_jobs)
                } thread::sleep(Duration::from_millis(50))
            }

            {
                self.executed_jobs = 0;
                self.jobs_done.store(0, Ordering::Relaxed)
            }
        }
    }

    fn new(
        clean: Command<'a>,
        context: &'a Compiled<'a>,
        graph: Graph<'a>,
        transitive_deps: Graph<'a>,
        flags: Flags,
        db_read: Option::<Db<'a>>,
        default_edge: DefaultEdge<'a>,
    ) -> Self {
        let n = context.edges.len();
        let (poller, fd_sender, polling_thread) = Poller::spawn(
            Arc::new(flags),
            Arc::new(AtomicBool::new(false)),
            Arc::new(AtomicUsize::new(0)),
            Arc::new(AtomicUsize::new(0)),
            Arc::new(DashMap::new())
        );
        Self {
            clean,
            graph,
            poller,
            context,
            db_read,
            fd_sender,
            default_edge,
            polling_thread,
            transitive_deps,

            executed_jobs: 0,
            not_up_to_date: None,
            db_write: Db::write(),
            metadata_cache: MetadataCache::new(n),
            executed: StrHashSet::with_capacity(n),
        }
    }

    #[inline]
    fn finish(self) -> Db<'a> {
        if self.flags.check_is_up_to_date() {
            match self.not_up_to_date {
                None => println!("[up to date]"),
                Some(target) => println!("[{target} is not up to date]")
            }
        }
        self.stop.store(true, Ordering::Relaxed);
        _ = self.polling_thread.join();
        self.db_write
    }

    #[inline]
    fn build_subgraph(&self, target: &'a str) -> Graph<'a> {
        let deps = self.transitive_deps.get(target).cloned().unwrap_or_default();
        let mut subgraph = Graph::with_capacity(deps.len() + 1);

        subgraph.insert(target, Arc::clone(&deps));
        for dep in deps.iter() {
            if let Some(deps_of_dep) = self.graph.get(dep) {
                subgraph.insert(&dep, Arc::clone(deps_of_dep));
            }
        } subgraph
    }

    #[inline]
    fn resolve_and_run_target(&mut self, edge: &Edge<'a>) {
        let levels = {
            let subgraph = self.build_subgraph(edge.target);
            topological_sort(&subgraph)
        };
        _ = self.run_levels(&levels)
    }

    pub fn run(
        clean: Command<'a>,
        context: &'a Compiled,
        graph: Graph<'a>,
        transitive_deps: Graph<'a>,
        flags: Flags,
        db_read: Option::<Db<'a>>,
        default_edge: DefaultEdge<'a>,
    ) -> Db<'a> {
        let mut cr = Self::new(
            clean,
            context,
            graph,
            transitive_deps,
            flags,
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
            topological_sort(&cr.graph)
        };

        _ = cr.run_levels(&levels);
        cr.finish()
    }

    #[inline]
    fn execute_command(&self, command: &Command, target: &str) -> io::Result::<()> {
        if self.flags.print_commands() {
            let output = command.to_string(&self.flags);
            println!("{output}");
            return Ok(())
        }

        command.execute(&self.poller, FdSender::clone(&self.fd_sender)).map_err(|e| {
            println!("[could not execute edge: {target}: {e}]");
            e
        })
    }

    fn run_phony(&mut self, edge: &'a Edge<'a>) {
        if edge.target == CLEAN_TARGET {
            _ = self.execute_command(&self.clean, CLEAN_TARGET).map(|_| self.executed_jobs += 1);
            return
        }

        let Phony::Phony { command, aliases, .. } = &edge.phony else { unreachable!() };

        aliases.iter().filter_map(|_edge| {
            match self.context.edges.get(_edge.as_str()) {
                Some(j) => Some(j),
                None => {
                    let mut msg = report_fmt!{
                        edge.loc,
                        "undefined edge: {target}\nNOTE: in phony jobs you can only alias jobs\n",
                        target = _edge
                    };

                    if let Some(compiled) = did_you_mean_compiled(
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
            let command = Command {command, target: edge.target, description: None};
            self.execute_command(&command, edge.target).map(|_| self.executed_jobs += 1)
        });
    }

    fn execute_edge(&mut self, edge: &Edge<'a>, rule: &Rule) -> ExecutorFlow {
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
            if _self.flags.always_build() && !_self.flags.check_is_up_to_date() {
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

                if let Ok(mut file) = fs::File::create_new(path_buf) {
                    _ = file.write(b"*")
                }
            } Ok(())
        }

        if edge.target == CLEAN_TARGET {
            _ = self.execute_command(&self.clean, CLEAN_TARGET).map(|_| self.executed_jobs += 1);
            return ExecutorFlow::Ok
        }

        let Some(command) = compile_command(self, edge, rule) else {
            return ExecutorFlow::Ok
        };

        if needs_rebuild(self, edge, &command) {
            if self.flags.check_is_up_to_date() {
                self.not_up_to_date = Some(edge.target);
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

            let target = edge.target;
            let command = command.as_ref();
            let description = description.as_deref();

            let command = Command {target, command, description};
            _ = self.execute_command(&command, edge.target).map(|_| self.executed_jobs += 1);
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

            if !any_err && self.flags.verbose() ||
                self.default_edge.as_ref().map_or(false, |def| {
                    def.target == edge.target || def.aliases().map_or(false, |aliases| {
                        aliases.iter().any(|a| a == edge.target)
                    })
                }) && !self.flags.check_is_up_to_date()
            {
                println!("[{target} is already built]", target = edge.target)
            }
        } ExecutorFlow::Ok
    }

    fn resolve_and_run(&mut self, edge: &'a Edge<'a>) {
        // TODO: reserve total amount of edges here
        let mut stack = vec![edge];
        while let Some(edge) = stack.pop() {
            if self.executed.contains(edge.target) { continue }

            let Phony::NotPhony { rule, inputs, .. } = &edge.phony else {
                self.run_phony(edge);
                return
            };

            let mut all_deps_resolved = true;
            for input in inputs.iter() {
                if !self.executed.contains(input) {
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
                self.executed.insert(edge.target);
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
