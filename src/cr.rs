use crate::flags::Flags;
use crate::types::StrHashSet;
use crate::db::{Db, Metadata};
use crate::parser::comp::{Job, Phony};
use crate::command::{Command, MetadataCache};
use crate::parser::{Rule, Compiled, DefaultJob};
use crate::consts::{CLEAN_TARGET, PHONY_TARGETS};
use crate::poll::{Poller, FdSender, PollingThread};
use crate::graph::{Graph, Levels, topological_sort};

use std::io;
use std::thread;
use std::sync::Arc;
use std::path::Path;
use std::time::Duration;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{Ordering, AtomicBool, AtomicUsize};

use bumpalo::Bump;
use dashmap::DashMap;

#[derive(PartialEq)]
#[cfg_attr(feature = "dbg", derive(Debug))]
enum ExecutorFlow { Ok, Stop }

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct CommandRunner<'a> {
    arena: &'a Bump,

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

    default_job: DefaultJob<'a>,

    executed: StrHashSet<'a>,

    metadata_cache: MetadataCache<'a>,
}

impl std::ops::Deref for CommandRunner<'_> {
    type Target = Poller;
    #[inline(always)]
    fn deref(&self) -> &Self::Target { &self.poller }
}

impl<'a> CommandRunner<'a> {
    #[inline]
    fn run_levels(&mut self, levels: &Levels<'a>) {
        'outer: for level in levels.into_iter() {
            #[cfg(feature = "dbg")] {
                println!("RUNNING LEVEL: {level:#?}")
            }

            {
                self.jobs_done.store(0, Ordering::Relaxed);
                self.executed_jobs = 0
            }

            for job in level.into_iter().filter_map(|t| self.context.jobs.get(t)) {
                self.resolve_and_run(job);
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
        arena: &'a Bump,
        context: &'a Compiled,
        graph: Graph<'a>,
        transitive_deps: Graph<'a>,
        flags: Flags,
        db_read: Option::<Db<'a>>,
        default_job: DefaultJob<'a>,
    ) -> Self {
        let n = context.jobs.len();
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
            arena,
            poller,
            context,
            db_read,
            fd_sender,
            default_job,
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
    fn resolve_and_run_target(&mut self, job: &Job<'a>) {
        let levels = {
            let subgraph = self.build_subgraph(job.target);
            topological_sort(&subgraph)
        };
        _ = self.run_levels(&levels)
    }

    #[inline]
    pub fn run(
        clean: Command<'a>,
        arena: &'a Bump,
        context: &'a Compiled,
        graph: Graph<'a>,
        transitive_deps: Graph<'a>,
        flags: Flags,
        db_read: Option::<Db<'a>>,
        default_job: DefaultJob<'a>,
    ) -> Db<'a> {
        let mut cr = Self::new(
            clean,
            arena,
            context,
            graph,
            transitive_deps,
            flags,
            db_read,
            default_job
        );

        let levels = if let Some(job) = default_job {
            if PHONY_TARGETS.contains(&job.target) {
                cr.run_phony(job);
            } else {
                cr.resolve_and_run_target(job);
            }
            return cr.finish()
        } else {
            topological_sort(&cr.graph)
        };

        _ = cr.run_levels(&levels);
        cr.finish()
    }

    #[inline]
    fn execute_command(&self, command: &Command<'a>, target: &str) -> io::Result::<()> {
        if self.flags.print_commands() {
            let output = command.to_string(&self.flags);
            println!("{output}");
            return Ok(())
        }

        command.execute(&self.poller, self.fd_sender.clone()).map_err(|e| {
            println!("[could not execute job: {target}: {e}]");
            e
        })
    }

    #[inline]
    fn run_phony(&mut self, job: &'a Job<'a>) {
        if job.target == CLEAN_TARGET {
            _ = self.execute_command(&self.clean, CLEAN_TARGET).map(|_| self.executed_jobs += 1);
            return
        }

        let Phony::Phony { command, aliases, .. } = &job.phony else { unreachable!() };

        aliases.iter().filter_map(|_job| {
            match self.context.jobs.get(_job.as_str()) {
                Some(j) => Some(j),
                None => report_panic!{
                    job.loc,
                    "undefined job: {target}\nNOTE: in phony jobs you can only alias jobs\n",
                    target = _job
                }
            }
        }).for_each(|job| self.resolve_and_run_target(job));

        _ = command.as_ref().map(|command| {
            let command = Command { command, description: None };
            self.execute_command(&command, job.target).map(|_| self.executed_jobs += 1)
        });
    }

    fn execute_job(&mut self, job: &Job<'a>, rule: &Rule) -> ExecutorFlow {
        #[inline(always)]
        fn hash(command: &str) -> u64 {
            let mut hasher = fnv::FnvHasher::default();
            command.hash(&mut hasher);
            hasher.finish()
        }

        #[inline]
        fn compile_command<'a>(_self: &CommandRunner<'a>, job: &Job<'a>, rule: &Rule) -> Option::<String> {
            rule.command.compile(job, &_self.context.defs).map_err(|e| {
                println!("{e}");
                rule.description.as_ref()
                    .and_then(|d| d.check(&job.shadows, &_self.context.defs).err())
                    .map(|err| println!("{err}"));
            }).map(|command| {
                _self.db_write.metadata_write(job.target, Metadata {
                    command_hash: hash(&command)
                }); command
            }).ok()
        }

        #[inline]
        fn needs_rebuild<'a>(_self: &CommandRunner<'a>, job: &Job<'a>, command: &str) -> bool {
            // in `check_is_up_to_date` mode `always_build` is disabled
            // TODO: make that happen in the `Mode` struct
            if _self.flags.always_build() && !_self.flags.check_is_up_to_date() {
                return true
            }
            _self.db_read.as_ref().map_or(false, |db| {
                db.metadata_read(job.target).map_or(false, |md| {
                    md.command_hash != hash(command)
                })
            }) || _self.metadata_cache.needs_rebuild(job, &_self.transitive_deps)
        }

        #[inline]
        fn create_dirs_if_needed(path: &str) -> io::Result::<()> {
            let path: &Path = path.as_ref();
            if let Some(parent) = path.parent() {
                if parent.exists() { return Ok(()) }
                std::fs::create_dir_all(parent)?
            } Ok(())
        }

        if job.target == CLEAN_TARGET {
            _ = self.execute_command(&self.clean, CLEAN_TARGET).map(|_| self.executed_jobs += 1);
            return ExecutorFlow::Ok
        }

        let Some(command) = compile_command(self, job, rule) else {
            return ExecutorFlow::Ok
        };

        if needs_rebuild(self, job, &command) {
            if self.flags.check_is_up_to_date() {
                self.not_up_to_date = Some(job.target);
                return ExecutorFlow::Stop
            }

            if let Err(e) = create_dirs_if_needed(job.target) {
                println!{
                    "[could not create build directory for target: {target}: {e}]",
                    target = job.target
                };
                return ExecutorFlow::Ok
            }

            let description = rule.description.as_ref()
                .and_then(|d| {
                    d.compile(&job, &self.context.defs)
                        .map_err(|e| {
                            println!("{e}");
                        }).ok()
                });

            let command = self.arena.alloc_str(&command);
            let description = description.as_ref().map(|d| self.arena.alloc_str(d) as &_);

            let command = Command { command, description };
            _ = self.execute_command(&command, job.target).map(|_| self.executed_jobs += 1);
        } else {
            let mut any_err = false;
            if let Err(err) = rule.command.check(&job.shadows, &self.context.defs) {
                println!("{err}");
                any_err = true
            }

            if let Some(Err(err)) = rule.description.as_ref().map(|d| d.check(&job.shadows, &self.context.defs)) {
                println!("{err}");
                any_err = true
            }

            if !any_err && self.flags.verbose() ||
                self.default_job.as_ref().map_or(false, |def| {
                    def.target == job.target || def.aliases().map_or(false, |aliases| {
                        aliases.iter().any(|a| a == job.target)
                    })
                }) && !self.flags.check_is_up_to_date()
            {
                println!("[{target} is already built]", target = job.target)
            }
        } ExecutorFlow::Ok
    }

    fn resolve_and_run(&mut self, job: &'a Job<'a>) {
        // TODO: reserve total amount of jobs here
        let mut stack = vec![job];
        while let Some(job) = stack.pop() {
            if self.executed.contains(job.target) { continue }

            let Phony::NotPhony { rule, inputs, .. } = &job.phony else {
                self.run_phony(job);
                return
            };

            let mut all_deps_resolved = true;
            for input in inputs.iter() {
                if !self.executed.contains(input) {
                    if let Some(dep_job) = self.context.jobs.get(input) {
                        stack.push(job);
                        stack.push(dep_job);
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

            let Some(ref job_rule) = rule else { continue };
            if let Some(rule) = self.context.rules.get(job_rule) {
                self.executed.insert(job.target);
                if self.execute_job(job, rule) == ExecutorFlow::Stop { break }
            } else {
                report_panic!{
                    job.loc,
                    "no rule named: {rule} found for job {target}\n",
                    rule = job_rule,
                    target = job.target
                }
            }
        }
    }
}
