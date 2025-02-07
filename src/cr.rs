use crate::mode::Mode;
use crate::types::StrDashSet;
use crate::db::{Db, Metadata};
use crate::consts::PHONY_TARGETS;
use crate::parser::comp::{Job, Phony};
use crate::command::{Command, MetadataCache, CommandOutput};
use crate::parser::{Rule, Processed, DefaultJob};
use crate::graph::{Graph, topological_sort_levels};

use std::sync::Arc;
use std::path::Path;
use std::io::{self, Write};
use std::hash::{Hash, Hasher};
use std::thread::{self, JoinHandle};

use dashmap::DashSet;
use rayon::prelude::*;
use fxhash::FxBuildHasher;
use crossbeam_channel::{unbounded, Sender};

type Stdout = Sender::<String>;
type StdoutThread = JoinHandle::<()>;

macro_rules! cr_print {
    ($self: expr, $($arg:tt)*) => {{
        #[cfg(feature = "dbg")] {
            _ = $self.print(format!{
                "{f}:{l}:{c}:\n",
                f = file!(), l = line!(), c = column!()
            });
        }
        _ = $self.print(std::fmt::format(format_args!($($arg)*)));
    }};
}

macro_rules! cr_report {
    ($self: expr, $($arg:tt)*) => {{
        #[cfg(feature = "dbg")] {
            _ = $self.print(format!{
                "{f}:{l}:{c}:\n",
                f = file!(), l = line!(), c = column!()
            });
        }
        _ = $self.print(report_fmt!($($arg)*));
    }};
}

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct CommandRunner<'a> {
    mode: &'a Mode,
    stdout: Stdout,
    graph: Graph<'a>,
    db_read: Option::<Db<'a>>,
    db_write: Db<'a>,
    context: &'a Processed<'a>,
    default_job: DefaultJob<'a>,
    executed: StrDashSet::<'a>,
    metadata_cache: MetadataCache<'a>,
    transitive_deps: Graph<'a>
}

impl<'a> CommandRunner<'a> {
    pub fn run(
        mode: &'a Mode,
        context: &'a Processed,
        graph: Graph<'a>,
        db: Option::<Db<'a>>,
        default_job: DefaultJob<'a>,
        transitive_deps: Graph<'a>
    ) -> Db<'a> {
        let (stdout, writer) = Self::stdout_thread();
        let cr = Self::new(mode, stdout, graph, db, default_job, context, transitive_deps);

        let levels = if let Some(job) = default_job {
            cr.run_target(job);
            return cr.finish(writer)
        } else {
            topological_sort_levels(&cr.graph)
        };

        levels.into_iter().for_each(|level| {
            level.into_par_iter().filter_map(|t| context.jobs.get(t)).for_each(|job| {
                cr.resolve_and_run(job)
            });
        });

        cr.finish(writer)
    }

    #[inline]
    fn new(
        mode: &'a Mode,
        stdout: Stdout,
        graph: Graph<'a>,
        db_read: Option::<Db<'a>>,
        default_job: DefaultJob<'a>,
        context: &'a Processed<'a>,
        transitive_deps: Graph<'a>
    ) -> Self {
        let n = context.jobs.len();
        Self {
            mode,
            graph,
            stdout,
            context,
            db_read,
            default_job,
            transitive_deps,
            db_write: Db::write(),
            executed: DashSet::with_capacity_and_hasher(n, FxBuildHasher::default()),
            metadata_cache: MetadataCache::new(n)
        }
    }

    #[inline]
    fn finish(self, writer: StdoutThread) -> Db<'a> {
        drop(self.stdout);
        _ = writer.join();
        self.db_write
    }

    #[inline]
    fn execute_command(&self, command: Command, target: &str) -> io::Result::<CommandOutput> {
        command.execute().map_err(|e| {
            cr_print!(self, "could not execute job: {target}: {e}\n");
            e
        })
    }

    fn run_phony(&self, job: &Job<'a>) {
        match &job.phony {
            Phony::Phony { command, aliases, .. } => {
                aliases.iter().filter_map(|_job| {
                    match self.context.jobs.get(_job.as_str()) {
                        Some(j) => Some(j),
                        None => {
                            cr_report!{
                                self,
                                job.loc,
                                "undefined job: {target}\nNOTE: in phony jobs you can only alias jobs\n",
                                target = _job
                            };
                            None
                        }
                    }
                }).for_each(|job| self.resolve_and_run_target(job));

                if let Some(command_output) = {
                    command.as_ref().map(|c| {
                        let command = Command { command: c.to_owned(), description: None };  
                        self.execute_command(command, job.target)
                    })
                } {
                    cr_print!{
                        self,
                        "{output}",
                        output = match command_output {
                            Ok(ok) => ok.to_string(&self.mode),
                            Err(e) => e.to_string()
                        }
                    }
                }
            },
            Phony::NotPhony { .. } => unreachable!()
        }
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
    fn resolve_and_run_target(&self, job: &Job<'a>) {
        let levels = {
            let subgraph = self.build_subgraph(job.target);
            topological_sort_levels(&subgraph)
        };

        levels.into_iter().for_each(|level| {
            level.into_par_iter().filter_map(|t| self.context.jobs.get(t)).for_each(|job| {
                self.resolve_and_run(job);
            });
        });
    }

    #[inline]
    fn stdout_thread() -> (Stdout, StdoutThread) {
        let (stdout, stdout_recv) = unbounded::<String>();
        let writer = thread::spawn(move || {
            let mut stdout_handle = io::stdout().lock();
            for s in stdout_recv {
                _ = stdout_handle.write_all(s.as_bytes());
            }
            _ = stdout_handle.flush();
        }); (stdout, writer)
    }

    #[inline]
    fn run_target(&self, job: &Job<'a>) {
        if PHONY_TARGETS.contains(&job.target) {
            self.run_phony(job);
            return
        }

        self.resolve_and_run_target(job);
    }

    #[inline]
    fn create_dirs_if_needed(&self, path: &str) -> io::Result::<()> {
        let path: &Path = path.as_ref();
        if let Some(parent) = path.parent() {
            if parent.exists() { return Ok(()) }
            std::fs::create_dir_all(parent)?
        } Ok(())
    }

    #[inline(always)]
    fn print(&self, s: String) {
        #[cfg(feature = "dbg")] {
            self.stdout.send(s).unwrap()
        } #[cfg(not(feature = "dbg"))] {
            _ = self.stdout.send(s);
        }
    }

    #[inline]
    fn hash(command: &str) -> u64 {
        let mut hasher = fnv::FnvHasher::default();
        command.hash(&mut hasher);
        hasher.finish()
    }

    #[inline]
    fn needs_rebuild(&self, job: &Job<'a>, command: &str) -> bool {
        if self.mode.always_build() { return true }
        self.db_read.as_ref().map_or(false, |db| {
            db.metadata_read(job.target).map_or(false, |md| {
                md.command_hash != Self::hash(command)
            })
        }) || self.metadata_cache.needs_rebuild(job, &self.transitive_deps)
    }

    #[inline]
    fn compile_command(&self, job: &Job<'a>, rule: &Rule) -> Option::<String> {
        rule.command.compile(job, &self.context.defs).map_err(|e| {
            cr_print!(self, "{e}\n");
            rule.description.as_ref().and_then(|d| d.check(&job.shadows, &self.context.defs).err()).map(|err| {
                cr_print!(self, "{err}\n");
            });
        }).map(|command| {
            self.db_write.metadata_write(job.target, Metadata {
                command_hash: Self::hash(&command)
            }); command
        }).ok()
    }

    fn execute_job(&self, job: &Job<'a>, rule: &Rule) -> bool {
        let Some(command) = self.compile_command(job, rule) else {
            return true
        };

        if self.needs_rebuild(job, &command) {
            if let Err(e) = self.create_dirs_if_needed(job.target) {
                cr_print!{
                    self,
                    "could not create build directory for target: {target}: {e}\n",
                    target = job.target
                };
                return false
            }

            let description = rule.description.as_ref().and_then(|d| d.compile(&job, &self.context.defs).map_err(|e| {
                cr_print!(self, "{e}\n");
            }).ok());

            let command = Command { command, description };
            let Ok(command_output) = self.execute_command(command, job.target) else {
                return true
            };
            let command_output = command_output.to_string(&self.mode);
            cr_print!(self, "{command_output}");
        } else {
            let mut any_err = false;
            if let Err(err) = rule.command.check(&job.shadows, &self.context.defs) {
                cr_print!(self, "{err}\n");
                any_err = true
            }

            if let Some(Err(err)) = rule.description.as_ref().map(|d| d.check(&job.shadows, &self.context.defs)) {
                cr_print!(self, "{err}\n");
                any_err = true
            }

            if !any_err && self.mode.verbose() ||
                self.default_job.as_ref().map_or(false, |def| {
                    def.target == job.target || def.aliases().map_or(false, |aliases| {
                        aliases.iter().any(|a| a == job.target)
                    })
                })
            {
                cr_print!(self, "{target} is already built\n", target = job.target)
            }
        } false
    }

    fn resolve_and_run(&self, job: &Job<'a>) {
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
                            cr_print!(self, "dependency {input} is assumed to exist\n");
                        }
                    }
                }
            }

            if !all_deps_resolved { continue }

            let Some(ref job_rule) = rule else { continue };
            if let Some(rule) = self.context.rules.get(job_rule) {
                self.executed.insert(job.target);
                if self.execute_job(job, rule) { continue }
            } else {
                cr_report!{
                    self,
                    job.loc,
                    "no rule named: {rule} found for job {target}\n",
                    rule = job_rule,
                    target = job.target
                }
            }
        }
    }
}
