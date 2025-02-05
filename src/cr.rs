use crate::types::StrDashSet;
use crate::consts::PHONY_TARGETS;
use crate::parser::{Job, Rule, Phony, Processed};
use crate::command::{Command, MetadataCache, CommandOutput};
use crate::graph::{Graph, TransitiveDeps, topological_sort_levels};

use std::sync::Arc;
use std::path::Path;
use std::io::{self, Write};
use std::thread::{self, JoinHandle};

use dashmap::DashSet;
use rayon::prelude::*;
use fxhash::FxBuildHasher;
use crossbeam_channel::{unbounded, Sender};

pub type DefaultTarget<'a> = Option::<&'a Job<'a>>;

type Stdout = Sender::<String>;
type StdoutThread = JoinHandle::<()>;

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct CommandRunner<'a> {
    stdout: Stdout,
    graph: Graph<'a>,
    context: &'a Processed<'a>,
    processed: StrDashSet::<'a>,
    metadata_cache: MetadataCache<'a>,
    transitive_deps: &'a TransitiveDeps<'a>
}

impl<'a> CommandRunner<'a> {
    pub fn run(
        context: &'a Processed,
        graph: Graph<'a>,
        default_target: DefaultTarget<'a>,
        transitive_deps: &'a TransitiveDeps<'a>
    ) {
        let (stdout, writer) = Self::stdout_thread();
        let cb = Self::new(stdout, graph, context, transitive_deps);

        let levels = if let Some(job) = default_target {
            cb.run_target(job);
            cb.drop(writer);
            return
        } else {
            topological_sort_levels(&cb.graph)
        };

        levels.into_iter().for_each(|level| {
            level.into_par_iter().filter_map(|t| context.jobs.get(t)).for_each(|job| {
                cb.resolve_and_run(job)
            });
        });

        cb.drop(writer);
    }

    #[inline]
    fn new(
        stdout: Stdout,
        graph: Graph<'a>,
        context: &'a Processed<'a>,
        transitive_deps: &'a TransitiveDeps<'a>
    ) -> Self {
        let n = context.jobs.len();
        Self {
            graph,
            stdout,
            context,
            processed: DashSet::with_capacity_and_hasher(n, FxBuildHasher::default()),
            metadata_cache: MetadataCache::new(n),
            transitive_deps
        }
    }

    #[inline]
    fn drop(self, writer: StdoutThread) {
        drop(self);
        _ = writer.join()
    }

    #[inline]
    fn execute_command(&self, command: Command, target: &str) -> io::Result::<CommandOutput> {
         command.execute().map_err(|e| {
             let err = format!("could not execute job: {target}: {e}\n");
             _ = self.print(err);
             e
         })
    }

    #[track_caller]
    fn run_phony(&self, job: &Job<'a>) {
        match &job.phony {
            Phony::Phony { command, aliases, .. } => {
                aliases.iter().filter_map(|_job| {
                    match self.context.jobs.get(_job.as_str()) {
                        Some(j) => Some(j),
                        None => {
                            let e = report_fmt!{
                                job.loc,
                                "undefined job: {target}\nNOTE: in phony jobs you can only alias jobs, no input permitted here\n",
                                target = job.target
                            };
                            _ = self.print(e);
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
                    let output = match command_output {
                        Ok(ok) => ok.to_string(),
                        Err(e) => e.to_string()
                    };
                    _ = self.print(output);
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
        } #[cfg(not(feature = "dbg"))] unsafe {
            self.stdout.send(s).unwrap_unchecked()
        }
    }

    fn execute_job(&self, job: &Job<'a>, rule: &Rule) -> bool {
        if self.metadata_cache.needs_rebuild(job, &self.transitive_deps) {
            if let Err(e) = self.create_dirs_if_needed(job.target) {
                let msg = format!{
                    "could not create build directory for target: {target}: {e}",
                    target = job.target
                };
                _ = self.print(msg);
                return false
            }

            let Some(command) = rule.command.compile(job, &self.context.defs).map_err(|e| {
                _ = self.print(e);
                rule.description.as_ref().and_then(|d| d.check(&self.context.defs).err()).map(|err| {
                    _ = self.print(err)
                });
            }).ok() else {
                return true
            };

            let description = rule.description.as_ref().and_then(|d| d.compile(&job, &self.context.defs).map_err(|e| {
                _ = self.print(e)
            }).ok());

            let command = Command { command, description };
            let Ok(command_output) = self.execute_command(command, job.target) else {
                return true
            };

            _ = self.print(command_output.to_string());
        } else {
            let mut any_err = false;
            if let Err(err) = rule.command.check(&self.context.defs) {
                _ = self.print(err);
                any_err = true
            }

            if let Some(Err(err)) = rule.description.as_ref().map(|d| d.check(&self.context.defs)) {
                _ = self.print(err);
                any_err = true
            }

            if !any_err {
                let msg = format!("{target} is already built\n", target = job.target);
                _ = self.print(msg)
            }
        } false
    }

    fn resolve_and_run(&self, job: &Job<'a>) {
        // TODO: reserve total amount of jobs here
        let mut stack = vec![job];
        while let Some(job) = stack.pop() {
            if self.processed.contains(job.target) { continue }

            match &job.phony {
                Phony::Phony { .. } => {
                    self.run_phony(job);
                    return
                },
                Phony::NotPhony { rule, inputs, .. } => {
                    let mut all_deps_resolved = true;
                    for input in inputs.iter() {
                        if !self.processed.contains(input) {
                            if let Some(dep_job) = self.context.jobs.get(input) {
                                stack.push(job);
                                stack.push(dep_job);
                                all_deps_resolved = false;
                                break
                            } else {
                                #[cfg(feature = "dbg")] {
                                    let msg = format!("dependency {input} is assumed to exist\n");
                                    _ = self.print(msg)
                                }
                            }
                        }
                    }

                    if !all_deps_resolved { continue }

                    if let Some(ref job_rule) = rule {
                        if let Some(rule) = self.context.rules.get(job_rule) {
                            self.processed.insert(job.target);
                            if self.execute_job(job, rule) { continue }
                        } else {
                            let err = report_fmt!{
                                job.loc,
                                "no rule named: {rule} found for job {target}\n",
                                rule = job_rule,
                                target = job.target
                            };
                            _ = self.print(err)
                        }
                    }
                }
            }
        }
    }
}
