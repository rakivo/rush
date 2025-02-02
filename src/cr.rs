use crate::types::StrDashSet;
use crate::parser::{Processed, CompiledJob};
use crate::command::{Command, MetadataCache, CommandOutput};
use crate::graph::{Graph, TransitiveDeps, topological_sort_levels};

use std::thread;
use std::path::Path;
use std::io::{self, Write};

use dashmap::DashSet;
use rayon::prelude::*;
use fxhash::FxBuildHasher;
use crossbeam_channel::{unbounded, Sender};

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct CommandRunner<'a> {
    context: &'a Processed<'a>,
    stdout: Sender::<String>,
    processed: StrDashSet::<'a>,
    metadata_cache: MetadataCache<'a>,
    transitive_deps: TransitiveDeps<'a>
}

impl<'a> CommandRunner<'a> {
    pub fn run(context: &'a Processed, graph: &Graph, transitive_deps: TransitiveDeps<'a>) {
        let levels = topological_sort_levels(graph);

        #[cfg(feature = "dbg")]
        levels.iter().enumerate().for_each(|(i, level)| {
            println!("{i}: {level:?}")
        });

        let (stdout, stdout_recv) = unbounded::<String>();
        let writer = thread::spawn(move || {
            let mut stdout_handle = io::stdout().lock();
            for s in stdout_recv {
                _ = stdout_handle.write_all(s.as_bytes());
                _ = stdout_handle.flush();
            }
        });

        let n = context.jobs.len();
        let cb = Self {
            stdout,
            context,
            processed: DashSet::with_capacity_and_hasher(n, FxBuildHasher::default()),
            metadata_cache: MetadataCache::new(n),
            transitive_deps
        };

        levels.into_iter().for_each(|level| {
            level.into_par_iter().filter_map(|t| context.jobs.get(t)).for_each(|job| {
                cb._resolve_and_run(job)
            });
        });

        drop(cb);
        _ = writer.join()
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
                            _ = self.print(msg)
                        }
                    }
                }
            }

            if !all_deps_resolved { continue }

            if let Some(rule) = self.context.rules.get(job.rule) {
                self.processed.insert(job.target);

                if self.metadata_cache.needs_rebuild(job, &self.transitive_deps) {
                    if let Err(e) = self.create_dirs_if_needed(job.target) {
                        let msg = format!{
                            "could not create build directory for target: {target}: {e}",
                            target = job.target
                        };
                        _ = self.print(msg);
                        return
                    }

                    let Some(command) = rule.command.compile_(job, &self.context.defs).map_err(|e| {
                        _ = self.print(e);
                        rule.description.as_ref().and_then(|d| d.check(&self.context.defs).err()).map(|err| {
                            _ = self.print(err)
                        });
                    }).ok() else {
                        continue
                    };

                    let description = rule.description.as_ref().and_then(|d| d.compile_(&job, &self.context.defs).map_err(|e| {
                        _ = self.print(e)
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
                            _ = self.print(err);
                            continue
                        }
                    };

                    let command = description.unwrap_or(command);
                    let output = format!("{command}\n{stdout}{stderr}");
                    _ = self.print(output);
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
                }
            } else {
                let err = report_fmt!{
                    job.loc,
                    "no rule named: {rule} found for job {target}\n",
                    rule = job.rule,
                    target = job.target
                };
                _ = self.print(err)
            }
        }
    }
}
