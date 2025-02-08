use crate::flags::Flags;
use crate::types::StrDashSet;
use crate::db::{Db, Metadata};
use crate::consts::PHONY_TARGETS;
use crate::parser::comp::{Job, Phony};
use crate::parser::{Rule, Compiled, DefaultJob};
use crate::graph::{Graph, Levels, topological_sort};
use crate::command::{Command, MetadataCache, CommandOutput};

use std::sync::Arc;
use std::path::Path;
use std::io::{self, Write};
use std::hash::{Hash, Hasher};
use std::thread::{self, JoinHandle};
use std::sync::atomic::{Ordering, AtomicU64, AtomicBool, AtomicUsize};

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

#[derive(PartialEq)]
#[cfg_attr(feature = "dbg", derive(Debug))]
enum ExecutorFlow { Ok, Stop }

#[cfg_attr(feature = "dbg", derive(Debug))]
struct AtomicStr { ptr: AtomicU64, len: AtomicU64 }

impl AtomicStr {
    #[inline(always)]
    fn empty() -> Self {
        Self { ptr: AtomicU64::new(0), len: AtomicU64::new(0) }
    }

    #[inline(always)]
    fn store(&self, s: &str, ordering: Ordering) {
        self.ptr.store(s.as_ptr() as u64, ordering);
        self.len.store(s.len() as u64, ordering);
    }

    #[inline]
    fn load(&self, ordering: Ordering) -> Option::<&str> {
        let ptr = self.ptr.load(ordering);
        if ptr == 0 { return None }
        let len = self.len.load(ordering);
        let s = unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(ptr as *const _, len as _))
        };
        Some(s)
    }
}

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct CommandRunner<'a> {
    context: &'a Compiled<'a>,

    graph: Graph<'a>,
    transitive_deps: Graph<'a>,

    flags: &'a Flags,

    stdout: Stdout,
    stdout_thread: StdoutThread,

    stop: AtomicBool,

    fail_count: AtomicUsize,
    not_up_to_date: AtomicStr,

    db_read: Option::<Db<'a>>,
    db_write: Db<'a>,

    default_job: DefaultJob<'a>,

    executed: StrDashSet<'a>,

    metadata_cache: MetadataCache<'a>,
}

impl<'a> CommandRunner<'a> {
    #[inline]
    fn run_levels(&self, levels: &Levels<'a>) {
        for level in levels.into_iter() {
            if level.into_par_iter().filter_map(|t| self.context.jobs.get(t)).try_for_each(|job| {
                self.resolve_and_run(job);
                if self.stop.load(Ordering::Relaxed) { Err(()) } else { Ok(()) }
            }).is_err() {
                cr_print!(self, "[execution failed]\n");
                break
            }
        }
    }

    #[inline]
    fn stdout_thread() -> (Stdout, StdoutThread) {
        let (stdout, stdout_recv) = unbounded::<String>();
        let writer = thread::spawn(move || {
            let mut stdout_handle = io::stdout().lock();
            for s in stdout_recv {
                _ = stdout_handle.write_all(s.as_bytes())
            }
            _ = stdout_handle.flush();
        }); (stdout, writer)
    }

    #[inline]
    fn new(
        context: &'a Compiled,
        graph: Graph<'a>,
        transitive_deps: Graph<'a>,
        flags: &'a Flags,
        db_read: Option::<Db<'a>>,
        default_job: DefaultJob<'a>,
    ) -> Self {
        let n = context.jobs.len();
        let (stdout, stdout_thread) = Self::stdout_thread();
        Self {
            flags,
            graph,
            stdout,
            context,
            db_read,
            default_job,
            stdout_thread,
            transitive_deps,
            db_write: Db::write(),
            stop: AtomicBool::new(false),
            fail_count: AtomicUsize::new(0),
            not_up_to_date: AtomicStr::empty(),
            metadata_cache: MetadataCache::new(n),
            executed: DashSet::with_capacity_and_hasher(n, FxBuildHasher::default()),
        }
    }

    #[inline]
    pub fn run(
        context: &'a Compiled,
        graph: Graph<'a>,
        transitive_deps: Graph<'a>,
        flags: &'a Flags,
        db_read: Option::<Db<'a>>,
        default_job: DefaultJob<'a>,
    ) -> Db<'a> {
        let cr = Self::new(context, graph, transitive_deps, flags, db_read, default_job);

        let levels = if let Some(job) = default_job {
            cr.run_target(job);
            return cr.finish()
        } else {
            topological_sort(&cr.graph)
        };

        _ = cr.run_levels(&levels);
        cr.finish()
    }

    #[inline]
    fn finish(self) -> Db<'a> {
        if self.flags.check_is_up_to_date() {
            let not_up_to_date = self.not_up_to_date.load(Ordering::Relaxed);
            match not_up_to_date {
                None => cr_print!(self, "[up to date]\n"),
                Some(target) => cr_print!(self, "[{target} is not up to date]\n")
            }
        }
        drop(self.stdout);
        _ = self.stdout_thread.join();
        self.db_write
    }

    #[inline]
    fn job_failed(&self) -> bool {
        self.flags.max_fail_count().map_or(false, |max| {
            self.fail_count.fetch_add(1, Ordering::Relaxed);
            if self.fail_count.load(Ordering::Relaxed) >= *max {
                self.stop.store(true, Ordering::Relaxed);
                true
            } else {
                false
            }
        })
    }

    #[inline]
    fn execute_command(&self, command: Command, target: &str) -> io::Result::<(CommandOutput, bool)> {
        if self.flags.print_commands() {
            return Ok((command.print_command(), false))
        }

        command.execute().map_err(|e| {
            cr_print!(self, "[could not execute job: {target}: {e}]\n");
            e
        }).map(|command_output| {
            let failed = if command_output.status != 0 {
                self.job_failed()
            } else {
                false
            }; (command_output, failed)
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

                if let Some(Ok((command_output, _))) = {
                    command.as_ref().map(|c| {
                        let command = Command { command: c.to_owned(), description: None };  
                        self.execute_command(command, job.target)
                    })
                } {
                    cr_print!{
                        self,
                        "{output}",
                        output = command_output.to_string(&self.flags)
                    };
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
            topological_sort(&subgraph)
        };

        _ = self.run_levels(&levels)
    }

    #[inline]
    fn run_target(&self, job: &Job<'a>) {
        if PHONY_TARGETS.contains(&job.target) {
            self.run_phony(job);
            return
        }

        _ = self.resolve_and_run_target(job)
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
            _ = self.stdout.send(s)
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
        if self.flags.always_build() { return true }
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

    fn execute_job(&self, job: &Job<'a>, rule: &Rule) -> ExecutorFlow {
        let Some(command) = self.compile_command(job, rule) else {
            return ExecutorFlow::Ok
        };

        if self.needs_rebuild(job, &command) {
            if self.flags.check_is_up_to_date() {
                self.not_up_to_date.store(job.target, Ordering::Relaxed);
                return ExecutorFlow::Stop
            }

            if let Err(e) = self.create_dirs_if_needed(job.target) {
                cr_print!{
                    self,
                    "[could not create build directory for target: {target}: {e}]\n",
                    target = job.target
                };
                return ExecutorFlow::Ok
            }

            let description = rule.description.as_ref().and_then(|d| d.compile(&job, &self.context.defs).map_err(|e| {
                cr_print!(self, "{e}\n");
            }).ok());

            let command = Command { command, description };
            let Ok((command_output, failed)) = self.execute_command(command, job.target) else {
                return ExecutorFlow::Ok
            };

            let command_output = command_output.to_string(&self.flags);
            cr_print!(self, "{command_output}");

            return if failed { ExecutorFlow::Stop } else { ExecutorFlow::Ok }
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

            if !any_err && self.flags.verbose() ||
                self.default_job.as_ref().map_or(false, |def| {
                    def.target == job.target || def.aliases().map_or(false, |aliases| {
                        aliases.iter().any(|a| a == job.target)
                    })
                }) && !self.flags.check_is_up_to_date()
            {
                cr_print!(self, "[{target} is already built]\n", target = job.target)
            }
        } ExecutorFlow::Ok
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
                            cr_print!(self, "[dependency {input} is assumed to exist]\n");
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
