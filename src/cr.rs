use crate::flags::Flags;
use crate::types::StrHashSet;
use crate::db::{Db, Metadata};
use crate::consts::PHONY_TARGETS;
use crate::parser::comp::{Job, Phony};
use crate::command::{Command, MetadataCache};
use crate::parser::{Rule, Compiled, DefaultJob};
use crate::graph::{Graph, Levels, topological_sort};

use std::io;
use std::sync::Arc;
use std::path::Path;
use std::hash::{Hash, Hasher};
use std::collections::HashSet;
use std::os::fd::{AsFd, AsRawFd};
use std::thread::{self, JoinHandle};
use std::sync::atomic::{Ordering, AtomicBool, AtomicUsize};
use std::time::Duration;

use dashmap::DashMap;
use crossbeam_channel::{Sender, unbounded, TryRecvError};
use nix::poll::{poll, PollFd, PollFlags, PollTimeout};

pub type Stdout = Sender::<String>;
type StdoutThread = JoinHandle::<()>;
type FdSender = Sender::<PollFd<'static>>;

#[derive(PartialEq)]
#[cfg_attr(feature = "dbg", derive(Debug))]
enum ExecutorFlow { Ok, Stop }

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Subprocess {
    pub id: usize,
    pub command: String,
}

pub type SubprocessMap = DashMap::<i32, Subprocess>;

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct CommandRunner<'a> {
    context: &'a Compiled<'a>,

    graph: Graph<'a>,
    transitive_deps: Graph<'a>,

    flags: &'a Flags,

    stdout: Stdout,
    stdout_thread: StdoutThread,

    fd_sender: FdSender,
    active_fds: Arc::<AtomicUsize>,
    fd_to_subprocess: Arc::<SubprocessMap>,

    stop: Arc::<AtomicBool>,
    jobs_done: Arc::<AtomicUsize>,

    executed_jobs: usize,
    curr_subprocess_id: usize,

    fail_count: usize,
    not_up_to_date: Option::<&'a str>,

    db_read: Option::<Db<'a>>,
    db_write: Db<'a>,

    default_job: DefaultJob<'a>,

    executed: StrHashSet<'a>,

    metadata_cache: MetadataCache<'a>,
}

impl<'a> CommandRunner<'a> {
    #[inline]
    fn run_levels(&mut self, levels: &Levels<'a>) {
        'outer: for level in levels.into_iter() {
            #[cfg(feature = "dbg")] {
                print!("RUNNING LEVEL: {level:#?}\n")
            }

            {
                self.jobs_done.store(0, Ordering::Relaxed);
                self.executed_jobs = 0
            }

            for job in level.into_iter().filter_map(|t| self.context.jobs.get(t)) {
                self.resolve_and_run(job);
                if self.stop.load(Ordering::Relaxed) { break 'outer }
            }

            let executed = self.executed_jobs;
            loop {
                let jobs_done = self.jobs_done.load(Ordering::Relaxed);
                if jobs_done >= executed {
                    break
                } else {
                    #[cfg(feature = "dbg")] {
                        println!("{jobs_done} < {executed}")
                    }
                    thread::sleep(Duration::from_millis(50))
                }
            }

            {
                self.executed_jobs = 0;
                self.jobs_done.store(0, Ordering::Relaxed)
            }
        }
    }

    fn stdout_thread(
        stop: Arc<AtomicBool>,
        jobs_done: Arc::<AtomicUsize>,
        active_fds: Arc<AtomicUsize>,
        fd_to_subprocess: Arc<SubprocessMap>,
    ) -> (FdSender, Stdout, StdoutThread) {
        let (stdout, stdout_recv) = unbounded::<String>();
        let (fd_sender, poll_fd_recv) = unbounded::<PollFd>();
        let writer = thread::spawn(move || {
            let mut poll_fds = Vec::new();
            let mut seen_fds = HashSet::new();
            let mut stdout_recv_dropped = false;
            let mut fds_with_output = HashSet::new();

            let mut hung_ids = HashSet::<usize>::new();
            let mut printed_ids = HashSet::<usize>::new();
            loop {
                // Process messages from the main thread
                match stdout_recv.try_recv() {
                    Ok(s) => print!("{s}"),
                    Err(TryRecvError::Disconnected) => stdout_recv_dropped = true,
                    _ => {}
                }

                if stdout_recv_dropped && stop.load(Ordering::Relaxed) && active_fds.load(Ordering::Relaxed) == 0 {
                    break
                }

                while let Ok(poll_fd) = poll_fd_recv.try_recv() {
                    let fd = poll_fd.as_fd().as_raw_fd();
                    if !seen_fds.contains(&fd) {
                        poll_fds.push(poll_fd);
                        seen_fds.insert(fd);
                    }
                }

                if poll_fds.is_empty() {
                    continue
                }

                if let Err(e) = poll(&mut poll_fds, PollTimeout::MAX) {
                    _ = print!("poll failed: {e}\n");
                    break
                }

                let mut i = 0;
                while i < poll_fds.len() {
                    if let Some(revents) = poll_fds[i].revents() {
                        let fd = poll_fds[i].as_fd().as_raw_fd();

                        #[cfg(feature = "dbg_hardcore")] {
                            print!("polled: FD: {}, revents={:?}\n", fd, revents);
                        }

                        if revents.contains(PollFlags::POLLIN) {
                            fds_with_output.insert(fd);

                            let mut buf = [0; 4096];
                            let n = unsafe { libc::read(fd, buf.as_mut_ptr() as *mut _, buf.len()) };
                            if n > 0 {
                                let subprocess = fd_to_subprocess.get(&fd).unwrap();
                                let Subprocess { id, command } = subprocess.value();
                                let Ok(data) = std::str::from_utf8(&buf[..n as usize]) else {
                                    print!("command output is not valid utf8\n");
                                    continue
                                };
                                if !printed_ids.contains(&id) {
                                    // TODO: print command before waiting till its end
                                    print!("{command}\n");
                                    _ = printed_ids.insert(*id)
                                }
                                print!("{data}");
                            }
                        } else if revents.contains(PollFlags::POLLHUP) {
                            /* drop reference into `fd_to_subprocess` before calling `fd_to_subprocess.remove` */ {
                                let subprocess = fd_to_subprocess.get(&fd).unwrap();
                                let Subprocess { id, command } = subprocess.value();
                                if !hung_ids.contains(id) {
                                    jobs_done.fetch_add(1, Ordering::Relaxed);
                                    _ = hung_ids.insert(*id)
                                }

                                if !fds_with_output.contains(&fd) && !printed_ids.contains(id) {
                                    printed_ids.insert(*id);
                                    print!("{command}\n")
                                }
                            }

                            poll_fds.remove(i);
                            fd_to_subprocess.remove(&fd);

                            if active_fds.load(Ordering::Relaxed) > 0 {
                                _ = active_fds.fetch_sub(1, Ordering::Relaxed)
                            } continue
                        }
                    } i += 1
                }
            }
        }); (fd_sender, stdout, writer)
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
        let stop = Arc::new(AtomicBool::new(false));
        let jobs_done = Arc::new(AtomicUsize::new(0));
        let active_fds = Arc::new(AtomicUsize::new(0));
        let fd_to_subprocess = Arc::new(DashMap::new());
        let (fd_sender, stdout, stdout_thread) = Self::stdout_thread(
            Arc::clone(&stop),
            Arc::clone(&jobs_done),
            Arc::clone(&active_fds),
            Arc::clone(&fd_to_subprocess),
        );
        Self {
            stop,
            flags,
            graph,
            stdout,
            context,
            db_read,
            jobs_done,
            fd_sender,
            active_fds,
            default_job,
            stdout_thread,
            transitive_deps,
            fd_to_subprocess,

            fail_count: 0,
            executed_jobs: 0,
            db_write: Db::write(),
            not_up_to_date: None,
            curr_subprocess_id: 0,
            executed: StrHashSet::with_capacity(n),
            metadata_cache: MetadataCache::new(n),
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
        let mut cr = Self::new(context, graph, transitive_deps, flags, db_read, default_job);

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
            match self.not_up_to_date {
                None => print!("[up to date]\n"),
                Some(target) => print!("[{target} is not up to date]\n")
            }
        }
        self.stop.store(true, Ordering::Relaxed);
        drop(self.stdout);
        _ = self.stdout_thread.join();
        self.db_write
    }

    #[inline]
    fn job_failed(&mut self) -> bool {
        self.flags.max_fail_count().map_or(false, |max| {
            self.fail_count += 1;
            if self.fail_count >= *max {
                self.stop.store(true, Ordering::Relaxed);
                true
            } else {
                false
            }
        })
    }

    #[inline]
    fn execute_command(&mut self, command: &Command, target: &str) -> io::Result::<()> {
        if self.flags.print_commands() {
            return Ok(())
        }

        self.executed_jobs += 1;

        command.execute(
            &mut self.curr_subprocess_id,
            #[cfg(feature = "dbg")] self.stdout.clone(),
            self.fd_sender.clone(),
            &self.fd_to_subprocess,
            &self.active_fds
        ).map_err(|e| {
            print!("[could not execute job: {target}: {e}]\n");
            e
        })
    }

    fn run_phony(&mut self, job: &Job<'a>) {
        match &job.phony {
            Phony::Phony { command, aliases, .. } => {
                aliases.iter().filter_map(|_job| {
                    match self.context.jobs.get(_job.as_str()) {
                        Some(j) => Some(j),
                        None => {
                            report_panic!{
                                job.loc,
                                "undefined job: {target}\nNOTE: in phony jobs you can only alias jobs\n",
                                target = _job
                            };
                        }
                    }
                }).for_each(|job| self.resolve_and_run_target(job));

                _ = command.as_ref().map(|c| {
                    let command = Command { command: c.to_owned(), description: None };  
                    self.execute_command(&command, job.target)
                });
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
    fn resolve_and_run_target(&mut self, job: &Job<'a>) {
        let levels = {
            let subgraph = self.build_subgraph(job.target);
            topological_sort(&subgraph)
        };

        _ = self.run_levels(&levels)
    }

    #[inline]
    fn run_target(&mut self, job: &Job<'a>) {
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
            print!("{e}\n");
            rule.description.as_ref().and_then(|d| d.check(&job.shadows, &self.context.defs).err()).map(|err| {
                print!("{err}\n");
            });
        }).map(|command| {
            self.db_write.metadata_write(job.target, Metadata {
                command_hash: Self::hash(&command)
            }); command
        }).ok()
    }

    fn execute_job(&mut self, job: &Job<'a>, rule: &Rule) -> ExecutorFlow {
        let Some(command) = self.compile_command(job, rule) else {
            return ExecutorFlow::Ok
        };

        if self.needs_rebuild(job, &command) {
            if self.flags.check_is_up_to_date() {
                self.not_up_to_date = Some(job.target);
                return ExecutorFlow::Stop
            }

            if let Err(e) = self.create_dirs_if_needed(job.target) {
                print!{                    "[could not create build directory for target: {target}: {e}]\n",
                    target = job.target
                };
                return ExecutorFlow::Ok
            }

            let description = rule.description.as_ref().and_then(|d| d.compile(&job, &self.context.defs).map_err(|e| {
                print!("{e}\n");
            }).ok());

            let command = Command { command, description };
            _ = self.execute_command(&command, job.target);
        } else {
            let mut any_err = false;
            if let Err(err) = rule.command.check(&job.shadows, &self.context.defs) {
                print!("{err}\n");
                any_err = true
            }

            if let Some(Err(err)) = rule.description.as_ref().map(|d| d.check(&job.shadows, &self.context.defs)) {
                print!("{err}\n");
                any_err = true
            }

            if !any_err && self.flags.verbose() ||
                self.default_job.as_ref().map_or(false, |def| {
                    def.target == job.target || def.aliases().map_or(false, |aliases| {
                        aliases.iter().any(|a| a == job.target)
                    })
                }) && !self.flags.check_is_up_to_date()
            {
                print!("[{target} is already built]\n", target = job.target)
            }
        } ExecutorFlow::Ok
    }

    fn resolve_and_run(&mut self, job: &Job<'a>) {
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
                            print!("[dependency {input} is assumed to exist]\n");
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
