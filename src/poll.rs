use crate::flags::Flags;
use crate::command::Command;
use crate::dbg_unwrap::DbgUnwrap;

use std::sync::Arc;
use std::time::Duration;
use std::collections::HashSet;
use std::os::fd::{AsFd, AsRawFd};
use std::thread::{self, JoinHandle};
use std::sync::atomic::{Ordering, AtomicBool, AtomicUsize};

use dashmap::DashMap;
use nix::poll::{poll, PollFd, PollFlags};
use crossbeam_channel::{unbounded, Receiver, Sender};

pub type PollingThread = JoinHandle::<()>;
pub type FdSender = Sender::<PollFd<'static>>;

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Subprocess {
    pub pid: libc::pid_t,
    pub target: Box::<str>,
    pub command: Box::<str>,
    pub description: Option::<Box::<str>>,
}

pub type SubprocessMap = DashMap::<i32, Arc::<Subprocess>>;

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Poller {
    pub flags: Arc::<Flags>,
    pub stop: Arc::<AtomicBool>,
    pub jobs_done: Arc::<AtomicUsize>,
    pub active_fds: Arc::<AtomicUsize>,
    pub curr_subprocess_id: AtomicUsize,
    pub poll_fd_recv: Receiver::<PollFd<'static>>,
    pub fd_to_subprocess: Arc::<SubprocessMap>,
}

impl Poller {
    #[inline]
    pub fn spawn(
        flags: Arc::<Flags>,
        stop: Arc::<AtomicBool>,
        jobs_done: Arc::<AtomicUsize>,
        active_fds: Arc::<AtomicUsize>,
        fd_to_subprocess: Arc::<SubprocessMap>,
    ) -> (Arc::<Self>, FdSender, PollingThread) {
        let (fd_sender, poll_fd_recv) = unbounded();
        let poller = Self {
            flags,
            stop,
            jobs_done,
            active_fds,
            poll_fd_recv,
            fd_to_subprocess,
            curr_subprocess_id: AtomicUsize::new(0)
        };
        let poller = Arc::new(poller);
        let thread = thread::spawn({
            let poller = Arc::clone(&poller);
            move || poller.start_polling()
        });
        (poller, fd_sender, thread)
    }

    #[inline(always)]
    fn job_is_done(&self) {
        _ = self.jobs_done.fetch_add(1, Ordering::Relaxed)
    }

    #[inline]
    fn get_process_exit_code(pid: libc::pid_t) -> Option::<i32> {
        let mut status = 0;
        unsafe {
            let ret = libc::waitpid(pid, &mut status, 0);
            if ret == -1 { return None }
            if libc::WIFEXITED(status) {
                Some(libc::WEXITSTATUS(status))
            } else {
                None
            }
        }
    }

    fn start_polling(&self) {
        let mut jobs_failed = 0;
        let mut poll_fds = Vec::new();
        let mut handled_pids = HashSet::new();
        let mut printed_pids = HashSet::new();

        let print_job = |target: &str, command: &Box::<str>, description: &Option::<Box::<str>>| {
            if !self.flags.quiet() {
                let output = Command {
                    target,
                    command: &command,
                    description: description.as_deref()
                }.to_string(&self.flags);
                println!("{output}");
            }
        };

        loop {
            if self.stop.load(Ordering::Relaxed) && self.active_fds.load(Ordering::Relaxed) == 0 {
                break
            }

            while let Ok(poll_fd) = self.poll_fd_recv.try_recv() {
                poll_fds.push(poll_fd);
            }

            if poll_fds.is_empty() {
                thread::sleep(Duration::from_millis(25));
                continue
            }

            if let Err(e) = poll(&mut poll_fds, 15u8) {
                _ = println!("poll failed: {e}");
                break
            }

            let mut i = 0;
            'outer: while i < poll_fds.len() {
                if let Some(revents) = poll_fds[i].revents() {
                    let fd = poll_fds[i].as_fd().as_raw_fd();

                    #[cfg(feature = "dbg_hardcore")] {
                        println!("polled: FD: {}, revents={:?}", fd, revents);
                    }

                    if revents.contains(PollFlags::POLLIN) {
                        let mut buf = [0; 4096];
                        let n = unsafe { libc::read(fd, buf.as_mut_ptr() as *mut _, buf.len()) };
                        if n > 0 {
                            let Ok(data) = std::str::from_utf8(&buf[..n as usize]) else {
                                println!("command output is not valid utf8");
                                continue
                            };
                            {
                                let subprocess = self.fd_to_subprocess.get(&fd).unwrap_dbg();
                                let Subprocess { pid, target, command, description } = subprocess.value().as_ref();
                                if !printed_pids.contains(pid) {
                                    print_job(target, command, description);
                                    _ = printed_pids.insert(*pid)
                                }
                            }
                            print!("{data}")
                        }
                    } else if revents.contains(PollFlags::POLLHUP) {
                        /* drop reference into `fd_to_subprocess` before calling `fd_to_subprocess.remove` */ {
                            let subprocess = self.fd_to_subprocess.get(&fd).unwrap_dbg();
                            let Subprocess { pid, target, command, description } = subprocess.value().as_ref();

                            if !printed_pids.contains(pid) {
                                print_job(target, command, description);
                                _ = printed_pids.insert(*pid)
                            }

                            if self.flags.rush() && !handled_pids.contains(pid) {
                                self.job_is_done()
                            }

                            if !handled_pids.contains(pid) {
                                if !self.flags.rush() {
                                    // wait on process only if `-k` flag is specified to achieve maximum speed
                                    let exit_code = Self::get_process_exit_code(*pid);
                                    if exit_code.map_or(false, |code| code != 0) {
                                        jobs_failed += 1;
                                        if jobs_failed >= *self.flags.max_fail_count().unwrap_or(&1) {
                                            self.stop.store(true, Ordering::Relaxed);
                                            break 'outer
                                        }
                                    } self.job_is_done()
                                }
                                _ = handled_pids.insert(*pid)
                            }
                        }

                        poll_fds.remove(i);
                        self.fd_to_subprocess.remove(&fd);

                        if self.active_fds.load(Ordering::Relaxed) > 0 {
                            _ = self.active_fds.fetch_sub(1, Ordering::Relaxed)
                        } continue
                    }
                } i += 1
            }
        }
    }
}
