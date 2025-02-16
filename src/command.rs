use crate::flags::Flags;
use crate::graph::Graph;
use crate::parser::comp::Job;
use crate::types::StrDashMap;
use crate::consts::CLEAN_TARGET;
use crate::poll::{Poller, Subprocess};

use std::io;
use std::ptr;
use std::mem;
use std::fs::File;
use std::sync::Arc;
use std::path::Path;
use std::ffi::CString;
use std::time::SystemTime;
use std::os::fd::{AsFd, AsRawFd};
use std::sync::atomic::{Ordering};
use std::os::fd::{IntoRawFd, FromRawFd};

use dashmap::DashMap;
use fxhash::FxBuildHasher;
use crossbeam_channel::Sender;
use nix::poll::{PollFd, PollFlags};

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct Command<'a> {
    pub target: &'a str,
    pub command: &'a str,
    pub description: Option::<&'a str>
}

// Custom implementation of `Command` to avoid fork + exec overhead
impl<'a> Command<'a> {
    #[inline]
    pub fn to_string(&self, flags: &Flags) -> String {
        let Command { target, command, description } = self;
        let n = description.as_ref().map_or(0, |d| 1 + d.len() + 1) + command.len() + 1;
        let mut buf = String::with_capacity(n);
        if flags.verbose() {
            if let Some(ref d) = description {
                if *target != CLEAN_TARGET { buf.push('[') }
                buf.push_str(d);
                if *target != CLEAN_TARGET { buf.push(']') }
                buf.push('\n');
            }
            buf.push_str(&command)
        } else if let Some(ref d) = description {
            if *target != CLEAN_TARGET { buf.push('[') }
            buf.push_str(d);
            if *target != CLEAN_TARGET { buf.push(']') }
        } else {
            buf.push_str(&command)
        } buf
    }

    #[inline]
    fn create_pipe() -> io::Result::<(File, File)> {
        let mut fds = [0; 2];
        if unsafe { libc::pipe(fds.as_mut_ptr()) } != 0 {
            return Err(io::Error::last_os_error())
        }
        let r = unsafe { File::from_raw_fd(fds[0]) };
        let w = unsafe { File::from_raw_fd(fds[1]) };
        Ok((r, w))
    }

    pub fn execute(
        &self,
        poller: &Poller,
        poll_fds_sender: Sender::<PollFd>,
    ) -> io::Result::<()> {
        let cmd = CString::new(self.command.as_bytes())?;
        let args = [
            c"/bin/sh".as_ptr(),
            c"-c".as_ptr(),
            cmd.as_ptr(),
            ptr::null(),
        ];

        let (stdout_reader, stdout_writer) = Self::create_pipe()?;
        let (stderr_reader, stderr_writer) = Self::create_pipe()?;

        let stdout_reader_fd = stdout_reader.as_raw_fd();
        let stderr_reader_fd = stderr_reader.as_raw_fd();

        let stdout_writer_fd = stdout_writer.into_raw_fd();
        let stderr_writer_fd = stderr_writer.into_raw_fd();

        let mut file_actions = unsafe { mem::zeroed() };
        unsafe {
            libc::posix_spawn_file_actions_init(&mut file_actions);
            libc::posix_spawn_file_actions_adddup2(&mut file_actions, stdout_writer_fd, libc::STDOUT_FILENO);
            libc::posix_spawn_file_actions_adddup2(&mut file_actions, stderr_writer_fd, libc::STDERR_FILENO);
        }

        let mut attr = unsafe { mem::zeroed() };
        unsafe {
            libc::posix_spawnattr_init(&mut attr);
        }

        let env = [c"PATH=/usr/bin:/bin".as_ptr(), ptr::null()];
        let mut pid = 0;
        let ret = unsafe {
            libc::posix_spawn(
                &mut pid,
                c"/bin/sh".as_ptr(),
                &file_actions,
                &attr,
                args.as_ptr() as *const *mut _,
                env.as_ptr() as *const *mut _,
            )
        };

        if ret != 0 {
            return Err(io::Error::last_os_error())
        }

        unsafe {
            libc::close(stdout_writer_fd);
            libc::close(stderr_writer_fd);
        }

        {
            let target = Box::from(self.target);
            let command = Box::from(self.command);
            let description = self.description.map(Box::from);
            let subprocess = Arc::new(Subprocess {pid, target, command, description});
            
            poller.fd_to_subprocess.insert(stdout_reader_fd, Arc::clone(&subprocess));
            poller.fd_to_subprocess.insert(stderr_reader_fd, subprocess);
        }

        poller.active_fds.fetch_add(1, Ordering::Relaxed);

        // TODO: dont leak here
        let stdout_poll_fd = {
            let stdout_reader_fd: &'static _ = Box::leak(Box::new(stdout_reader));
            PollFd::new(stdout_reader_fd.as_fd(), PollFlags::POLLIN)
        };

        let stderr_poll_fd = {
            let stderr_reader_fd: &'static _ = Box::leak(Box::new(stderr_reader));
            PollFd::new(stderr_reader_fd.as_fd(), PollFlags::POLLIN)
        };

        #[cfg(feature = "dbg_hardcore")] {
            {
                let stdout_fd = stdout_poll_fd.as_fd().as_raw_fd();
                let mut stdout = format!("sending: FD: {stdout_fd:?} ");
                if let Some(revents) = stdout_poll_fd.revents() {
                    let revents = format!("revents: {revents:?}");
                    stdout.push_str(&revents)
                }
                println!("{stdout}");
            }

            {
                let stderr_fd = stderr_poll_fd.as_fd().as_raw_fd();
                let mut stderr = format!("sending: FD: {stderr_fd:?} ");
                if let Some(revents) = stdout_poll_fd.revents() {
                    let revents = format!("revents: {revents:?}");
                    stderr.push_str(&revents)
                }
                println!("{stderr}");
            }
        }

        _ = poll_fds_sender.send(stdout_poll_fd);
        _ = poll_fds_sender.send(stderr_poll_fd);

        Ok(())
    }
}

#[cfg_attr(feature = "dbg", derive(Debug))]
pub struct MetadataCache<'a> {
    files: StrDashMap::<'a, SystemTime>
}

impl<'a> MetadataCache<'a> {
    #[inline]
    pub fn new(files_count: usize) -> Self {
        Self {
            files: DashMap::with_capacity_and_hasher(files_count, FxBuildHasher::default())
        }
    }

    #[inline]
    pub fn mtime(&self, f: &'a str) -> io::Result::<SystemTime> {
        if let Some(mtime) = self.files.get(f) {
            Ok(*mtime)
        } else {
            let p: &Path = f.as_ref();
            let m = p.metadata()?.modified()?;
            self.files.insert(f, m);
            Ok(m)
        }
    }

    #[inline]
    pub fn needs_rebuild(&self, job: &Job<'a>, transitive_deps: &Graph<'a>) -> bool {
        // TODO: do something here if dependent file does not exist
        let mtimes = unsafe {
            transitive_deps.get(job.target).unwrap_unchecked()
        }.iter().filter_map(|dep| {
            self.mtime(*dep).ok()
        }).collect::<Vec::<_>>();

        let Ok(target_mtime) = self.mtime(job.target) else {
            return true
        };

        mtimes.into_iter().any(|src_mtime| src_mtime > target_mtime)
    }
}
