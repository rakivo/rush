#![allow(clippy::collapsible_if)]

#[macro_use]
mod loc;

mod cli;
mod command;
mod consts;
mod cr;
mod db;
mod dbg_unwrap;
mod graph;
mod parser;
mod poll;
mod template;
mod types;
mod util;
mod ux;

use cli::Cli;
use cr::CommandRunner;
use db::Db;
use graph::build_dependency_graph;
use parser::{comp, read_file, Parser};

use std::env;
use std::process::ExitCode;

use bumpalo::Bump;
use clap::Parser as _;

fn main() -> ExitCode {
    let flags = Cli::parse();

    if let Some(cd) = &flags.change_dir {
        if let Err(e) = env::set_current_dir(cd) {
            eprintln!("[could not enter {cd:?}]: {e}");
            return ExitCode::FAILURE;
        } else {
            println!("[changed directory to {cd:?}]")
        }
    }

    let rush_file_path = flags
        .file_path
        .as_ref()
        .map(|s| s.strip_prefix("./").unwrap_or(s).trim())
        .unwrap_or(Parser::RUSH_FILE_PATH)
        .to_owned();

    let Some(mmap) = read_file(&rush_file_path) else {
        eprintln!("could not read: {rush_file_path}");
        return ExitCode::FAILURE;
    };

    let s = unsafe { std::str::from_utf8_unchecked(&mmap[..]) };
    let mut pp = String::with_capacity(s.len());
    let escaped_indexes = Parser::preprocess_content(s, &mut pp);

    let arena_cap = (pp.len() as f64 * 2.5) as _;

    let arena = Bump::with_capacity(arena_cap);
    let context = Parser::parse(&arena, &pp, &rush_file_path, &escaped_indexes);

    let context = context.compile(&arena);

    #[cfg(feature = "dbg")]
    {
        println!("real size: {size}", size = arena.allocated_bytes())
    }

    if flags.list_jobs {
        let edges = context.pretty_print_targets();
        println!("available jobs: [{edges}]");
        return ExitCode::SUCCESS;
    }

    if flags.list_rules {
        let rules = context.pretty_print_rules();
        println!("available rules: [{rules}]");
        return ExitCode::SUCCESS;
    }

    if flags.list_jobs_and_rules {
        let edges = context.pretty_print_targets();
        println!("available jobs: [{edges}]");

        let rules = context.pretty_print_rules();
        println!("available rules: [{rules}]");
        return ExitCode::SUCCESS;
    }

    let default_edge = flags.default_target.as_ref().map(|t| {
        context
            .edges
            .get(t.as_str())
            .unwrap_or_else(|| util::report_undefined_target(t, None, &context))
    });

    let mmap = Db::read_cache(&context.cache_file_path);
    let content = mmap
        .as_ref()
        .map(|mmap| unsafe { std::str::from_utf8_unchecked(&mmap[..]) });
    let db = content.and_then(|content| Db::read(content).ok());

    let (graph, default_edge, transitive_deps) =
        build_dependency_graph(&arena, &context, default_edge);

    if flags.print_default_job {
        if let Some(comp::Edge { target, .. }) = default_edge.as_ref() {
            println!("default job: {target}");
            return ExitCode::SUCCESS;
        } else {
            println!("no default job");
            return ExitCode::SUCCESS;
        }
    }

    _ = CommandRunner::run(&context, graph, transitive_deps, flags, db, default_edge)
        .write_finish(&context.cache_file_path);

    ExitCode::SUCCESS
}
