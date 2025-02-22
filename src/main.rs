#[macro_use]
mod loc;

mod ux;
mod cr;
mod db;
mod poll;
mod flags;
mod types;
mod graph;
mod parser;
mod consts;
mod command;
mod template;
mod dbg_unwrap;
mod edit_distance;

use db::Db;
use flags::Flags;
use cr::CommandRunner;
use graph::build_dependency_graph;
use parser::{comp, Parser, read_file};
use ux::{check_args, did_you_mean_compiled};

use std::env;
use std::process::{exit, ExitCode};

use bumpalo::Bump;
use flager::Parser as FlagParser;

fn main() -> ExitCode {
    let args = env::args().collect::<Vec::<_>>();
    if let Some(undefined_flag) = check_args(&args) {
        eprintln!{
            "did you mean: {program} -t {undefined_flag} [..flags]?",
            program = args[0],
        };
        return ExitCode::FAILURE
    }

    let flag_parser = FlagParser::new();
    let flags = Flags::new(&flag_parser);

    if flags.help() {
        Flags::print_help();
        return ExitCode::SUCCESS
    }

    if let Some(cd) = flags.change_dir() {
        if let Err(e) = env::set_current_dir(cd) {
            eprintln!("[could not enter {cd:?}]: {e}");
            return ExitCode::FAILURE
        } else {
            println!("[changed directory to {cd:?}]")
        }
    }

    let rush_file_path = flags.file_path()
        .map(|s| s.as_str())
        .map(|s| s.strip_prefix("./").unwrap_or(s))
        .unwrap_or(Parser::RUSH_FILE_PATH);

    unsafe {
        loc::RUSH_FILE_PATH_PTR = rush_file_path.as_ptr();
        loc::RUSH_FILE_PATH_LEN = rush_file_path.len();
    }

    let Some(mmap) = read_file(&rush_file_path) else {
        eprintln!("could not read: {rush_file_path}");
        return ExitCode::FAILURE
    };

    let content = unsafe { std::str::from_utf8_unchecked(&mmap[..]) };
    let (content, escaped_indexes) = Parser::preprocess_content(content);
    let context = Parser::parse(&content, &escaped_indexes);
    let reserve = context.guess_preallocation();
    #[cfg(feature = "dbg")] {
        println!("guessed size: {reserve}")
    }

    let arena = Bump::with_capacity(reserve);
    let context = context.compile(&arena);

    #[cfg(feature = "dbg")] {
        println!("real size: {size}", size = arena.allocated_bytes())
    }

    if flags.list_jobs() {
        let edges = context.pretty_print_targets();
        println!("available jobs: [{edges}]");
        return ExitCode::SUCCESS
    }

    if flags.list_rules() {
        let rules = context.pretty_print_rules();
        println!("available rules: [{rules}]");
        return ExitCode::SUCCESS
    }

    if flags.list_jobs_and_rules() {
        let edges = context.pretty_print_targets();
        println!("available jobs: [{edges}]");

        let rules = context.pretty_print_rules();
        println!("available rules: [{rules}]");
        return ExitCode::SUCCESS
    }

    let clean = context.generate_clean_edge(&arena, &flags);

    let default_edge = flags.default_target().map(|t| {
        context.edges.get(t.as_str()).unwrap_or_else(|| {
            let targets = context.pretty_print_targets();
            eprintln!("no target: {t} found in {rush_file_path}");
            if let Some(compiled) = did_you_mean_compiled(
                t,
                &context.edges,
                &context.rules,
            ) {
                println!("did you mean: {compiled}?")
            }
            eprintln!("available targets: [{targets}]");
            exit(1)
        })
    });

    let mmap = Db::read_cache(&context.cache_file_path);
    let content = mmap.as_ref().map(|mmap| unsafe {
        std::str::from_utf8_unchecked(&mmap[..])
    });
    let db = content.and_then(|content| Db::read(content).ok());

    let (graph, default_edge, transitive_deps) = build_dependency_graph(
        &arena,
        &context,
        default_edge
    );

    if flags.print_default_job() {
        if let Some(comp::Edge { target, .. }) = default_edge.as_ref() {
            println!("default job: {target}");
            return ExitCode::SUCCESS
        } else {
            println!("no default job");
            return ExitCode::SUCCESS
        }
    }

    _ = CommandRunner::run(
        clean,
        &arena,
        &context,
        graph,
        transitive_deps,
        flags,
        db,
        default_edge
    ).write_finish(&context.cache_file_path);

    ExitCode::SUCCESS
}
