#[macro_use]
mod loc;

mod cr;
mod db;
mod flags;
mod types;
mod graph;
mod parser;
mod consts;
mod command;
mod template;

use db::Db;
use flags::Flags;
use cr::CommandRunner;
use parser::{Parser, read_file};
use graph::build_dependency_graph;

use std::process::{exit, ExitCode};

use bumpalo::Bump;
use flager::Parser as FlagParser;

fn main() -> ExitCode {
    let flag_parser = FlagParser::new();
    let flags = Flags::new(&flag_parser);

    if flags.help() {
        Flags::print_help();
        return ExitCode::SUCCESS
    }

    let rush_file_path = flags.file_path().map(|s| s.as_str()).unwrap_or(Parser::RUSH_FILE_PATH);

    unsafe {
        loc::RUSH_FILE_PATH_PTR = rush_file_path.as_ptr();
        loc::RUSH_FILE_PATH_LEN = rush_file_path.len();
    }

    let Some(mmap) = read_file(&rush_file_path) else {
        eprintln!("could not read: {rush_file_path}");
        return ExitCode::FAILURE
    };

    // /* Handle custom parallelalism */ {
    //     if let Some(&parall) = flags.parallelalism() {
    //         let max = available_parallelism().unwrap().get() as _;
    //         if parall < 0 || parall > max {
    //             eprintln!("invalid amount of the maximum parallel jobs: {parall}");
    //             return ExitCode::FAILURE
    //         }
    //         if let Err(e) = ThreadPoolBuilder::new().num_threads(parall as _).build_global() {
    //             eprintln!("could not initialize thread pool: {e}");
    //             return ExitCode::FAILURE
    //         }
    //     }
    // }

    let content = unsafe { std::str::from_utf8_unchecked(&mmap[..]) };
    let (escaped, escaped_indexes) = Parser::handle_newline_escapes(content);
    let context = Parser::parse(&escaped, &escaped_indexes);
    let reserve = context.guess_preallocation();

    #[cfg(feature = "dbg")] {
        println!("guessed size: {reserve}")
    }

    let arena = Bump::with_capacity(reserve);
    let context = context.compile(&arena);

    #[cfg(feature = "dbg")] {
        println!("real size: {size}", size = arena.allocated_bytes())
    }

    let default_job = flags.default_target().map(|t| {
        context.jobs.get(t.as_str()).unwrap_or_else(|| {
            let targets = context.pretty_print_targets();
            eprintln!("no target: {t} found in {rush_file_path}");
            eprintln!("available targets: [{targets}]");
            exit(1)
        })
    });

    let mmap = Db::read_cache();
    let content = mmap.as_ref().map(|mmap| unsafe { std::str::from_utf8_unchecked(&mmap[..]) });
    let db = content.and_then(|content| Db::read(content).ok());

    let (graph, default_job, transitive_deps) = build_dependency_graph(&context, default_job);
    _ = CommandRunner::run(&context, graph, transitive_deps, &flags, db, default_job).write_finish();

    ExitCode::SUCCESS
}
