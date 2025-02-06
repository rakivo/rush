#[macro_use]
mod loc;

mod cr;
mod db;
mod mode;
mod types;
mod graph;
mod parser;
mod consts;
mod command;
mod template;

use db::Db;
use mode::Mode;
use cr::CommandRunner;
use rayon::ThreadPoolBuilder;
use parser::{Parser, read_file};
use graph::build_dependency_graph;
use flager::{Flag, Parser as FlagParser, new_flag};

use std::process::ExitCode;
use std::thread::available_parallelism;

const CUSTOM_FILE_PATH: Flag::<String> = new_flag!("-f", "--file");
const CUSTOM_PARALLELALISM: Flag::<i64> = new_flag!("-j", "--jobs");

fn main() -> ExitCode {
    let flag_parser = FlagParser::new();
    let rush_file_path = flag_parser.parse(&CUSTOM_FILE_PATH)
        .unwrap_or(Parser::RUSH_FILE_PATH.to_owned());

    let mode = Mode::new(&flag_parser);

    let Some(mmap) = read_file(&rush_file_path) else {
        eprintln!("could not read: {rush_file_path}");
        return ExitCode::FAILURE
    };

    /* Handle custom parallelalism */ {
        let parall = if flag_parser.passed(&CUSTOM_PARALLELALISM) {
            flag_parser.parse(&CUSTOM_PARALLELALISM).or(Some(1))
        } else {
            None
        };

        if let Some(parall) = parall {
            let max = available_parallelism().unwrap().get() as _;
            if parall < 0 || parall > max {
                eprintln!("invalid amount of the maximum parallel jobs: {parall}");
                return ExitCode::FAILURE
            }
            if let Err(e) = ThreadPoolBuilder::new().num_threads(parall as _).build_global() {
                eprintln!("could not initialize thread pool: {e}");
                return ExitCode::FAILURE
            }
        }
    }

    let content = unsafe { std::str::from_utf8_unchecked(&mmap[..]) };
    let (escaped, escaped_indexes) = Parser::handle_newline_escapes(content);
    let processed = Parser::parse(&escaped, &escaped_indexes).into_processed();

    let mmap = Db::read_cache();
    let content = mmap.as_ref().map(|mmap| unsafe { std::str::from_utf8_unchecked(&mmap[..]) });
    let db = content.and_then(|content| Db::read(content).ok());

    let (graph, default_target, transitive_deps) = build_dependency_graph(&processed);
    _ = CommandRunner::run(&mode, &processed, graph, db, default_target, transitive_deps).write_finish();

    ExitCode::SUCCESS
}
