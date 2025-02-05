#[macro_use]
mod loc;

mod cr;
mod db;
mod types;
mod graph;
mod parser;
mod consts;
mod command;
mod template;

use db::Db;
use parser::Parser;
use cr::CommandRunner;
use graph::build_dependency_graph;

use std::process::ExitCode;

fn main() -> ExitCode {
    let Some(mmap) = Parser::read_rush() else {
        eprintln!("no rush file found in cwd");
        return ExitCode::FAILURE
    };

    let content = unsafe { std::str::from_utf8_unchecked(&mmap[..]) };
    let (escaped, escaped_indexes) = Parser::handle_newline_escapes(content);
    let processed = Parser::parse(&escaped, &escaped_indexes).into_processed();

    let mmap = Db::read_cache();
    let content = mmap.as_ref().map(|mmap| unsafe { std::str::from_utf8_unchecked(&mmap[..]) });
    let db = content.and_then(|content| Db::read(content).ok());

    let (graph, default_target, transitive_deps) = build_dependency_graph(&processed);
    let db = CommandRunner::run(&processed, graph, db, default_target, transitive_deps);

    _ = db.write_finish();

    ExitCode::SUCCESS
}
