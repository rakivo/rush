#[macro_use]
mod loc;

mod cr;
mod types;
mod graph;
mod parser;
mod consts;
mod command;
mod template;

use cr::CommandRunner;
use parser::{Parser, read_rush};
use graph::build_dependency_graph;

use std::process::ExitCode;

fn main() -> ExitCode {
    let Some(mmap) = read_rush() else {
        eprintln!("no rush file found in cwd");
        return ExitCode::FAILURE
    };

    let content = unsafe { std::str::from_utf8_unchecked(&mmap[..]) };
    let (escaped, escaped_indexes) = Parser::handle_newline_escapes(content);
    let processed = Parser::parse(&escaped, &escaped_indexes).into_processed();

    let (graph, default_target, transitive_deps) = build_dependency_graph(&processed);
    CommandRunner::run(&processed, graph, default_target, &transitive_deps);

    ExitCode::SUCCESS
}
