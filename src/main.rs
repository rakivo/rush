use std::fs::{self, File};
use std::process::ExitCode;
use std::collections::HashMap;

use memmap2::Mmap;

const RUSH_FILE_NAME: &str = "build.rush";

#[inline(always)]
fn to_str(bytes: &[u8]) -> &str {
    unsafe { std::str::from_utf8_unchecked(bytes) }
}

fn read_rush() -> Option::<Mmap> {
    if let Some(f) = fs::read_dir(".")
        .expect("could not read cwd")
        .filter_map(|res| res.map(|e| e.path()).ok())
        .find(|path| {
            path.file_name()
                .map_or(false, |name| name.to_string_lossy() == RUSH_FILE_NAME)
        })
    {
        let file = File::open(f).ok()?;
        Some(unsafe { Mmap::map(&file) }.ok()?)
    } else {
        None
    }
}

#[derive(Debug, Default, PartialEq)]
struct Rule<'a> {
    name: &'a str,
    command: &'a str,
}

#[derive(Debug, Default, PartialEq)]
struct Job<'a> {
    input: &'a str,
    output: &'a str,
}

#[derive(Debug, Default, PartialEq)]
struct Def<'a> {
    name: &'a str,
    value: &'a str
}

#[derive(Debug, Default)]
struct Parsed<'a> {
    jobs: Vec::<Job<'a>>,
    defs: HashMap::<&'a str, Def<'a>>,
    rules: HashMap::<&'a str, Rule<'a>>,
}

#[derive(Debug, Default, PartialEq)]
enum Context<'a> {
    #[default]
    Global,
    Rule(&'a str),
}

#[derive(Debug, Default)]
struct Parser<'a> {
    cursor: usize,
    parsed: Parsed<'a>,
    context: Context<'a>
}

impl<'a> Parser<'a> {
    fn parse_line(&mut self, line: &'a str) {
        let start = line.chars().take_while(|c| c.is_ascii_whitespace()).count();

        if start == 0 && matches!(self.context, Context::Rule(..)) {
            self.context = Context::Global
        }

        let Some(first_space) = line[start..].chars()
            .position(|c| c.is_ascii_whitespace())
            .map(|p| p + start) else {
                return
            };

        let Some(second_space) = line[first_space..].chars()
            .position(|c| !c.is_ascii_whitespace())
            .map(|p| p + first_space) else {
                return
            };

        let first_token = line[start..first_space].trim();

        match &self.context {
            Context::Rule(name) => {
                if first_token != "command" {
                    if line.is_empty() || line == "\n" {
                        self.context = Context::Global;
                        return
                    }
                }

                let ref command = line[second_space + 1 + 1..].trim();

                let rule = Rule { name, command };
                self.parsed.rules.insert(name, rule);
            },
            Context::Global => {
                match first_token {
                    "rule" => {
                        self.context = Context::Rule(&line[second_space..].trim());
                        return
                    },
                    "build" => {
                        let colon_idx = line.chars().position(|c| c == ':').unwrap();
                        let ref input = line[first_space..colon_idx].trim();
                        let ref output = line[colon_idx+1..].trim();
                        let job = Job {input, output};
                        self.parsed.jobs.push(job);
                        return
                    },
                    _ => {}
                };

                let ref name = first_token;
                let ref value = line[second_space + 1..].trim();

                let def = Def { name, value };
                self.parsed.defs.insert(name, def);
            },
        };
    }

    fn parse(content: &'a str) -> Parsed<'a> {
        let mut parser = Self::default();
        for line in content.lines() {
            parser.cursor += 1;
            parser.parse_line(line)
        }
        parser.parsed
    }
}

fn main() -> ExitCode {
    let Some(rush) = read_rush() else {
        return ExitCode::FAILURE
    };

    let content = to_str(&rush[..]);
    let parser = Parser::parse(content);

    println!("{parser:#?}");

    ExitCode::SUCCESS
}
