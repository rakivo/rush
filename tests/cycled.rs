use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Output};

const MANIFEST_PATH: &str = env!("CARGO_MANIFEST_DIR");

#[inline]
fn create_rush_file(name: &str, content: &str) {
    let mut path_buf = PathBuf::from(name);
    _ = path_buf.set_extension("rush");

    let path = path_buf.display();

    let mut file = match File::create(&path_buf) {
        Ok(ok) => ok,
        Err(e) => panic!("could not create {path}: {e}")
    };

    if let Err(e) = file.write_all(content.as_bytes()) {
        panic!("could not write to {path}: {e}")
    }
}

#[inline]
fn run_rush(path: &str) -> Output {
    let binary_path = format!("{MANIFEST_PATH}/target/debug/rush");
    Command::new(binary_path)
        .arg(format!("-f {path}.rush"))
        .output()
        .expect("could not execute command")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cycled() {
        let file_path = "cycled";

        create_rush_file(file_path, r#"rule echo
  command = echo "hello $in"

build hello: echo hello
build all: hello"#);

        let out = run_rush(file_path);

        assert!(!out.status.success());

        let stderr = unsafe {
            std::str::from_utf8_unchecked(&out.stderr)
        };

        assert!{
            stderr.contains(r#"cycled.rush:4: edge "hello" depends on itself"#)
        }
    }

    #[test]
    fn cycled_path() {
        let file_path = "cycled_path";

        create_rush_file(file_path, r#"rule echo
  command = echo "Building $out"

build A: echo B
build B: echo C
build C: echo A

default A"#);

        let out = run_rush(file_path);

        assert!(!out.status.success());

        let stderr = unsafe {
            std::str::from_utf8_unchecked(&out.stderr)
        };

        assert!{
            stderr.contains(r#"cycled_path.rush:4: cycle detected: A -> B -> C -> A
note: edge "A" is causing the cycle"#)
        }
    }
}
