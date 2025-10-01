use std::fs::{self, File};
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
        Err(e) => panic!("could not create {path}: {e}"),
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

#[inline]
fn remove_rush(path: &str) {
    if let Err(e) = fs::remove_file(format!("{path}.rush")) {
        panic!("could not remove: {path}: {e}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dollar_escape() {
        let file_name = "dollar_escape";

        create_rush_file(
            file_name,
            r#"
rule echo
  command = echo "hello $$in"

build hello: echo
"#,
        );

        let out = run_rush(file_name);
        remove_rush(file_name);

        assert!(out.status.success());

        let stdout = unsafe { std::str::from_utf8_unchecked(&out.stdout) };

        assert!(stdout.contains("hello $in"));
    }
}
