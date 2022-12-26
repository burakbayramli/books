use std::io::prelude::*;
use std::process::{Command, Stdio};
fn main() {
    let process = match Command::new("rev")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
    {
        Err(err) => panic!("couldn't spawn rev: {}", err),
        Ok(process) => process,
    };
    match process.stdin.unwrap().write_all("palindrome".as_bytes()) {
        Err(why) => panic!("couldn't write to stdin: {}", why),
        Ok(_) => println!("sent text to rev command"),
    }
    let mut child_output = String::new();
    match process.stdout.unwrap().read_to_string(&mut child_output) {
        Err(err) => panic!("couldn't read stdout: {}", err),
        Ok(_) => print!("Output from child process is:\n{}", child_output),
    }
}