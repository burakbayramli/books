use std::io::prelude::*;
use std::process::{Command, Stdio};
fn main() {
    // Spawn the `ps` command
    let process = match Command::new("ps").stdout(Stdio::piped()).spawn() {
        Err(err) => panic!("couldn't spawn ps: {}", err),
        Ok(process) => process,
    };
    let mut ps_output = String::new();
    match process.stdout.unwrap().read_to_string(&mut ps_output) {
        Err(err) => panic!("couldn't read ps stdout: {}", err),
        Ok(_) => print!("ps output from child process is:\n{}", ps_output),
    }
}
