use std::process::{Command, Stdio};
fn main() {
    let _child_process = match Command::new("invalid-command")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
    {
        Err(err) => panic!("Unable to spawn child process: {}", err),
        Ok(new_process_handle) => {
            println!("Successfully spawned child process");
            new_process_handle
        }
    };
}