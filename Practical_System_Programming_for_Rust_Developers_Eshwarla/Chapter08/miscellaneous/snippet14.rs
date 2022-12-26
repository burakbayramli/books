use std::panic;
use std::process::{Command, Stdio};
fn main() {
    panic::set_hook(Box::new(|_| {
        println!(
            " This is an example of custom panic
hook, which is invoked on thread panic, but
before the panic run-time is invoked"
        )
    }));
    let _child_process = match Command::new("invalid-command")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
    {
        Err(err) => panic!("Normal panic message {}", err),
        Ok(new_process_handle) => new_process_handle,
    };
}