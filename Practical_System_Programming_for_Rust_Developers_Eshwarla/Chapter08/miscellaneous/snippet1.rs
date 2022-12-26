use std::process::Command;
fn main() {
    Command::new("ls")
        .spawn()
        .expect("ls command failed to start");
}