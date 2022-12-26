use std::process::Command;
fn main() {
    Command::new("ls")
        .arg("-l")
        .arg("-h")
        .spawn()
        .expect("ls command failed to start");
}