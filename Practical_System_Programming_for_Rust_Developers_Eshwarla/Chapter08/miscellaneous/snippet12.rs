use std::process::Command;
fn main() {
    Command::new("env")
        .env_clear()
        .spawn()
        .expect("Command failed to execute");
}