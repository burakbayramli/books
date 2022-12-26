use std::process::Command;
fn main() {
    Command::new("ls")
        .current_dir("..")
        .args(&["-l", "-h"])
        .spawn()
        .expect("ls command failed to start");
}
