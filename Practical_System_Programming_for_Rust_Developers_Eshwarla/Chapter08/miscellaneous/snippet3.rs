use std::process::Command;
fn main() {
    Command::new("ls").args(&["-l", "-h"]).spawn().unwrap();
}