use std::process::Command;
fn main() {
    let status = Command::new("cat")
        .arg("non-existent-file.txt")
        .status()
        .expect("failed to execute cat");
    if status.success() {
        println!("Successful operation");
    } else {
        println!("Unsuccessful operation");
    }
}