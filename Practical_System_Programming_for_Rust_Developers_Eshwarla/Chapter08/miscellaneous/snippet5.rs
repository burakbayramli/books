use std::process::Command;
fn main() {
    let output = Command::new("cat").arg("a.txt").output().unwrap();
    if !output.status.success() {
        println!("Command executed with failing error code");
    }
    println!("printing: {}", String::from_utf8(output.stdout).unwrap());
}
