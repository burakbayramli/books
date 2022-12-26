use std::fs;
fn main() {
    let mut permissions = fs::metadata("stats.txt").unwrap().permissions();
    permissions.set_readonly(true);
    let _ = fs::set_permissions("stats.txt", permissions).expect("Unable to set permission");
    fs::write("stats.txt", "Hello- Can you see me?").expect("Unable to write to file");
}