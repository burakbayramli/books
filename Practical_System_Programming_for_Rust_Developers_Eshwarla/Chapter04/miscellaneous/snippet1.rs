use std::fs;
fn main() {
    let entries = fs::read_dir("/tmp").unwrap();
    for entry in entries {
        if let Ok(entry) = entry {
            println!("{:?}", entry.path());
        }
    }
}
