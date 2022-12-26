use std::fs::File;
use std::fs::OpenOptions;
fn main() {
    // Method 1
    let _file1 = File::open("stats1.txt").expect("File not found");
    // Method 2
    let _file2 = OpenOptions::new()
        .write(true)
        .create(true)
        .open("stats2.txt");
}
