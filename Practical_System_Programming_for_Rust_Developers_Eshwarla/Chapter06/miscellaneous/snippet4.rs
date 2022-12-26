use std::fs;
fn main() {
    fs::rename("stats1.txt", "stats3.txt").expect("Unable to rename");
}