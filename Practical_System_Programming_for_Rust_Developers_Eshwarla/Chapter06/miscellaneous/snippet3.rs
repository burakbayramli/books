use std::fs;
fn main() {
    fs::copy("stats1.txt", "stats2.txt").expect("Unable to copy");
}