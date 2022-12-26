use std::fs;
fn main() {
    fs::write("stats3.txt", "Rust is exciting,isn't it?").expect("Unable to write to file");
}