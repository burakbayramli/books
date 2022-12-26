use std::io::{self, Write};
fn main() {
    //Create a memory buffer to read from file
    let mut buffer = String::new();
    // read a line into the buffer
    let _ = io::stdin().read_line(&mut buffer).unwrap();
    // Write the buffer to standard output
    io::stdout().write(&mut buffer.as_bytes()).unwrap();
}
