use std::fs::File;
use std::io::{BufWriter, Write};
fn main() {
    // Create a file
    let f = File::create("file.txt").unwrap();
    // Create a BufWriter, passing in the file handle
    let mut buf_writer = BufWriter::new(f);
    //Create a memory buffer
    let buffer = String::from("Hello, testing");
    // write into the buffer
    buf_writer.write(buffer.as_bytes()).unwrap();
    println!("wrote the following: {}", buffer);
}