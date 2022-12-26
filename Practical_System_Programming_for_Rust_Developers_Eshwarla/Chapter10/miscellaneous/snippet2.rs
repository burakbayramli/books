use std::fs::File;
use std::io::{BufRead, BufReader};
fn main() {
    // Open a file
    let f = File::open("records.txt").unwrap();
    // Create a BufReader, passing in the file handle
    let mut buf_reader = BufReader::new(f);
    //Create a memory buffer to read from file
    let mut buffer = String::new();
    // read a line into the buffer
    buf_reader.read_line(&mut buffer).unwrap();
    println!("Read the following: {}", buffer);
}