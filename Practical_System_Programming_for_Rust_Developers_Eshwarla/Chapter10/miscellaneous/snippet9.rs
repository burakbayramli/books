use std::fs::File;
use std::io::{BufRead, BufReader};
fn main() {
    // Open a file for reading
    let f = File::open("file.txt").unwrap();
    //Create a BufReader instance to optimize sys calls
    let file_reader = BufReader::new(f);
    // Read from standard input line-by-line
    for single_line in file_reader.lines() {
        println!("Line read from file :{}", single_line.unwrap());
    }
}