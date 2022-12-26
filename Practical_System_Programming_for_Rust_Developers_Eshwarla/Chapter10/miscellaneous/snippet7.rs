use std::io::{BufRead, BufReader};
fn main() {
    // Create handle to standard input
    let s = std::io::stdin();
    //Create a BufReader instance to optimize sys calls
    let file_reader = BufReader::new(s);
    // Read from standard input line-by-line
    for single_line in file_reader.lines() {
        println!("You typed:{}", single_line.unwrap());
    }
}