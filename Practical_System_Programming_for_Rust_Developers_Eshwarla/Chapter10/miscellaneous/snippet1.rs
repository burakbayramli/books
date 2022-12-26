use std::fs::File;
use std::io::Read;
fn main() {
    // Open a file
    let mut f = File::open("records.txt").unwrap();
    //Create a memory buffer to read from file
    let mut buffer = [0; 1024];
    // read from file into buffer
    let _ = f.read(&mut buffer[..]).unwrap();
}