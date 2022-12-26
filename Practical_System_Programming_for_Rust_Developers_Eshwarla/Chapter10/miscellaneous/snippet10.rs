use std::fs::File;
use std::io::Read;
fn main() {
    // Open two file handles for reading
    let f1 = File::open("file1.txt").unwrap();
    let f2 = File::open("file2.txt").unwrap();
    //Chain the two file handles
    let mut chained_handle = f1.chain(f2);
    // Create a buffer to read into
    let mut buffer = String::new();
    // Read from chained handle into buffer
    chained_handle.read_to_string(&mut buffer).unwrap();
    // Print out the value read into the buffer
    println!("Read from chained handle:\n{}", buffer);
}