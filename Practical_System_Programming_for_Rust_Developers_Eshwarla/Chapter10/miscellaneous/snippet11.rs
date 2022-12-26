use std::fs::File;
use std::io::Read;
fn main() -> std::io::Result<()> {
    // Open two file handles for reading
    let f1 = File::open("file1.txt")?;
    let f2 = File::open("file3.txt")?;
    //Chain the two file handles
    let mut chained_handle = f1.chain(f2);
    // Create a buffer to read into
    let mut buffer = String::new();
    // Read from chained handle into buffer
    chained_handle.read_to_string(&mut buffer)?;
    println!("Read from chained handle: {}", buffer);
    Ok(())
}