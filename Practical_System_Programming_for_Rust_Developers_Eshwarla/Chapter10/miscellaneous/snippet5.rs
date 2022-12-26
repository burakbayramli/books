use std::io::{Read, Write};
fn main() {
    //Create a memory buffer
    let mut buffer = [8; 1024];
    // Get handle to input stream
    let stdin_handle = std::io::stdin();
    // Lock the handle to input stream
    let mut locked_stdin_handle = stdin_handle.lock();
    // read a line into the buffer
    locked_stdin_handle.read(&mut buffer).unwrap();
    // Get handle to output stream
    let stdout_handle = std::io::stdout();
    // Lock the handle to output stream
    let mut locked_stdout_handle = stdout_handle.lock();
    // Write the buffer to standard output
    locked_stdout_handle.write(&mut buffer).unwrap();
}
