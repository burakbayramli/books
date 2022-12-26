use std::io::Write;
fn main() {
    //Create a memory buffer
    let buffer = b"Hello, this is error message from standard error stream\n";
    // Get handle to output error stream
    let stderr_handle = std::io::stderr();
    // Lock the handle to output error stream
    let mut locked_stderr_handle = stderr_handle.lock();
    // write into error stream from buffer
    locked_stderr_handle.write(buffer).unwrap();
}