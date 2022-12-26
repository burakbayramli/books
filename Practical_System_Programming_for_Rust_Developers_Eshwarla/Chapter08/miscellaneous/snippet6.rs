use std::process;
fn main() {
    println!("Going to abort process");
    process::abort();
    // This statement will not get executed
    println!("Process aborted");
}