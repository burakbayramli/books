use std::process;
fn main() {
    println!("Going to exit process with error code 64");
    process::exit(64);
    // execution never gets here
    println!("Process exited");
}
