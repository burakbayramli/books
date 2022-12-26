use std::fs;
use std::thread;
fn copy_file() -> thread::Result<()> {
    thread::spawn(|| {
        fs::copy("a.txt", "b.txt").expect("Error occurred");
    })
    .join()
}
fn main() {
    match copy_file() {
        Ok(_) => println!("Ok. copied"),
        Err(_) => println!("Error in copying file"),
    }
}