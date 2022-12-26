use std::fs;
use std::thread;
struct Filenames {
    source: String,
    destination: String,
}
impl Drop for Filenames {
    fn drop(&mut self) {
        if thread::panicking() {
            println!("dropped due to  panic");
        } else {
            println!("dropped without panic");
        }
    }
}
fn copy_file(file_struct: Filenames) -> thread::Result<()> {
    thread::spawn(move || {
        fs::copy(&file_struct.source, &file_struct.destination).expect("Error occurred");
    })
    .join()
}
fn main() {
    let foo = Filenames {
        source: "a1.txt".into(),
        destination: "b.txt".into(),
    };
    match copy_file(foo) {
        Ok(_) => println!("Ok. copied"),
        Err(_) => println!("Error in copying file"),
    }
}
