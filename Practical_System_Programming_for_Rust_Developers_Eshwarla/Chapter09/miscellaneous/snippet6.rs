use std::thread;
use std::time::Duration;
fn main() {
    let duration = Duration::new(1, 0);
    println!("Going to sleep");
    thread::sleep(duration);
    println!("Woke up");
}
