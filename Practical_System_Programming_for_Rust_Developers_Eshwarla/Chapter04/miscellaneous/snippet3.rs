use std::thread::sleep;
use std::time::{Duration, Instant};
fn main() {
    let now = Instant::now();
    sleep(Duration::new(3, 0));
    println!("{:?}", now.elapsed().as_secs());
}
