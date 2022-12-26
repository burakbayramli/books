static mut THREAD_COUNT: u32 = 4;
use std::env::var;
fn change_thread_count(count: u32) {
    unsafe {
        THREAD_COUNT = count;
    }
}
fn main() {
    if let Some(thread_count) = var("THREAD_COUNT").ok() {
        change_thread_count(thread_count.parse::<u32>().unwrap());
    };
    unsafe {
        println!("Thread count is: {}", THREAD_COUNT);
    }
}