use std::thread;
fn main() {
    for _ in 1..5 {
        thread::spawn(|| {
            println!("Hi from thread id {:?}", thread::current().id());
        });
    }
}