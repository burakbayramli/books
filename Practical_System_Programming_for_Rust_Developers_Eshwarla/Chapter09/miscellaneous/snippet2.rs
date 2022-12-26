use std::thread;
fn main() {
    let mut child_threads = Vec::new();
    for _ in 1..5 {
        let handle = thread::spawn(|| {
            println!("Hi from thread id {:?}", thread::current().id());
        });
        child_threads.push(handle);
    }
    for i in child_threads {
        i.join().unwrap();
    }
}
