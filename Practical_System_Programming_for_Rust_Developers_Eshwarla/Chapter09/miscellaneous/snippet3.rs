use std::thread;
fn main() {
    let mut child_threads = Vec::new();
    for i in 1..5 {
        let builder = thread::Builder::new().name(format!("mythread{}", i));
        let handle = builder
            .spawn(|| {
                println!("Hi from thread id {:?}", thread::current().name().unwrap());
            })
            .unwrap();
        child_threads.push(handle);
    }
    for i in child_threads {
        i.join().unwrap();
    }
}
