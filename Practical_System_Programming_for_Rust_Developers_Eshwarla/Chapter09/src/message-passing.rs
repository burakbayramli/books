use std::sync::mpsc;
use std::thread;

fn main() {
    let (transmitter1, receiver) = mpsc::channel();

    let transmitter2 = mpsc::Sender::clone(&transmitter1);

    thread::spawn(move || {
        let num_vec: Vec<String> = vec!["One".into(), "two".into(), "three".into(), "four".into()];
        for num in num_vec {
            transmitter1.send(num).unwrap();
        }
    });

    thread::spawn(move || {
        let num_vec: Vec<String> =
            vec!["Five".into(), "Six".into(), "Seven".into(), "eight".into()];
        for num in num_vec {
            transmitter2.send(num).unwrap();
        }
    });

    for received_val in receiver {
        println!("Received from thread: {}", received_val);
    }
}
