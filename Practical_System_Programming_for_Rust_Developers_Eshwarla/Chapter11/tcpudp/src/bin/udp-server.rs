use std::net::UdpSocket;
use std::str;
use std::thread;

fn main() {
    let socket = UdpSocket::bind("127.0.0.1:3000").expect("Unable to bind to port");
    let mut buffer = [0; 1024];
    loop {
        let socket_new = socket.try_clone().expect("Unable to clone socket");
        match socket_new.recv_from(&mut buffer) {
            Ok((num_bytes, src_addr)) => {
                thread::spawn(move || {
                    let send_buffer = &mut buffer[..num_bytes];
                    println!(
                        "Received from client:{}",
                        str::from_utf8(send_buffer).unwrap()
                    );
                    let response_string =
                        format!("Received this: {}", String::from_utf8_lossy(send_buffer));
                    socket_new
                        .send_to(&response_string.as_bytes(), &src_addr)
                        .expect("error in sending datagram to remote socket");
                });
            }
            Err(err) => {
                println!("Error in receiving datagrams over UDP: {}", err);
            }
        }
    }
}
