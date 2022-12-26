use std::net::UdpSocket;

fn main() {
    // Create a local UDP socket
    let socket = UdpSocket::bind("0.0.0.0:0").expect("Unable to bind to socket");

    // Connect the socket to a remote socket
    socket
        .connect("127.0.0.1:3000")
        .expect("Could not connect to UDP server");
    println!("socket peer addr is {:?}", socket.peer_addr());
    // Send a datagram to the remote socket
    socket
        .send("Hello: sent using send() call".as_bytes())
        .expect("Unable to send bytes");
}
