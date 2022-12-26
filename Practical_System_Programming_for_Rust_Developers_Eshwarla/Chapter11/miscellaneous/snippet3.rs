use std::net::{IpAddr, Ipv4Addr, SocketAddr};
fn main() {
    // Create an ipv4 socket
    let socket = SocketAddr::new(IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)), 8000);
    println!(
        "Socket address is {}, port is {}",
        socket.ip(),
        socket.port()
    );
    println!("Is this IPv6 socket?{}", socket.is_ipv6());
}