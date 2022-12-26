use std::net::{IpAddr, Ipv4Addr, Ipv6Addr};
fn main() {
    // Create an ipv4 address
    let ip_v4_addr = IpAddr::V4(Ipv4Addr::new(106, 201, 34, 209));
    // check if an address is ipv4 or ipv6 address
    println!("Is ip_v4_addr an ipv4 address? {}", ip_v4_addr.is_ipv4());
    println!("Is ip_v4_addr an ipv6 address? {}", ip_v4_addr.is_ipv6());
    // Create an ipv6 address
    let ip_v6_addr = IpAddr::V6(Ipv6Addr::new(0, 0, 0, 0, 0, 0, 0, 1));
    println!("Is ip_v6_addr an ipv6 address? {}", ip_v6_addr.is_ipv6());
}
