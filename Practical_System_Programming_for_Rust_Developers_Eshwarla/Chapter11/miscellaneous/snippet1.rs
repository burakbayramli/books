use std::net::{Ipv4Addr, Ipv6Addr};
fn main() {
    // Create a new IPv4 address with four 8-bit integers
    let ip_v4_addr1 = Ipv4Addr::new(106, 201, 34, 209);
    // Use the built-in constant to create a new loopback
    // (localhost) address
    let ip_v4_addr2 = Ipv4Addr::LOCALHOST;
    println!(
        "Is ip_v4_addr1 a loopback address? {}",
        ip_v4_addr1.is_loopback()
    );
    println!(
        "Is ip_v4_addr2 a loopback address? {}",
        ip_v4_addr2.is_loopback()
    );
    //Create a new IPv6 address with eight 16-bit
    // integers, represented in hex
    let ip_v6_addr = Ipv6Addr::new(2001, 0000, 3238, 0xDFE1, 0063, 0000, 0000, 0xFEFB);
    println!("IPV6 segments {:?}", ip_v6_addr.segments());
}
