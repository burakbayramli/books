use std::env;
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::process::exit;
use std::thread;

fn main() {
    // Accept commandline parameters for proxy_stream and origin_stream
    let args: Vec<_> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Please provide proxy-from and proxy-to addresses");
        exit(2);
    }
    let proxy_server = &args[1];
    let origin_server = &args[2];

    // Start a socket server on proxy_stream
    let proxy_listener;
    if let Ok(proxy) = TcpListener::bind(proxy_server) {
        proxy_listener = proxy;
        let addr = proxy_listener.local_addr().unwrap().ip();
        let port = proxy_listener.local_addr().unwrap().port();
        if let Err(_err) = TcpStream::connect(origin_server) {
            println!("Please re-start the origin server");
            exit(1);
        }

        println!("Running on Addr:{}, Port:{}\n", addr, port);
    } else {
        eprintln!("Unable to bind to specified proxy port");
        exit(1);
    }

    let mut thread_handles = Vec::new();

    // Listen for incoming connections from proxy_server and read byte stream
    for proxy_stream in proxy_listener.incoming() {
        let mut proxy_stream = proxy_stream.expect("Error in incoming TCP connection");
        // Establish a new TCP connection to origin_stream
        let mut origin_stream =
            TcpStream::connect(origin_server).expect("Please re-start the origin server");
        let handle =
            thread::spawn(move || handle_connection(&mut proxy_stream, &mut origin_stream));
        thread_handles.push(handle);
    }
    for handle in thread_handles {
        handle.join().expect("Unable to join child thread");
    }
}

fn handle_connection(proxy_stream: &mut TcpStream, origin_stream: &mut TcpStream) {
    let mut in_buffer: Vec<u8> = vec![0; 200];
    let mut out_buffer: Vec<u8> = vec![0; 200];

    // Read incoming request to proxy_stream
    if let Err(err) = proxy_stream.read(&mut in_buffer) {
        println!("Error in reading from incoming proxy stream: {}", err);
    } else {
        println!(
            "1: Incoming client request: {}",
            String::from_utf8_lossy(&in_buffer)
        );
    }

    // Write the byte stream to origin_stream
    let _ = origin_stream.write(&mut in_buffer).unwrap();
    println!("2: Forwarding request to origin server\n");
    // Read response from the backend server
    let _ = origin_stream.read(&mut out_buffer).unwrap();
    println!(
        "3: Received response from origin server: {}",
        String::from_utf8_lossy(&out_buffer)
    );
    // Write response back to the proxy client
    let _ = proxy_stream.write(&mut out_buffer).unwrap();
    println!("4: Forwarding response back to client");
}
