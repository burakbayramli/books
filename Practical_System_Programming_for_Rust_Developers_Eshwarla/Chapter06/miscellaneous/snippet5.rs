use std::fs;
fn main() {
    let byte_arr = fs::read("stats3.txt").expect("Unable to read file into bytes");
    println!(
        "Value read from file into bytes is {}",
        String::from_utf8(byte_arr).unwrap()
    );
    let string1 = fs::read_to_string("stats3.txt").expect("Unable to read file into string");
    println!("Value read from file into string is {}", string1);
}