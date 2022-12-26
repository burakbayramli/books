use std::env;
fn main() {
    for argument in env::args() {
        println!("{}", argument);
    }
}
