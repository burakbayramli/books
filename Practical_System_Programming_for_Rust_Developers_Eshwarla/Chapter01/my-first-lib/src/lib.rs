pub fn hello_from_lib(message: & str) {
    println!("Printing Hello {} from library",message);
}


#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
