use std::path::PathBuf;
fn main() {
    let mut f_path = PathBuf::new();
    f_path.push(r"/tmp");
    f_path.push("packt");
    f_path.push("rust");
    f_path.push("book");
    f_path.set_extension("rs");
    println!("Path constructed is {:?}", f_path);
}