use std::fs::DirBuilder;
fn main() {
    let dir_structure = "/tmp/dir1/dir2/dir3";
    DirBuilder::new()
        .recursive(true)
        .create(dir_structure)
        .unwrap();
}