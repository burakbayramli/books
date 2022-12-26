use std::fs;
fn main() {
    let file_metadata = fs::metadata("stats.txt").expect("Unable to get file metadata");
    println!(
        "Len: {}, last accessed: {:?}, modified : {:?}, created: {:?}",
        file_metadata.len(),
        file_metadata.accessed(),
        file_metadata.modified(),
        file_metadata.created()
    );
    println!(
        "Is file: {}, Is dir: {}, is Symlink: {}",
        file_metadata.is_file(),
        file_metadata.is_dir(),
        file_metadata.file_type().is_symlink()
    );
    println!("File metadata: {:?}", fs::metadata("stats.txt"));
    println!("Permissions of file are: {:?}", file_metadata.permissions());
}