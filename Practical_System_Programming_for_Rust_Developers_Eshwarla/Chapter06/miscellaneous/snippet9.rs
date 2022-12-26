use std::fs;
use std::path::Path;
fn main() {
    let dir_entries = fs::read_dir(".").expect("Unable to read directory contents");
    // Read directory contents
    for entry in dir_entries {
        //Get details of each directory entry
        let entry = entry.unwrap();
        let entry_path = entry.path();
        let entry_metadata = entry.metadata().unwrap();
        let entry_file_type = entry.file_type().unwrap();
        let entry_file_name = entry.file_name();
        println!(
            "Path is {:?}.\n Metadata is {:?}\n File_type is {:?}.\n Entry name is{:?}.\n",
            entry_path, entry_metadata, entry_file_type, entry_file_name
        );
    }
    // Get path components
    let new_path = Path::new("/usr/d1/d2/d3/bar.txt");
    println!("Path parent is: {:?}", new_path.parent());
    for component in new_path.components() {
        println!("Path component is: {:?}", component);
    }
}