use std::ffi::OsStr;
use std::fs;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::thread;

#[derive(Debug)]
pub struct SrcStats {
    pub number_of_files: u32,
    pub loc: u32,
    pub comments: u32,
    pub blanks: u32,
}

fn main() {
    let src_stats = SrcStats {
        number_of_files: 0,
        loc: 0,
        comments: 0,
        blanks: 0,
    };
    let stats_counter = Arc::new(Mutex::new(src_stats));

    let mut dir_list = File::open("./dirnames.txt").unwrap();
    let reader = BufReader::new(&mut dir_list);
    let dir_lines: Vec<_> = reader.lines().collect();

    let mut child_handles = vec![];
    for dir in dir_lines {
        let dir = dir.unwrap();
        let src_stats = Arc::clone(&stats_counter);

        let handle = thread::spawn(move || {
            let mut dir_entries = vec![PathBuf::from(dir)];
            let mut file_entries = vec![];
            while let Some(entry) = dir_entries.pop() {
                for inner_entry in fs::read_dir(&entry).unwrap() {
                    if let Ok(entry) = inner_entry {
                        if entry.path().is_dir() {
                            dir_entries.push(entry.path());
                        } else {
                            if entry.path().extension() == Some(OsStr::new("rs")) {
                                println!("File name processed is {:?}", entry);
                                file_entries.push(entry);
                            }
                        }
                    }
                }
            }
            for file in file_entries {
                let file_contents = fs::read_to_string(&file.path()).unwrap();

                let mut stats_pointer = src_stats.lock().unwrap();
                for line in file_contents.lines() {
                    if line.len() == 0 {
                        stats_pointer.blanks += 1;
                    } else if line.starts_with("//") {
                        stats_pointer.comments += 1;
                    } else {
                        stats_pointer.loc += 1;
                    }
                }

                stats_pointer.number_of_files += 1;
            }
        });
        child_handles.push(handle);
    }

    for handle in child_handles {
        handle.join().unwrap();
    }
    println!("Source stats: {:?}", stats_counter.lock().unwrap());
}
