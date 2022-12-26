use std::fs;
use std::os::unix::fs as fsunix;
fn main() {
    fsunix::symlink("stats.txt", "sym_stats.txt").expect("Cannot create symbolic link");
    let sym_path = fs::read_link("sym_stats.txt").expect("Cannot read link");
    println!("Link is {:?}", sym_path);
}