use std::fs;
fn main() -> std::io::Result<()> {
    fs::hard_link("stats.txt", "./statsa.txt")?; // Hard
                                                 // link stats.txt to statsa.txt
    Ok(())
}