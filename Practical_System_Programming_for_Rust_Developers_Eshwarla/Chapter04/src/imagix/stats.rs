use super::error::ImagixError;
use super::resize::get_image_files;
use std::path::PathBuf;

pub fn get_stats(src_folder: PathBuf) -> Result<(usize, f64), ImagixError> {
    let image_files = get_image_files(src_folder.to_path_buf())?;
    let size = image_files
        .iter()
        .map(move |f| f.metadata().unwrap().len())
        .sum::<u64>();
    Ok((image_files.len(), (size / 1000000) as f64))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_get_stats() {
        let path = PathBuf::from("/tmp/images");
        let (count, size) = get_stats(path).unwrap();
        // Note: For this test to pass,
        // alter the count and size with the right values
        assert_eq!(count, 2);
        assert_eq!(size, 17.0);
    }
}
