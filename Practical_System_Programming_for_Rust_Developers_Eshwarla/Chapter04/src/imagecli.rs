#[allow(dead_code)]
mod imagix;
use ::imagix::error::ImagixError;
use ::imagix::resize::{process_resize_request, Mode, SizeOption};
use ::imagix::stats::get_stats;
use std::path::PathBuf;
use structopt::StructOpt;
// Define commandline arguments in a struct

#[derive(StructOpt, Debug)]
#[structopt(
    name = "resize",
    about = "This is a tool for image resizing and stats",
    help = "Specify subcommand resize or stats. For help, type imagecli resize --help or imagecli stats --help"
)]
enum Commandline {
    #[structopt(help = "Specify size(small/medium/large) , mode(single/all) and srcfolder")]
    Resize {
        #[structopt(long)]
        size: SizeOption,
        #[structopt(long)]
        mode: Mode,
        #[structopt(long, parse(from_os_str))]
        srcfolder: PathBuf,
    },
    #[structopt(help = "Specify srcfolder")]
    Stats {
        #[structopt(long, parse(from_os_str))]
        srcfolder: PathBuf,
    },
}

fn main() {
    let args: Commandline = Commandline::from_args();
    match args {
        Commandline::Resize {
            size,
            mode,
            mut srcfolder,
        } => {
            match process_resize_request(size, mode, &mut srcfolder) {
                Ok(_) => println!("Image(s) resized successfully"),
                Err(e) => match e {
                    ImagixError::FileIOError(e) => println!("{}", e),
                    ImagixError::UserInputError(e) => println!("{}", e),
                    ImagixError::ImageResizingError(e) => println!("{}", e),
                    _ => println!("Error in processing"),
                },
            };
        }
        Commandline::Stats { srcfolder } => match get_stats(srcfolder) {
            Ok((count, size)) => println!(
                "Found {:?} image files with aggregate size of {:?} MB",
                count, size
            ),
            Err(e) => match e {
                ImagixError::FileIOError(e) => println!("{}", e),
                ImagixError::UserInputError(e) => println!("{}", e),
                _ => println!("Error in processing"),
            },
        },
    }
}
