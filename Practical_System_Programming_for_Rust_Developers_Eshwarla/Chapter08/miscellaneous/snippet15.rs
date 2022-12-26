use signal_hook::iterator::Signals;
use std::io::Error;
fn main() -> Result<(), Error> {
    let signals = Signals::new(&[signal_hook::SIGTERM, signal_hook::SIGINT])?;
    'signal_loop: loop {
        // Pick up signals that arrived since last time
        for signal in signals.pending() {
            match signal {
                signal_hook::SIGINT => {
                    println!("Received signal SIGINT");
                }
                signal_hook::SIGTERM => {
                    println!("Received signal SIGTERM");
                    break 'signal_loop;
                }
                _ => unreachable!(),
            }
        }
    }
    println!("Terminating program");
    Ok(())
}