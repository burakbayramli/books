use std::io::Write;
use std::io::{stdin, stdout};
use std::io::{Error, ErrorKind};
use std::process::Command;
fn main() {
    println!("Hello! Welcome to Myshell");
    loop {
        print!("$ ");
        stdout().flush().unwrap();
        let mut user_input = String::new();
        stdin()
            .read_line(&mut user_input)
            .expect("Unable to read user input");
        let command_to_execute = user_input.trim();
        let command_args: Vec<&str> = command_to_execute.split_whitespace().collect();

        if command_args.len() > 0 {
            let child = match command_args[0] {
                "show" if command_args.len() > 1 => match command_args[1] {
                    "files" => Command::new("ls").args(&command_args[2..]).spawn(),

                    "process" => Command::new("ps").args(&command_args[2..]).spawn(),

                    _ => Err(Error::new(
                        ErrorKind::InvalidInput,
                        "please enter valid command",
                    )),
                },
                "show" if command_args.len() == 1 => Err(Error::new(
                    ErrorKind::InvalidInput,
                    "please enter valid command",
                )),
                "quit" => std::process::exit(0),
                _ => Command::new(command_args[0])
                    .args(&command_args[1..])
                    .spawn(),
            };
            match child {
                Ok(mut child) => {
                    if !child.wait().unwrap().success() {
                        println!("\n{}", "Child process failed")
                    }
                }
                Err(e) => match e.kind() {
                    ErrorKind::InvalidInput => eprintln!(
                        "Sorry, show command only supports following options: files , process "
                    ),
                    _ => eprintln!("Please enter a valid command"),
                },
            }
        }
    }
}
