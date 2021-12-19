use std::fs;
use std::fmt::Display;

pub fn run_challenge<A, B, F, G>(day: &str, part: &str, parser: F, challenge: G) -> ()
    where B: Display, F: FnOnce(String) -> Result<A, String>, G: FnOnce(A) -> B {

    fn read_input(day: &str) -> std::io::Result<String> {
        return fs::read_to_string(format!("inputs/{}", day));
    }

    let process_input = |input| {
        match parser(input) {
            Ok(p) => {
                println!("{}-{}: {}", day, part, challenge(p));
            },
            Err(err) => {
                println!("[error]: Could not parse input for {}-{}: {}", day, part, err);
            }
        }
    };

    match read_input(day) {
        Ok(input) => process_input(input),
        Err(error) => println!("[error]: failed to read file for {}-{}: {}", day, part, error)
    };
}