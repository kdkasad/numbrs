use crate::parser::Parser;
use rustyline::{error::ReadlineError, Editor};

mod ast;
mod parser;
mod scanner;
mod token;

const PROMPT: &str = "> ";

const COLOR_ERR: &str = "\x1b[1;31m";
const COLOR_RST: &str = "\x1b[m";

fn main() {
    // TODO: add completion helper
    let mut rl = Editor::<()>::new();

    loop {
        match rl.readline(PROMPT) {
            Ok(line) => {
                rl.add_history_entry(&line);

                let p = Parser::new(&line);
                match p.parse() {
                    Ok(ast) => println!("{:?}", ast),
                    Err(e) => println!("{}Error:{} {}", COLOR_ERR, COLOR_RST, e),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("Interrupted");
                break;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(e) => {
                println!("{}Error:{} {:?}", COLOR_ERR, COLOR_RST, e);
                break;
            }
        }
    }
}
