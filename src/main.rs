extern crate bigdecimal;
extern crate rustyline;
extern crate thiserror;

use crate::runtime::Runtime;
use rustyline::{error::ReadlineError, Editor};

mod ast;
#[macro_use]
mod bigdecimal_helper;
mod eval;
mod parser;
mod runtime;
mod scanner;
mod token;

const PROMPT: &str = "> ";

const COLOR_ERR: &str = "\x1b[1;31m";
const COLOR_RST: &str = "\x1b[m";

fn main() {
    // TODO: add completion helper
    let mut rl = Editor::<()>::new();
    let mut rt = Runtime::new();

    loop {
        match rl.readline(PROMPT) {
            Ok(line) => {
                rl.add_history_entry(&line);

                match rt.evaluate(&line) {
                    Ok(value) => println!("{}", value),
                    Err(e) => println!("{}Error:{} {}", COLOR_ERR, COLOR_RST, e),
                }
            }

            Err(ReadlineError::Eof) => {
                break;
            }

            Err(ReadlineError::Interrupted) => {
                println!("Interrupted");
                break;
            }
            Err(e) => {
                println!("{}Error:{} {:?}", COLOR_ERR, COLOR_RST, e);
                break;
            }
        }
    }
}
