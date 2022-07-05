extern crate bigdecimal;
extern crate rustyline;
extern crate thiserror;

use crate::runtime::Runtime;
use crate::textio::{COLOR_ERR, COLOR_RST, COLOR_WARN, PROMPT};
use rustyline::{error::ReadlineError, Editor};

mod ast;
#[macro_use]
mod bigdecimal_helper;
mod eval;
mod parser;
mod runtime;
mod scanner;
mod token;

mod textio {
    pub const PROMPT: &str = "> ";

    pub const COLOR_ERR: &str = "\x1b[1;31m";
    pub const COLOR_WARN: &str = "\x1b[1;33m";
    pub const COLOR_RST: &str = "\x1b[m";
}

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
                    Err(e) => eprintln!("{}Error:{} {}", COLOR_ERR, COLOR_RST, e),
                }
            }

            Err(ReadlineError::Eof) => {
                break;
            }

            Err(ReadlineError::Interrupted) => {
                eprintln!("{}Warning:{} process interrupted.", COLOR_WARN, COLOR_RST);
                break;
            }
            Err(e) => {
                println!("{}Error:{} {:?}", COLOR_ERR, COLOR_RST, e);
                break;
            }
        }
    }
}
