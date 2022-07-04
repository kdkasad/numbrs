use crate::eval::Eval;
use crate::parser::Parser;
use decimal::d128;
use rustyline::{error::ReadlineError, Editor};
use std::collections::HashMap;

mod ast;
mod eval;
mod parser;
mod scanner;
mod token;

const PROMPT: &str = "> ";

const COLOR_ERR: &str = "\x1b[1;31m";
const COLOR_RST: &str = "\x1b[m";

fn main() {
    // TODO: add completion helper
    let mut rl = Editor::<()>::new();
    let mut env: HashMap<String, d128> = HashMap::new();

    loop {
        match rl.readline(PROMPT) {
            Ok(line) => {
                rl.add_history_entry(&line);

                let p = Parser::new(&line);
                match p.parse() {
                    Ok(ast) => {
                        println!("{:?}", ast);
                        match ast.eval(&mut env) {
                            Ok(value) => println!("{}", value),
                            Err(e) => println!("{}Error:{} {}", COLOR_ERR, COLOR_RST, e),
                        };
                    }
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
