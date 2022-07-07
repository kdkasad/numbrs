//
// numbrs - A text-based calculator
// Copyright (C) 2022  Kian Kasad
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, version 3 of the License.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//

extern crate bigdecimal;
#[cfg(test)]
extern crate pretty_assertions;
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
                if line == "q" || line == "quit" || line == "exit" {
                    break;
                }

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
