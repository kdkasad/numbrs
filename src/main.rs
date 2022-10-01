/*

Numbrs - A text-based calculator
main.rs - Numbrs command-line application
Copyright (C) 2022  Kian Kasad

This file is part of Numbrs.

Numbrs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, version 3 of the License.

Numbrs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Numbrs.  If not, see <https://www.gnu.org/licenses/>.

*/

extern crate rustyline;

use numbrs::runtime::Runtime;
use rustyline::{error::ReadlineError, Editor};

use self::textio::*;

mod textio {
    pub const PROMPT: &str = "> ";
    pub const COLOR_ERR: &str = "\x1b[1;31m";
    pub const COLOR_WARN: &str = "\x1b[1;33m";
    pub const COLOR_RST: &str = "\x1b[m";
}

fn main() {
    // TODO: add completion helper
    let mut rl = match Editor::<()>::new() {
        Ok(editor) => editor,
        Err(err) => {
            eprintln!(
                "{}Error:{} failed to create line editor: {}",
                COLOR_ERR, COLOR_RST, err
            );
            return;
        }
    };
    let mut rt = Runtime::new();

    loop {
        match rl.readline(PROMPT) {
            Ok(line) => {
                if line == "q" || line == "quit" || line == "exit" {
                    break;
                }

                rl.add_history_entry(&line);

                match rt.evaluate(&line) {
                    Ok(value) => match rt.format(&value) {
                        Ok(output) => println!("{}", output),
                        Err(err) => eprintln!("{}Error:{} {}", COLOR_ERR, COLOR_RST, err),
                    },
                    Err(err) => eprintln!("{}Error:{} {}", COLOR_ERR, COLOR_RST, err),
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
