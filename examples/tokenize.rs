/*

Numbrs - A text-based calculator
tokenize.rs - Lexer tester application
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

use numbrs::lexer::Lexer;
use rustyline::{error::ReadlineError, Editor};

fn main() {
    let mut rl: Editor<()> = Editor::new().unwrap();

    loop {
        match rl.readline("> ") {
            Ok(line) => {
                if line == "q" || line == "quit" || line == "exit" {
                    break;
                }
                rl.add_history_entry(&line);
                tokenize(&line);
            }
            Err(ReadlineError::Eof) => break,
            Err(err) => panic!("{:?}", err),
        }
    }
}

fn tokenize(line: &str) {
    Lexer::new(line).for_each(|tok| println!("{:?}", tok))
}
