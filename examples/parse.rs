/*

Numbrs - A text-based calculator
parse.rs - Parser tester application
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

use numbrs::parser::Parser;
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
                parse(&line);
            }
            Err(ReadlineError::Eof) => break,
            Err(err) => Err(err).unwrap(),
        }
    }
}

fn parse(line: &str) {
    dbg!(Parser::new(line).parse().unwrap());
}
