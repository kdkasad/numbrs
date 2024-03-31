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

mod completion;

use std::{cell::RefCell, rc::Rc};

use numbrs::{affixes::standard_prefixes, runtime::Runtime};
use rustyline::{error::ReadlineError, CompletionType, Editor};

use self::textio::*;
use crate::completion::IdentCompleter;

macro_rules! message {
    ($name:literal) => {
        include_str!(concat!(env!("OUT_DIR"), "/messages/", $name, ".txt"))
    };
}

mod textio {
    pub const PROMPT: &str = "> ";

    #[cfg(feature = "debug")]
    pub const COLOR_DBG: &str = "\x1b[1;34m";

    pub const COLOR_ERR: &str = "\x1b[1;31m";
    pub const COLOR_WARN: &str = "\x1b[1;33m";
    pub const COLOR_RST: &str = "\x1b[m";

    pub const STARTUP_MESSAGE: &str = message!("startup");
    pub const LICENSE_MESSAGE: &str = message!("license");
}

fn main() {
    let editor_config = rustyline::Config::builder()
        .completion_type(CompletionType::List)
        .build();
    let mut rl = match Editor::with_config(editor_config) {
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
    if let Err(err) = rt.load_defaults() {
        eprintln!(
            "{}Error:{} failed to load defaults into runtime: {}",
            COLOR_ERR, COLOR_RST, err
        );
        return;
    }
    let shared_rt = Rc::new(RefCell::new(rt));

    // Create and register completion helper
    rl.set_helper(Some(IdentCompleter::new(
        Rc::clone(&shared_rt),
        standard_prefixes(),
    )));

    // Print startup message
    // TODO: only print when connected to terminal
    eprintln!("{}", STARTUP_MESSAGE);

    loop {
        match rl.readline(PROMPT) {
            Ok(line) => {
                if line == "q" || line == "quit" || line == "exit" {
                    break;
                }

                rl.add_history_entry(&line);

                if line == "license" {
                    print!("{}", LICENSE_MESSAGE);
                    continue;
                }

                // Ignore comments
                if let Some('#') = line.chars().next() {
                    continue;
                }

                #[cfg(not(feature = "debug"))]
                {
                    let value = shared_rt.borrow_mut().evaluate(&line);
                    match value {
                        Ok(value) => match shared_rt.borrow().format(&value) {
                            Ok(output) => println!("{}", output),
                            Err(err) => eprintln!("{}Error:{} {}", COLOR_ERR, COLOR_RST, err),
                        },
                        Err(err) => eprintln!("{}Error:{} {}", COLOR_ERR, COLOR_RST, err),
                    }
                }

                #[cfg(feature = "debug")]
                {
                    let (tokens, maybe_tree) = shared_rt.borrow_mut().evaluate_debug(&line);
                    eprintln!("{}Tokens:{} {:?}", COLOR_DBG, COLOR_RST, tokens);
                    match maybe_tree {
                        Ok((tree, maybe_value)) => {
                            println!("{}Tree:{} {:?}", COLOR_DBG, COLOR_RST, tree);
                            match maybe_value {
                                Ok(value) => match shared_rt.borrow().format(&value) {
                                    Ok(output) => println!("{}", output),
                                    Err(err) => eprintln!("{}Error:{} {}", COLOR_ERR, COLOR_RST, err),
                                },
                                Err(err) => eprintln!("{}Error:{} {}", COLOR_ERR, COLOR_RST, err),
                            };
                        }
                        Err(err) => eprintln!("{}Error:{} {}", COLOR_ERR, COLOR_RST, err),
                    }
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
