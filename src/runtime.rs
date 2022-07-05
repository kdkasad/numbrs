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

use crate::eval::Eval;
use crate::parser::Parser;
use crate::{eval, parser};
use bigdecimal::BigDecimal;
use std::collections::HashMap;
use std::error::Error;
use thiserror::Error;

pub struct Runtime {
    env: HashMap<String, BigDecimal>,
}

impl Runtime {
    pub fn new() -> Self {
        let mut env = HashMap::new();
        env.insert("_".to_string(), bigdec!(0));
        Self { env }
    }

    pub fn evaluate(&mut self, input: &str) -> Result<BigDecimal, Box<dyn Error>> {
        let value = Parser::new(input).parse()?.eval(&mut self.env)?;
        Ok(value)
    }
}

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("parse error: {0}")]
    Parse(#[from] parser::Error),

    #[error("evaluation error: {0}")]
    Eval(#[from] eval::NodeEvalError),
}
