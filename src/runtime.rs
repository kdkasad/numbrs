/*

runtime.rs - Numbrs runtime
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

//! Execution runtime for Numbrs.

use std::collections::HashMap;

use num::{BigRational, ToPrimitive, Zero};

use crate::{
    ast::Value,
    eval::EvalError,
    format::Formatter,
    parser::{ParseError, Parser},
    rat_util_macros::rat,
};

/// Execution runtime for Numbrs.
///
/// Handles storage of variable map and application of user
/// preferences like output format precision.
pub struct Runtime {
    env: HashMap<String, Value>,
}

impl Runtime {
    const DEFAULT_PRECISION: usize = 5;
    const UNASSIGN_IDENT: &'static str = "_";

    pub fn new() -> Self {
        let mut env = HashMap::new();
        env.insert(Self::UNASSIGN_IDENT.to_string(), BigRational::zero().into());
        env.insert("_prec".to_string(), rat!(Self::DEFAULT_PRECISION).into());
        Self { env }
    }

    pub fn evaluate(&mut self, input: &str) -> Result<Value, RuntimeError> {
        Ok(Parser::new(input).parse()?.eval(&mut self.env)?)
    }

    pub fn format(&self, value: &Value) -> Result<String, RuntimeError> {
        match self.env.get("_prec") {
            Some(prec) => match prec {
                Value::Number(rat) => {
                    if rat.is_integer() {
                        match rat.to_usize() {
                            Some(precision) => Ok(value.format(precision)),
                            None => Err(RuntimeError::InvalidPrecision(prec.clone())),
                        }
                    } else {
                        Err(RuntimeError::InvalidPrecision(prec.clone()))
                    }
                }
                Value::Quantity(_) => Err(RuntimeError::InvalidPrecision(prec.clone())),
            },
            None => Ok(value.format(Self::DEFAULT_PRECISION)),
        }
    }
}

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(thiserror::Error, Debug)]
pub enum RuntimeError {
    #[error("Parse error: {0}")]
    Parse(#[from] ParseError),

    #[error("Evaluation error: {0}")]
    Eval(#[from] EvalError),

    #[error("Invalid precision specifier: {}", .0.format(3))]
    InvalidPrecision(Value),
}

#[cfg(test)]
mod tests {
    use std::f64::consts::PI;

    use super::*;

    #[test]
    fn format() {
        macro_rules! case {
            ( $a:expr , $b:expr, $c:expr ) => {
                (
                    BigRational::from_float($a as f64).unwrap(),
                    BigRational::from_float($b as f64).unwrap(),
                    stringify!($c),
                )
            };
        }

        let mut rt = Runtime::new();
        let cases = [
            case!(13.14159, 2, 13.14),
            case!(PI, 0, 3),
            case!(123.456, -1, 120),
            case!(123.456, 4, 123.4560),
            case!(19, 2, 19.00),
            case!(12.345, 1.5, 12.35),
        ];

        for (value, prec, expected) in cases {
            // rt.env.insert("_prec".to_string(), prec);
            rt.evaluate(&format!("_prec := {}", prec)).unwrap();
            assert_eq!(expected, rt.format(&value.into()).unwrap());
        }
    }
}
