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
    ast::{BinaryExpression, Node, UnaryExpression, Value},
    eval::EvalError,
    format::Formatter,
    operation::Operation,
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
    const PRECISION_IDENT: &'static str = "_prec";
    pub const UNASSIGN_IDENT: &'static str = "_";

    pub fn new() -> Self {
        let mut env = HashMap::new();
        env.insert(Self::UNASSIGN_IDENT.to_string(), BigRational::zero().into());
        env.insert(
            Self::PRECISION_IDENT.to_string(),
            rat!(Self::DEFAULT_PRECISION).into(),
        );
        Self { env }
    }

    pub fn evaluate(&mut self, input: &str) -> Result<Value, RuntimeError> {
        let tree = Parser::new(input).parse()?;
        check_for_prohibited_behavior(&tree)?;
        Ok(tree.eval(&mut self.env)?)
    }

    pub fn format(&self, value: &Value) -> Result<String, RuntimeError> {
        match self.env.get(Self::PRECISION_IDENT) {
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

/// Check if an expression tree contains prohibited behavior.
/// The list of prohibited behaviors currently includes:
///   - Assigning to the `_` variable
fn check_for_prohibited_behavior(tree: &Node) -> Result<(), RuntimeError> {
    match tree {
        Node::BinaryExpression(BinaryExpression {
            operation,
            lhs,
            rhs,
        }) => {
            // Check for assignment to `_`
            if let Operation::Assign = operation {
                if let Node::Variable(var) = &**lhs {
                    if var.name() == Runtime::UNASSIGN_IDENT {
                        return Err(RuntimeError::AssignmentProhibited(var.name().to_owned()));
                    }
                }
            }

            // Recursively check subtrees
            check_for_prohibited_behavior(lhs)?;
            check_for_prohibited_behavior(rhs)?;
            Ok(())
        }

        Node::UnaryExpression(UnaryExpression { expr, .. }) => check_for_prohibited_behavior(expr),
        Node::Variable(_) | Node::Number(_) | Node::Quantity(_) => Ok(()),
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

    #[error("Got non-natural precision specifier `{}`. Unable to format result.", .0.format(3))]
    InvalidPrecision(Value),

    #[error("Can't assign special variable `{0}`")]
    AssignmentProhibited(String),
}

#[cfg(test)]
mod tests {
    use std::f64::consts::PI;

    use pretty_assertions::assert_eq;

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

    #[test]
    fn assign_underscore() {
        macro_rules! test {
            ($rt:expr, $expr:literal) => {
                match $rt
                    .evaluate($expr)
                    .expect_err("Expression evaluation was successful")
                {
                    RuntimeError::AssignmentProhibited(var) => {
                        assert_eq!(var, Runtime::UNASSIGN_IDENT);
                    }
                    _ => unreachable!(),
                }
            };
        }
        let mut rt = Runtime::new();
        test!(rt, "_ := 123");
        test!(rt, "1 + _ := 123");
        test!(rt, "a := _ := 17");
    }
}
