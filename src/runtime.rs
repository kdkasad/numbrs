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
use strum::{IntoEnumIterator, VariantNames};

use crate::{
    ast::{BinaryExpression, Node, UnaryExpression, Value},
    dimension::{BaseQuantity, Dimension},
    eval::EvalError,
    format::Formatter,
    operation::Operation,
    parser::{ParseError, Parser},
    rat_util_macros::rat,
    unit::{Unit, Units},
};

/// Execution runtime for Numbrs.
///
/// Handles storage of variable map and application of user
/// preferences like output format precision.
#[derive(Debug)]
pub struct Runtime {
    env: HashMap<String, Value>,
}

impl Runtime {
    pub const DEFAULT_PRECISION: isize = 5;
    pub const PRECISION_IDENT: &'static str = "_prec";
    pub const UNASSIGN_IDENT: &'static str = "_";

    pub fn new() -> Self {
        let mut env: HashMap<String, Value> = HashMap::new();
        env.insert(Self::UNASSIGN_IDENT.to_string(), BigRational::zero().into());
        env.insert(
            Self::PRECISION_IDENT.to_string(),
            rat!(Self::DEFAULT_PRECISION).into(),
        );

        create_base_units(&mut env);

        Self { env }
    }

    /// Load default unit definitions.
    ///
    /// Defaults are defined in `src/defaults.num`.
    pub fn load_defaults(&mut self) -> Result<(), RuntimeError> {
        const DEFAULTS_SRC: &str = include_str!("defaults.num");
        // Filter out comments and empty lines
        let lines = DEFAULTS_SRC
            .lines()
            .filter(|line| !line.is_empty() && !matches!(line.chars().next(), Some('#')));
        for line in lines {
            self.evaluate(line)?;
        }
        Ok(())
    }

    pub fn evaluate(&mut self, input: &str) -> Result<Value, RuntimeError> {
        let tree = Parser::new(input).parse()?;
        check_for_prohibited_behavior(&tree)?;
        Ok(tree.eval(&mut self.env)?)
    }

    pub fn format(&self, value: &Value) -> Result<String, RuntimeError> {
        match self.env.get(Self::PRECISION_IDENT) {
            Some(prec) => match prec {
                Value::Number(rat) => match (rat.is_integer(), rat.to_isize()) {
                    (true, Some(precision)) => Ok(value.format(precision)),
                    _ => Err(RuntimeError::NonIntegerPrecision(Box::new(prec.clone()))),
                },
                Value::Quantity(_) | Value::Unit(_) => {
                    Err(RuntimeError::NonIntegerPrecision(Box::new(prec.clone())))
                }
            },
            None => Ok(value.format(Self::DEFAULT_PRECISION)),
        }
    }
}

/// Create a base unit for each base quantity
fn create_base_units(env: &mut HashMap<String, Value>) {
    for variant in BaseQuantity::iter() {
        let name = variant.to_string();
        let mut dimension = Dimension::new();
        dimension[variant] = 1;
        let units = Units::from(vec![Unit::new(&name, 1, rat!(1), rat!(0), dimension)]);
        env.insert(name, units.into());
    }
}

/// Check if an expression tree contains prohibited behavior.
/// The list of prohibited behaviors currently includes:
///   - Assigning to the `_` variable
///   - Assigning to any base unit
fn check_for_prohibited_behavior(tree: &Node) -> Result<(), RuntimeError> {
    match tree {
        Node::BinaryExpression(BinaryExpression {
            operation,
            lhs,
            rhs,
        }) => {
            // Check for assignment to `_` or base units
            if let Operation::Assign | Operation::AssignUnit = operation {
                if let Node::Variable(var) = &**lhs {
                    if var.name() == Runtime::UNASSIGN_IDENT
                        || BaseQuantity::VARIANTS.contains(&var.name())
                    {
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

    #[error("Got non-integer precision specifier `{}`. Unable to format result.", .0.format(3))]
    NonIntegerPrecision(Box<Value>),

    #[error("Can't assign special variable `{0}`")]
    AssignmentProhibited(String),
}

#[cfg(test)]
mod tests {
    use ::pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn assign_protected() {
        macro_rules! test {
            ($rt:expr, $expr:expr, $varname:expr) => {
                match $rt
                    .evaluate($expr)
                    .expect_err("Expression evaluation was successful")
                {
                    RuntimeError::AssignmentProhibited(var) => {
                        assert_eq!(var, $varname);
                    }
                    _ => unreachable!(),
                }
            };
        }

        let mut rt = Runtime::new();

        test!(rt, "_ = 123", "_");
        test!(rt, "1 + _ = 123", "_");
        test!(rt, "a = _ = 17", "_");

        for bq in BaseQuantity::VARIANTS {
            test!(rt, &format!("{} = _", bq), *bq);
        }
    }

    #[test]
    fn base_unit_creation() {
        let rt = Runtime::new();
        for variant in BaseQuantity::iter() {
            let name = variant.to_string();
            assert!(matches!(rt.env.get(&name), Some(Value::Unit(_))));
        }
    }
}
