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

/*!
# Numbrs execution runtime

This module contains types and functions that provide a runtime for Numbrs.
The runtime unifies parsing and evaluating expressions into one API. It also
maintains the environment map.

The runtime also contains its own [`Runtime::format()`] function which
formats a [`Value`] using the precision stored in the variable by the name
of [`PRECISION_IDENT`][2] (currently `_prec`).

[2]: Runtime::PRECISION_IDENT

## Examples

This code snippet uses a [`Runtime`] to implement a basic command-line
[REPL][1]:

```no_run
use numbrs::runtime::Runtime;

let mut rt = Runtime::new();
rt.load_defaults();
for line in std::io::stdin().lines() {
    let input = line.unwrap();
    let value = rt.evaluate(&input)?;
    println!("Result: {}", rt.format(&value)?);
}
# Ok::<(), numbrs::runtime::RuntimeError>(())
```

Also see `src/main.rs`. While it is the source for the main Numbrs binary,
it suffices as an example of how to use the Numbrs runtime because it's
quite short. It's just a command-line wrapper around that uses the Numbrs
runtime to process input.

[1]: https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop
*/

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

#[cfg(feature = "debug")]
use crate::lexer::{Lexer, Token};

/// # Numbrs execution runtime
///
/// Handles storage of variable map and application of user preferences like
/// output format precision.
///
/// See the [module-level documentation][1] for details and examples.
///
/// [1]: crate::runtime
#[derive(Debug)]
pub struct Runtime {
    /// Environment map.
    ///
    /// Contains the defined variables and units for this [`Runtime`].
    pub env: HashMap<String, Value>,
}

impl Runtime {
    /// Default precision for formatting result values.
    ///
    /// See [`Formatter::format()`] for details.
    pub const DEFAULT_PRECISION: isize = 5;
    /// Name of the variable which stores the formatting precision.
    ///
    /// The [`Runtime`] will use the value of the variable with this name as the
    /// precision to use in [`Runtime::format()`].
    pub const PRECISION_IDENT: &'static str = "_prec";
    /// Unassignment identifier.
    ///
    /// This identifier has a value of zero (*0*). It is special in that when it
    /// is assigned to a variable, that variable is removed from the
    /// environment. See [`Runtime::evaluate()`] for more details.
    pub const UNASSIGN_IDENT: &'static str = "_";

    /// # Create a new [`Runtime`].
    ///
    /// Sets the environment to an empty map with two exceptions:
    ///  - *0* is assigned to the identifier [`Self::UNASSIGN_IDENT`]
    ///  - [`Self::DEFAULT_PRECISION`] is assigned to the identifier
    ///    [`Self::PRECISION_IDENT`].
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

    /// # Load default unit definitions.
    ///
    /// Default units are defined in `src/defaults.num`. They consist of many of
    /// the [SI][1] units and will contain [Imperial][2] units as well in the
    /// future.
    ///
    /// [1]: https://en.wikipedia.org/wiki/International_System_of_Units
    /// [2]:
    ///     https://en.wikipedia.org/wiki/Imperial_and_US_customary_measurement_systems#Units_in_use
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

    /// # Evaluate an expression string.
    ///
    /// Parses and evaluates the expression string `input` and returns the
    /// resulting [`Value`]. See the [parser][2] and [eval][3] modules for
    /// details on what that entails.
    ///
    /// [2]: crate::parser
    /// [3]: crate::eval
    ///
    /// ## Errors
    ///
    /// This function returns a [`RuntimeError`] when it fails.
    ///
    /// Errors returned by [`Parser::parse()`] and [`Node::eval()`] are
    /// propagated. The [`ParseError`] and [`EvalError`] types are encapsulated
    /// in [`RuntimeError::Parse`] and [`RuntimeError::Eval`], respectively.
    ///
    /// This function will also return [`RuntimeError::AssignmentProhibited`]
    /// when attempting to assign to the [unassignment identifier][1] or to the
    /// name of any base quantity (see [`BaseQuantity::VARIANTS`]).
    ///
    /// [1]: Runtime::UNASSIGN_IDENT
    ///
    /// ## Example
    /// ```
    /// # use numbrs::{
    /// #     eval::EvalError,
    /// #     runtime::{Runtime, RuntimeError},
    /// # };
    /// let mut rt = Runtime::new();
    /// // ...
    /// rt.evaluate("foo = _").unwrap();
    /// match rt.evaluate("foo") {
    ///     Err(RuntimeError::Eval(EvalError::UndefinedVariable(name))) => {
    ///         assert_eq!(name, "foo")
    ///     },
    ///     _ => panic!("Expected undefined variable error"),
    /// }
    /// ```
    pub fn evaluate(&mut self, input: &str) -> Result<Value, RuntimeError> {
        let tree = Parser::new(input).parse()?;
        check_for_prohibited_behavior(&tree)?;
        Ok(tree.eval(&mut self.env)?)
    }

    /// # Evaluate an expression string with debug information.
    ///
    /// Lexes, parses, and evaluates the expression string, returning the result
    /// if possible at each step.
    ///
    /// ## Returns
    ///
    ///  - If parsing fails: `(Vec<Token>, Result::Err(RuntimeError::Parse))`.
    ///  - If parsing succeeds but evaluation fails: `(Vec<Token>, Result::Ok(Node, Result::Err(RuntimeError)))`.
    ///  - If all operations succeed: `(Vec<Token>, Result::Ok(Node, Result::Ok(Value)))`.
    ///
    /// ## Errors
    ///
    /// This function always returns the
    ///
    /// Errors returned by [`Parser::parse()`] and [`Node::eval()`] are
    /// propagated. The [`ParseError`] and [`EvalError`] types are encapsulated
    /// in [`RuntimeError::Parse`] and [`RuntimeError::Eval`], respectively.
    ///
    /// This function will also return [`RuntimeError::AssignmentProhibited`]
    /// when attempting to assign to the [unassignment identifier][1] or to the
    /// name of any base quantity (see [`BaseQuantity::VARIANTS`]).
    ///
    /// [1]: Runtime::UNASSIGN_IDENT
    #[allow(clippy::type_complexity)]
    #[cfg(feature = "debug")]
    pub fn evaluate_debug(
        &mut self,
        input: &str,
    ) -> (
        Vec<Token>,
        Result<(Node, Result<Value, RuntimeError>), RuntimeError>,
    ) {
        let tokens: Vec<Token> = Lexer::new(input).collect();
        match Parser::from(tokens.iter().cloned()).parse() {
            Ok(tree) => {
                let maybe_value: Result<Value, RuntimeError> =
                    match check_for_prohibited_behavior(&tree) {
                        Ok(()) => tree.to_owned().eval(&mut self.env).map_err(|e| e.into()),
                        Err(e) => Err(e),
                    };
                (tokens, Ok((tree, maybe_value)))
            }
            Err(e) => (tokens, Err(e.into())),
        }
    }

    /// # Format a [`Value`] with configurable precision
    ///
    /// This function formats `value` using the value of the [precision
    /// specifier variable][1] as the precision to pass to
    /// [`value.format()`][2].
    ///
    /// The precision value must be an integer.
    ///
    /// ## Errors
    ///
    /// [`RuntimeError::NonIntegerPrecision`] is returned if the value of the
    /// precision specifier variable is not an integer.
    ///
    /// [1]: Runtime::PRECISION_IDENT
    /// [2]: crate::format::Formatter::format()
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
///   - Assigning to any base quantity identifier
// NOTE: also document prohibited behavior in Parser::evaluate().
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
        Node::Variable(_) | Node::Number(_) | Node::Quantity(_) | Node::FunctionCall(_) => Ok(()),
    }
}

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}

/// # Runtime error
///
/// Represents an error that occurs within the Numbrs execution runtime. This
/// encapsulates virtually every error that Numbrs can produce.
#[derive(thiserror::Error, Debug)]
pub enum RuntimeError {
    /// ## Parse error
    ///
    /// Propagated from [`parser::ParseError`][1].
    ///
    /// [1]: ParseError
    #[error("Parse error: {0}")]
    Parse(#[from] ParseError),

    /// ## Evaluation error
    ///
    /// Propagated from [`eval::EvalError`][2].
    ///
    /// [2]: EvalError
    #[error("Evaluation error: {0}")]
    Eval(#[from] EvalError),

    /// ## Non-integer precision
    ///
    /// The value stored in the [precision specifier variable][1] is not an
    /// integer and cannot be used to format a value.
    ///
    /// [1]: Runtime::PRECISION_IDENT
    #[error("Got non-integer precision specifier `{}`. Unable to format result.", .0.format(3))]
    NonIntegerPrecision(Box<Value>),

    /// ## Assignment prohibited
    ///
    /// See [`Runtime::evaluate()`] for a list of what assignments are
    /// prohibited.
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
