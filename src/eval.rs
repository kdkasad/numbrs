/*

eval.rs - Evaluation of AST nodes for Numbrs
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

//! Evaluation of [AST nodes][Node] for Numbrs

use std::{collections::HashMap, ops::Neg};

use num::{BigRational, ToPrimitive, Zero};
use thiserror::Error;

use crate::{
    affixes::{resolve_unit, try_get_prefix_scale},
    ast::*,
    dimension::Dimension,
    operation::Operation,
    rat_util_macros::rat,
    runtime::Runtime,
    unit::{self, Unit, Units},
};

pub trait Operable {
    fn binary_op(self, op: Operation, rhs: Value) -> Result<Value, EvalError>;
    fn unary_op(self, op: Operation) -> Result<Value, EvalError>;
}

impl Operable for BigRational {
    fn binary_op(self, op: Operation, rhs: Value) -> Result<Value, EvalError> {
        use Operation::*;

        if let Assign | AssignUnit = op {
            return Err(EvalError::InvalidAssignmentLHS);
        }

        if let UnaryAdd | UnarySubtract = op {
            return Err(EvalError::ExpectedOperation("binary", "unary", op));
        }

        match rhs {
            Value::Number(rhs) => Ok(match op {
                Add => self + rhs,
                Subtract => self - rhs,
                Multiply => self * rhs,
                Divide => self / rhs,
                Raise => bigrational_pow(self, rhs)?,
                Assign | AssignUnit | UnaryAdd | UnarySubtract | ConvertUnits => unreachable!(),
            }
            .into()),
            Value::Quantity(mut rhs) => match op {
                Add | Subtract => Err(EvalError::InvalidBinaryOperation(
                    Box::new(self.into()),
                    Box::new(rhs.into()),
                    op,
                )),
                Raise => Err(EvalError::NonIntegerExponent(Box::new(rhs.into()))),
                Multiply => {
                    rhs.mag *= self;
                    Ok(rhs.into())
                }
                Divide => {
                    rhs.mag = self / rhs.mag;
                    rhs.units.pow_assign(-1);
                    Ok(rhs.into())
                }
                Assign | AssignUnit | UnaryAdd | UnarySubtract | ConvertUnits => unreachable!(),
            },
            Value::Unit(mut rhs) => match op {
                Multiply => Ok(Quantity::new(self, rhs).into()),
                Divide => {
                    rhs.pow_assign(-1);
                    Ok(Quantity::new(self, rhs).into())
                }
                Add | Subtract => Err(EvalError::InvalidBinaryOperation(
                    Box::new(self.into()),
                    Box::new(rhs.into()),
                    op,
                )),
                Raise => Err(EvalError::NonIntegerExponent(Box::new(rhs.into()))),
                Assign | AssignUnit | UnaryAdd | UnarySubtract | ConvertUnits => unreachable!(),
            },
        }
    }

    fn unary_op(self, op: Operation) -> Result<Value, EvalError> {
        use Operation::*;
        match op {
            UnaryAdd => Ok(self.into()),
            UnarySubtract => Ok(self.neg().into()),
            Add | Subtract | Multiply | Divide | Raise | Assign | AssignUnit | ConvertUnits => {
                Err(EvalError::ExpectedOperation("unary", "binary", op))
            }
        }
    }
}

impl Operable for Quantity {
    fn binary_op(mut self, op: Operation, rhs: Value) -> Result<Value, EvalError> {
        use Operation::*;

        if let UnaryAdd | UnarySubtract = op {
            return Err(EvalError::ExpectedOperation("binary", "unary", op));
        }

        if let Assign | AssignUnit = op {
            return Err(EvalError::InvalidAssignmentLHS);
        }

        match rhs {
            Value::Quantity(rhs) => {
                // Check for invalid cases
                match op {
                    Add | Subtract => {
                        if !self.units.conforms_to(&rhs.units) {
                            return Err(EvalError::ConvertNonConformingUnits(
                                Box::new(self.units),
                                Box::new(rhs.units),
                            ));
                        }
                    }
                    Raise => return Err(EvalError::NonIntegerExponent(Box::new(rhs.into()))),
                    Multiply | Divide => (), // valid case, do nothing
                    UnaryAdd | UnarySubtract | Assign | AssignUnit | ConvertUnits => unreachable!(),
                }

                // Perform operations for valid cases
                match op {
                    Add => {
                        self.mag += unit::convert(&rhs.mag, &rhs.units, &self.units)?;
                    }
                    Subtract => {
                        self.mag -= unit::convert(&rhs.mag, &rhs.units, &self.units)?;
                    }
                    Multiply => {
                        self.units *= rhs.units;
                        self.mag *= rhs.mag;
                    }
                    Divide => {
                        self.units /= rhs.units;
                        self.mag /= rhs.mag;
                    }
                    Raise | UnaryAdd | UnarySubtract | Assign | AssignUnit | ConvertUnits => {
                        unreachable!()
                    }
                }
                Ok(self.into())
            }

            Value::Number(rhs) => match op {
                Add | Subtract => Err(EvalError::InvalidBinaryOperation(
                    Box::new(self.into()),
                    Box::new(rhs.into()),
                    op,
                )),

                // We've already implemented Value * Quantity, so we can call
                // that for Quantity * Value
                Multiply => rhs.binary_op(op, self.into()),

                Divide => {
                    self.mag /= rhs;
                    Ok(self.into())
                }

                Raise => {
                    if rhs.is_integer() {
                        if let Some(exp) = rhs.to_i32() {
                            self.units.pow_assign(exp);
                            Ok(self.into())
                        } else {
                            Err(EvalError::NonIntegerExponent(Box::new(rhs.into())))
                        }
                    } else {
                        Err(EvalError::NonIntegerExponent(Box::new(rhs.into())))
                    }
                }

                UnaryAdd | UnarySubtract | Assign | AssignUnit | ConvertUnits => unreachable!(),
            },
            Value::Unit(rhs) => match op {
                Multiply => {
                    self.units *= rhs;
                    Ok(self.into())
                }
                Divide => {
                    self.units /= rhs;
                    Ok(self.into())
                }
                Add | Subtract => Err(EvalError::InvalidBinaryOperation(
                    Box::new(self.into()),
                    Box::new(rhs.into()),
                    op,
                )),
                Raise => Err(EvalError::NonIntegerExponent(Box::new(rhs.into()))),
                UnaryAdd | UnarySubtract | Assign | AssignUnit | ConvertUnits => unreachable!(),
            },
        }
    }

    fn unary_op(mut self, op: Operation) -> Result<Value, EvalError> {
        use Operation::*;
        match op {
            UnaryAdd => Ok(self.into()),
            UnarySubtract => {
                self.mag = -self.mag;
                Ok(self.into())
            }
            Add | Subtract | Multiply | Divide | Raise | Assign | AssignUnit | ConvertUnits => {
                Err(EvalError::ExpectedOperation("unary", "binary", op))
            }
        }
    }
}

impl Operable for Value {
    fn binary_op(self, op: Operation, rhs: Value) -> Result<Value, EvalError> {
        match self {
            Value::Number(lhs) => lhs.binary_op(op, rhs),
            Value::Quantity(lhs) => lhs.binary_op(op, rhs),
            Value::Unit(lhs) => lhs.binary_op(op, rhs),
        }
    }

    fn unary_op(self, op: Operation) -> Result<Value, EvalError> {
        match self {
            Value::Number(operand) => operand.unary_op(op),
            Value::Quantity(operand) => operand.unary_op(op),
            Value::Unit(operand) => operand.unary_op(op),
        }
    }
}

impl Operable for Units {
    fn binary_op(mut self, op: Operation, rhs: Value) -> Result<Value, EvalError> {
        use Operation::*;

        if let UnaryAdd | UnarySubtract = op {
            return Err(EvalError::ExpectedOperation("binary", "unary", op));
        }

        if let Assign | AssignUnit = op {
            return Err(EvalError::InvalidAssignmentLHS);
        }

        match rhs {
            Value::Quantity(mut rhs) => match op {
                Multiply => rhs.binary_op(op, self.into()), // delegate to Quantity * Unit
                Divide => {
                    rhs.mag = rhs.mag.recip();
                    rhs.units.pow_assign(-1);
                    rhs.units *= self;
                    Ok(rhs.into())
                }
                Add | Subtract => Err(EvalError::InvalidBinaryOperation(
                    Box::new(self.into()),
                    Box::new(rhs.into()),
                    op,
                )),
                Raise => Err(EvalError::NonIntegerExponent(Box::new(rhs.into()))),
                Assign | AssignUnit | UnaryAdd | UnarySubtract | ConvertUnits => unreachable!(),
            },
            Value::Number(rhs) => match op {
                Raise => {
                    if rhs.is_integer() {
                        if let Some(exp) = rhs.to_i32() {
                            self.pow_assign(exp);
                            Ok(self.into())
                        } else {
                            Err(EvalError::NonIntegerExponent(Box::new(rhs.into())))
                        }
                    } else {
                        Err(EvalError::NonIntegerExponent(Box::new(rhs.into())))
                    }
                }
                Multiply => Ok(Quantity::new(rhs, self).into()),
                Divide => Ok(Quantity::new(rhs.recip(), self).into()),

                Add | Subtract => Err(EvalError::InvalidBinaryOperation(
                    Box::new(self.into()),
                    Box::new(rhs.into()),
                    op,
                )),
                Assign | AssignUnit | UnaryAdd | UnarySubtract | ConvertUnits => unreachable!(),
            },
            Value::Unit(rhs) => match op {
                Multiply => Ok(Value::from(self * rhs)),
                Divide => Ok(Value::from(self / rhs)),
                Add | Subtract => Err(EvalError::InvalidBinaryOperation(
                    Box::new(self.into()),
                    Box::new(rhs.into()),
                    op,
                )),
                Raise => Err(EvalError::NonIntegerExponent(Box::new(rhs.into()))),
                Assign | AssignUnit | UnaryAdd | UnarySubtract | ConvertUnits => unreachable!(),
            },
        }
    }

    fn unary_op(self, op: Operation) -> Result<Value, EvalError> {
        // Unary operations don't make sense on unit types
        Err(EvalError::InvalidUnaryOperation(Box::new(self.into()), op))
    }
}

impl Variable {
    pub fn eval(self, env: &HashMap<String, Value>) -> Result<Value, EvalError> {
        match env.get(self.name()) {
            Some(val) => Ok(val.clone()),
            None => match resolve_unit(self.name(), env) {
                Some(val) => Ok(Units::from([val]).into()),
                None => match try_get_prefix_scale(self.name()) {
                    Some(val) => Ok(val.into()),
                    None => Err(EvalError::UndefinedVariable(self.name().to_owned())),
                },
            },
        }
    }
}

/// Raise a [BigRational] to the power of another [BigRational].
///
/// The RHS must be an integer.
fn bigrational_pow(lhs: BigRational, rhs: BigRational) -> Result<BigRational, EvalError> {
    if rhs.is_integer() {
        match rhs.numer().to_i32() {
            Some(exp) => Ok(lhs.pow(exp)),
            None => Err(EvalError::NonIntegerExponent(Box::new(rhs.into()))),
        }
    } else {
        Err(EvalError::NonIntegerExponent(Box::new(rhs.into())))
    }
}

impl Node {
    /// Evaluate this node.
    ///
    /// # Examples
    ///
    /// ```
    /// # use num::{BigInt, BigRational};
    /// # use numbrs::{
    /// #     ast::{BinaryExpression, Node, Value},
    /// #     operation::Operation,
    /// # };
    /// # use std::collections::HashMap;
    /// # macro_rules! rat {
    /// #     ( $a:expr ) => {
    /// #         ::num::BigRational::from_integer(::num::BigInt::from($a))
    /// #     };
    /// # }
    /// let tree = Node::from(BinaryExpression::new(
    ///     Operation::Multiply,
    ///     Node::from(rat!(2)),
    ///     Node::from(rat!(3))
    /// ));
    /// let mut env = HashMap::new();
    /// match tree.eval(&mut env).unwrap() {
    ///     Value::Number(val) => {
    ///         assert_eq!(val, rat!(6))
    ///     }
    ///     _ => unreachable!(),
    /// }
    /// ```
    pub fn eval(self, env: &mut HashMap<String, Value>) -> Result<Value, EvalError> {
        match self {
            Node::BinaryExpression(node) => {
                if let Operation::Assign | Operation::AssignUnit = node.operation {
                    // Handle variable assignment here because it requires the
                    // LHS Node and not its result value.
                    let unit = matches!(node.operation, Operation::AssignUnit);
                    if let Node::Variable(var) = *node.lhs {
                        handle_assignment(var.name(), *node.rhs, unit, env)
                    } else {
                        Err(EvalError::InvalidAssignmentLHS)
                    }
                } else if let Operation::ConvertUnits = node.operation {
                    // Handle unit conversion here because it's the same for all
                    // LHS types.
                    let lhs = Quantity::from(node.lhs.eval(env)?);
                    let rhs = node.rhs.eval(env)?;
                    if let Value::Unit(units) = rhs {
                        Ok(lhs.convert_to(units)?.into())
                    } else {
                        Err(EvalError::InvalidBinaryOperation(
                            Box::new(lhs.into()),
                            Box::new(rhs),
                            node.operation,
                        ))
                    }
                } else {
                    let lval = node.lhs.eval(env)?;
                    let rval = node.rhs.eval(env)?;
                    lval.binary_op(node.operation, rval)
                }
            }
            Node::UnaryExpression(node) => {
                let val = node.expr.eval(env)?;
                val.unary_op(node.operation)
            }
            Node::Variable(var) => var.eval(env),

            Node::Number(num) => Ok(num.into()),
            Node::Quantity(q) => Ok(q.into()),
        }
    }
}

/// Handle a variable assignment.
///
/// Returns the [Value] of the RHS wrapped in a [Result], or an [EvalError] on
/// failure.
fn handle_assignment(
    name: &str,
    rhs: Node,
    unit: bool,
    env: &mut HashMap<String, Value>,
) -> Result<Value, EvalError> {
    // If RHS = Variable("_"), remove LHS from environment
    if let Node::Variable(rvar) = &rhs {
        if rvar.name() == Runtime::UNASSIGN_IDENT {
            env.remove(name);
            return Ok(BigRational::zero().into());
        }
    }

    // Else, assign LHS and return value of RHS
    let mut rval: Value = rhs.eval(env)?;

    // For unit assignments, coerce RHS to a new Unit
    if unit {
        rval = Value::from(Units::from([match rval {
            Value::Quantity(q) => Unit::new(
                name,
                1,
                q.mag * q.units.aggregate_scales(),
                rat!(0),
                q.units.dimension(),
            ),
            Value::Unit(units) => units.collapse_to(name),
            Value::Number(rat) => Unit::new(name, 1, rat, rat!(0), Dimension::new()),
        }]))
    }

    env.insert(name.to_owned(), rval.clone());
    Ok(rval)
}

#[derive(Error, Debug)]
pub enum EvalError {
    #[error("Invalid operation `{2}` for types {0} and {1}")]
    InvalidBinaryOperation(Box<Value>, Box<Value>, Operation),

    #[error("Cannot convert between units that describe different quantities: `{0}` and `{1}`")]
    ConvertNonConformingUnits(Box<Units>, Box<Units>),

    #[error("Expected {0} operation, found {1} operation `{2}`")]
    ExpectedOperation(&'static str, &'static str, Operation),

    #[error("`{0}` not defined")]
    UndefinedVariable(String),

    #[error("Only integer number exponents are supported, not `{0}`")]
    NonIntegerExponent(Box<Value>),

    #[error("Invalid LHS for assignment operation")]
    InvalidAssignmentLHS,

    #[error("Invalid unary operation `{1}` for type `{0}`")]
    InvalidUnaryOperation(Box<Value>, Operation),
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use num::Zero;
    use pretty_assertions::assert_eq;

    use super::*;
    use crate::rat_util_macros::rat;

    /// Test evaluation of variables from the environment map
    #[test]
    fn variable_eval() {
        let mut env: HashMap<String, Value> = HashMap::new();

        let value = BigRational::zero();
        env.insert("zero".to_string(), value.clone().into());
        let var = Variable::from("zero");
        match var.eval(&env).unwrap() {
            Value::Number(rat) => assert_eq!(rat, value),
            _ => panic!("Unexpected Value variant"),
        }

        let value = rat!(1);
        env.insert(
            "size".to_string(),
            Quantity::new(value.clone(), Units::new()).into(),
        );
        let var = Variable::from("size");
        let result = var.eval(&env).unwrap();
        assert!(matches!(result, Value::Quantity(_)));
        let result = if let Value::Quantity(q) = result {
            q
        } else {
            unreachable!()
        };
        assert_eq!(result.mag, value);
        assert!(result.units.is_dimensionless());
    }

    /// Test variable assignment
    #[test]
    fn variable_assignment() {
        let mut env: HashMap<String, Value> = HashMap::new();

        // Run the test twice to test both assigning new variables and
        // overwriting existing ones
        for i in 0..2 {
            let value = rat!(i);
            let var: Node = Variable::from("foo").into();

            // assign variable
            let tree = Node::from(BinaryExpression::new(
                Operation::Assign,
                var.clone(),
                value.clone().into(),
            ));

            // verify return value of assignment
            match tree.eval(&mut env).unwrap() {
                Value::Number(rat) => assert_eq!(rat, value),
                _ => unreachable!(),
            }

            // verify new variable exists
            match var.eval(&mut env).unwrap() {
                Value::Number(rat) => assert_eq!(rat, value),
                _ => unreachable!(),
            }
        }
    }

    /// Test variable unassignment
    #[test]
    fn variable_unassignment() {
        let mut env: HashMap<String, Value> = HashMap::new();

        let value = rat!(123);
        let varname = "foo";
        let var: Node = Variable::from(varname).into();
        let unassign_var: Node = Variable::from(Runtime::UNASSIGN_IDENT).into();

        // assign variable
        let tree = Node::from(BinaryExpression::new(
            Operation::Assign,
            var.clone(),
            value.clone().into(),
        ));
        tree.eval(&mut env).unwrap();

        // verify new variable exists
        match var.clone().eval(&mut env).unwrap() {
            Value::Number(rat) => assert_eq!(rat, value),
            _ => unreachable!(),
        }

        // unassign variable
        let tree = Node::from(BinaryExpression::new(
            Operation::Assign,
            var.clone(),
            unassign_var,
        ));

        // verify return value of unassignment
        match tree.eval(&mut env).unwrap() {
            Value::Number(rat) => assert_eq!(rat, rat!(0)),
            _ => unreachable!(),
        }

        // verify variable doesn't exist
        match var.eval(&mut env) {
            Err(EvalError::UndefinedVariable(name)) if name == varname => (), // success
            Err(_) => panic!("Unexpected error occurred"),
            Ok(_) => panic!("Variable exists"),
        }
    }
    /// Test binary operations between number nodes
    #[test]
    fn binary_ops_between_numbers() {
        /// Create binary expression test case tuples
        ///
        /// # Example
        /// ```
        /// let cases: Vec<(&str, Node, BigRational)> = binop_cases! {
        ///     1 + 2 = 3,
        ///     4 - 5 = -1,
        ///     6 * 7 = 42,
        ///     64 / 4 = 16,
        /// };
        /// ```
        macro_rules! binop_cases {
            ( $( $b:literal $a:tt $c:literal = $d:literal$(/$e:literal)? ),+ $(,)? ) => {
                vec![ $(
                    (
                        stringify!($b $a $c = $d$(/$e)?),
                        Node::BinaryExpression(BinaryExpression {
                            operation: stringify!($a).try_into().unwrap(),
                            lhs: Box::new(rat!($b).into()),
                            rhs: Box::new(rat!($c).into()),
                        }),
                        rat!($d $(, $e)?)
                    ),
                )+ ]
            }
        }

        let cases: Vec<(&str, Node, BigRational)> = binop_cases! {
            1 + 2 = 3,
            2 * 3 = 6,
            2 / 2 = 1,
            3398475 - 75 = 3398400,
            -1 + 2 = 1,
            -16 * 4 = -64,
            4 - 5 = -1,
            6 * 7 = 42,
            64 / 4 = 16,
            2 ^ 3 = 8,
            4 ^ -1 = 1/4,
        };

        let mut env = HashMap::new();
        for (repr, node, expected_value) in cases {
            println!("Test case: {}", repr);
            let res = node.eval(&mut env).unwrap();
            match res {
                Value::Number(result_value) => assert_eq!(result_value, expected_value),
                _ => unreachable!(),
            }
        }
    }

    /// Test unary operations with numbers
    #[test]
    fn unary_ops_with_numbers() {
        macro_rules! unop_cases {
            ( $( $a:tt $b:literal = $c:literal $( / $d:literal )? ),+ $(,)? ) => {
                vec![ $(
                    (
                        stringify!($a $b = $c$(/$d)?),
                        Node::UnaryExpression(UnaryExpression {
                            operation: Operation::unary_try_from(stringify!($a)).unwrap(),
                            expr: Box::new(rat!($b).into())
                        }),
                        rat!($c $(/ $d)?)
                    ),
                )+ ]
            };
        }

        let cases: Vec<(&str, Node, BigRational)> = unop_cases! {
            - 4 = -4,
            + -4 = -4,
            - -4 = 4,
            + 0 = 0,
            - 0 = 0,
        };

        let mut env = HashMap::new();
        for (repr, node, expected_value) in cases {
            println!("Test case: {}", repr);
            match node.eval(&mut env).unwrap() {
                Value::Number(result_value) => assert_eq!(result_value, expected_value),
                _ => unreachable!(),
            }
        }
    }

    // TODO: test evaluation of quantities
    // TODO: test variable unassignment
}
