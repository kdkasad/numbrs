/*

ast.rs - Abstract syntax tree for Numbrs
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
# Abstract syntax tree (AST)

Numbrs represents mathematical expressions as a tree of [nodes][1]. Each
node is either an expression, a variable, a number, or a quantity.

An expression ([`BinaryExpression`] or [`UnaryExpression`]) is an node which
has one or two children nodes and performs an operation on the child(ren).

A number is represented using the [`BigRational`][2] type from the [num][3]
crate.

A [`Variable`] contains a string which, when evaluated, is looked up in the
environment and replaced with the [Value] stored under the variable's name.

A [`Quantity`] consists of a number and a [`Units`][4] object, which
represents units of a physical quantity.

An expression tree can be evaluated. When evaluation succeeds, the result is
a [`Value`], which is either a number ([`BigRational`][2]), a [`Quantity`],
or a [`Units`] object.

See the [`parser` module][5] for a way to create expression trees from
text-based expression syntax.

[1]: self::Node
[2]: num::BigRational
[3]: https://crates.io/crates/num
[4]: crate::unit::Units
[5]: crate::parser
*/

use std::fmt::{self, Display};

use num::BigRational;

use crate::{operation::Operation, rat_util_macros::rat, unit::Units};

/// # Physical quantity
///
/// Represents an amount of some physical quantity, e.g. *5 meters*.
///
/// A quantity has a magnitude (numerical value) and zero or more units,
/// represented with the [Units] type.
///
/// In the example of *5 meters*, *5* is the magnitude and *meters* is the unit.
///
/// It is often useful to have a single quantity with multiple units, for
/// example *25 miles/hour*. This quantity has a magnitude of *25*, and two
/// units, *miles* and *hours^-1* (or *1/hours*). Note that the *hours* unit has
/// an exponent of *-1*. Quantities often contain units with exponents other
/// than *1*.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Quantity {
    pub(crate) mag: BigRational,
    pub(crate) units: Units,
}

impl Quantity {
    /// Tests whether this quantity is a numerical value.
    ///
    /// A quantity is a numerical value if either of the following are true:
    ///  - It has no units
    ///  - All of its units are dimensionless
    pub fn is_value(&self) -> bool {
        self.units.is_dimensionless()
    }

    /// Create a new [Quantity].
    pub fn new(mag: BigRational, units: Units) -> Self {
        Self { mag, units }
    }
}

impl Display for Quantity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.mag, self.units)
    }
}

impl From<Value> for Quantity {
    /// Convert a [Value] to a [Quantity].
    ///
    /// If the [Value] is a unit, it is given a magnitude of 1.
    /// If the [Value] is a number, it is given a dimensionless unit.
    fn from(src: Value) -> Self {
        match src {
            Value::Quantity(quantity) => quantity,
            Value::Number(rat) => Quantity::new(rat, Units::new()),
            Value::Unit(units) => Quantity::new(rat!(1), units),
        }
    }
}

/// # Variable
///
/// A variable is a placeholder for a value. When a value is re-used several
/// times, it can be saved as a variable and referenced by the variable's name.
///
/// A [`Variable`] contains a [`String`] which holds the variable's name. The
/// name can be any string, however the [parser][1] only recognizes variable
/// names that contain just letters, numbers, and underscores and do not start
/// with a number. This means only variables with names that fit those criteria
/// will be usable in expressions that are parsed from text (see the [parser][1]
/// module).
///
/// [1]: crate::parser
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Variable(String);

impl Variable {
    /// Returns the name of the [`Variable`]
    pub fn name(&self) -> &str {
        &self.0
    }
}

impl<T: ToString> From<T> for Variable {
    fn from(name: T) -> Self {
        Self(name.to_string())
    }
}

/// # Expression with one operand
///
/// A [`UnaryExpression`] is an [AST node][1] type which performs an operation
/// on just one operand (hence the term *unary* in the name).
///
/// The main use of this is for negation, i.e. unary subtraction. The expression
/// *-5* would be interpreted as a [`UnaryExpression`] with the operand being a
/// [`Node::Number`] with a value of *5* and the [`Subtract`][2] operation.
///
/// [1]: crate::ast
/// [2]: crate::operation::Operation::Subtract
#[derive(Clone, Debug, PartialEq)]
pub struct UnaryExpression {
    pub(crate) operation: Operation,
    pub(crate) expr: Box<Node>,
}

impl UnaryExpression {
    /// Create a new [`UnaryExpression`].
    ///
    /// Moves the operand `expr` into the expression node and sets the operation
    /// to `operation`.
    pub fn new(operation: Operation, expr: Node) -> Self {
        Self {
            operation,
            expr: Box::new(expr),
        }
    }
}

/// # Expression with two operands
///
/// A [`BinaryExpression`] is an [AST node][1] type which performs an operation
/// on two operands (hence the term *binary*).
///
/// For operations in which the order of operands matters (e.g. subtraction),
/// the operation is performed as LHS (left hand side) *operation* RHS (right
/// hand side). So with the example of subtraction, it's *LHS -- RHS*.
///
/// [1]: crate::ast
#[derive(Clone, Debug, PartialEq)]
pub struct BinaryExpression {
    pub(crate) operation: Operation,
    pub(crate) lhs: Box<Node>,
    pub(crate) rhs: Box<Node>,
}

impl BinaryExpression {
    /// Create a new [`BinaryExpression`].
    ///
    /// Moves the given `lhs` (left hand side) operand and `rhs` (right hand
    /// side) operand into the expression node, and sets the operation to
    /// perform to `operation`.
    pub fn new(operation: Operation, lhs: Node, rhs: Node) -> Self {
        Self {
            operation,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}

/// # Expression result value
///
/// Evaluating an expression returns a [`Value`], which is either a
/// [`Quantity`], a number ([`BigRational`]), or a [`Units`] type.
///
/// The [`Value`] type also represents the value of a [`Variable`] in the
/// environment.
#[derive(Clone, Debug, strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum Value {
    /// Amount of a physical quantity.
    ///
    /// See [`ast::Quantity`][Quantity].
    Quantity(Quantity),

    /// Numeric value.
    ///
    /// Stored as a [`BigRational`]
    Number(BigRational),

    /// Unit of a physical quantity.
    ///
    /// See [`Units`].
    Unit(Units),
}

impl From<Quantity> for Value {
    fn from(src: Quantity) -> Self {
        Value::Quantity(src)
    }
}

impl From<BigRational> for Value {
    fn from(src: BigRational) -> Self {
        Value::Number(src)
    }
}

impl From<Units> for Value {
    fn from(src: Units) -> Self {
        Value::Unit(src)
    }
}

/// # AST Node
///
/// See the [`ast` module][crate::ast] for an explanation.
#[derive(Clone, Debug, strum_macros::Display, PartialEq)]
#[strum(serialize_all = "lowercase")]
pub enum Node {
    /// Expression with one operand
    #[strum(serialize = "unary expression")]
    UnaryExpression(UnaryExpression),

    /// Expression with two operands
    #[strum(serialize = "binary expression")]
    BinaryExpression(BinaryExpression),

    /// Numerical variable
    ///
    /// I.e. a named placeholder for a value
    Variable(Variable),

    /// Number
    ///
    /// Stored as a [`BigRational`].
    Number(BigRational),

    /// Amount of a physical quantity
    ///
    /// See [`ast::Quantity`][Quantity].
    Quantity(Quantity),
}

impl From<BigRational> for Node {
    fn from(rat: BigRational) -> Node {
        Node::Number(rat)
    }
}

macro_rules! node_from_subtype {
    ( $subtype:ident ) => {
        impl From<$subtype> for Node {
            fn from(src: $subtype) -> Node {
                Node::$subtype(src)
            }
        }
    };
}

node_from_subtype!(UnaryExpression);
node_from_subtype!(BinaryExpression);
node_from_subtype!(Variable);
node_from_subtype!(Quantity);

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn value_repr() {
        assert_eq!(Value::Number(BigRational::default()).to_string(), "number");
        assert_eq!(
            Value::Quantity(Quantity::new(BigRational::default(), Units::new())).to_string(),
            "quantity"
        );
    }
}
