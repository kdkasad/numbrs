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

//! Abstract syntax tree for Numbrs

use std::fmt::{self, Display};

use num::BigRational;
use thiserror::Error;

use crate::{operation::Operation, unit::UnitList};

/// Represents a quantity
///
/// A quantity has a magnitude and zero or more units.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Quantity {
    pub(crate) mag: BigRational,
    pub(crate) units: UnitList,
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
}

impl Display for Quantity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.mag, self.units)
    }
}

/// Varible type.
///
/// The [String] parameter is the variable name.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Variable(pub(crate) String);

impl From<String> for Variable {
    fn from(name: String) -> Self {
        Self(name)
    }
}

/// Expression with unary operator
#[derive(Clone, Debug, PartialEq)]
pub struct UnaryExpression {
    pub(crate) operation: Operation,
    pub(crate) expr: Box<Node>,
}

impl UnaryExpression {
    pub fn new(operation: Operation, expr: Node) -> Self {
        Self {
            operation,
            expr: Box::new(expr),
        }
    }
}

/// Expression with binary operator
#[derive(Clone, Debug, PartialEq)]
pub struct BinaryExpression {
    pub(crate) operation: Operation,
    pub(crate) lhs: Box<Node>,
    pub(crate) rhs: Box<Node>,
}

impl BinaryExpression {
    pub fn new(operation: Operation, lhs: Node, rhs: Node) -> Self {
        Self {
            operation,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}

/// Value type
///
/// Values are returned from functions that evaluate nodes
#[derive(Clone, Debug, strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum Value {
    Quantity(Quantity),
    Number(BigRational),
}

impl Value {
    pub fn magnitude(&self) -> &BigRational {
        match self {
            Value::Quantity(q) => &q.mag,
            Value::Number(n) => n,
        }
    }
}

impl TryFrom<Node> for Value {
    type Error = Error;

    fn try_from(node: Node) -> Result<Self, Self::Error> {
        match node {
            Node::Number(num) => Ok(Value::Number(num)),
            Node::Quantity(q) => Ok(Value::Quantity(q)),
            _ => Err(Error::ValueConversionError(node)),
        }
    }
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

#[derive(Clone, Debug, strum_macros::Display, PartialEq)]
#[strum(serialize_all = "lowercase")]
pub enum Node {
    #[strum(serialize = "unary expression")]
    UnaryExpression(UnaryExpression),
    #[strum(serialize = "binary expression")]
    BinaryExpression(BinaryExpression),
    Variable(Variable),
    Number(BigRational),
    Quantity(Quantity),
}

impl From<Value> for Node {
    fn from(value: Value) -> Node {
        match value {
            Value::Quantity(q) => Node::from(q),
            Value::Number(n) => Node::from(n),
        }
    }
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

#[derive(Error, Debug)]
pub enum Error {
    #[error("Node is not a value: {:?}", .0)]
    ValueConversionError(Node),
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn value_repr() {
        assert_eq!(Value::Number(BigRational::default()).to_string(), "number");
        assert_eq!(
            Value::Quantity(Quantity {
                mag: BigRational::default(),
                units: UnitList::new()
            })
            .to_string(),
            "quantity"
        );
    }
}
