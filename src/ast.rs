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

use bigdecimal::BigDecimal;
use std::fmt;
use thiserror::Error;

#[derive(Debug, PartialEq)]
pub enum Node {
    Binary {
        operation: Operation,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    Unary {
        operation: Operation,
        expr: Box<Node>,
    },
    Number(BigDecimal),
    Variable(String),
}

impl Node {
    pub fn variant_name(&self) -> &'static str {
        match self {
            Node::Binary { .. } => "Node::Binary",
            Node::Unary { .. } => "Node::Unary",
            Node::Number(_) => "Node::Number",
            Node::Variable(_) => "Node::Variable",
        }
    }
}

impl From<BigDecimal> for Node {
    fn from(value: BigDecimal) -> Self {
        Node::Number(value)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Operation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Exponent,

    // not recognizable by scanner but used by parser
    UnaryAdd,
    UnarySubtract,

    Assign, // assignment operator ':='
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Associativity {
    Left,
    Right,
}

impl Operation {
    pub fn precedence(&self) -> u32 {
        use Operation::*;
        match self {
            Add | Subtract => 20,
            Multiply | Divide => 40,
            Exponent => 50,
            UnaryAdd | UnarySubtract => 60,
            Assign => 100,
        }
    }

    pub fn associativity(&self) -> Associativity {
        use Operation::*;
        match self {
            Add | Subtract | Multiply | Divide => Associativity::Left,
            Exponent => Associativity::Right,
            UnaryAdd | UnarySubtract => Associativity::Left, // doesn't matter (I think)
            Assign => Associativity::Right,                  // "a := b := c" == "a := (b := c)"
        }
    }
}

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use Operation::*;
        write!(
            f,
            "{}",
            match self {
                Add | UnaryAdd => "+",
                Subtract | UnarySubtract => "-",
                Multiply => "*",
                Divide => "/",
                Assign => ":=",
                Exponent => "^",
            }
        )
    }
}

impl TryFrom<char> for Operation {
    type Error = OperationError;
    fn try_from(c: char) -> Result<Self, <Self as TryFrom<char>>::Error> {
        use Operation::*;
        match c {
            '+' => Ok(Add),
            '-' => Ok(Subtract),
            '*' => Ok(Multiply),
            '/' => Ok(Divide),
            '^' => Ok(Exponent),
            _ => Err(OperationError::Parse(c)),
        }
    }
}

#[derive(Error, Debug)]
pub enum OperationError {
    #[error("invalid operation character '{0}'")]
    Parse(char),
}
