use decimal::d128;
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
    Number(d128),
}

impl From<d128> for Node {
    fn from(value: d128) -> Self {
        Node::Number(value)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Operation {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use Operation::*;
        write!(
            f,
            "{}",
            match self {
                Add => '+',
                Subtract => '-',
                Multiply => '*',
                Divide => '/',
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
            _ => Err(OperationError::Parse(c)),
        }
    }
}

#[derive(Error, Debug)]
pub enum OperationError {
    #[error("invalid operation character '{0}'")]
    Parse(char),
}
