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

impl Node {
    pub fn variant_name(&self) -> &'static str {
        match self {
            Node::Binary { .. } => "Node::Binary",
            Node::Unary { .. } => "Node::Unary",
            Node::Number(_) => "Node::Number",
        }
    }
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

    // not recognizable by scanner but used by parser
    UnaryAdd,
    UnarySubtract,
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
            UnaryAdd | UnarySubtract => 60,
        }
    }

    pub fn associativity(&self) -> Associativity {
        use Operation::*;
        match self {
            Add | Subtract | Multiply | Divide => Associativity::Left,
            UnaryAdd | UnarySubtract => Associativity::Left, // doesn't matter (I think)
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
                Add | UnaryAdd => '+',
                Subtract | UnarySubtract => '-',
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
