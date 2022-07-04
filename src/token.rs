use crate::ast::Operation;
use crate::scanner;
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Illegal(scanner::Error),

    Operator(Operation),
    LParen,
    RParen,

    Number(NumberBase, String),
    Ident(String),
}

impl From<Operation> for Token {
    fn from(op: Operation) -> Self {
        Token::Operator(op)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum NumberBase {
    Binary = 2,
    Decimal = 10,
    Hexadecimal = 16,
}

impl fmt::Display for NumberBase {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use NumberBase::*;
        write!(
            f,
            "{} (base {})",
            match self {
                Binary => "binary",
                Decimal => "decimal",
                Hexadecimal => "Hexadecimal",
            },
            *self as u32
        )
    }
}
