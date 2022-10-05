/*

parser.rs - Numbrs expression parser
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

//! Expression parser. Parses a token stream generated by the [Lexer] into an
//! [AST][crate::ast].

use std::iter::Peekable;

use num::BigRational;
use thiserror::Error;

use crate::{
    ast::{BinaryExpression, Node, UnaryExpression, Variable},
    lexer::{Lexer, Token},
    operation::Operation,
};

pub struct Parser {
    tokens: Peekable<Lexer>,
}

impl Parser {
    pub fn new(src: &str) -> Self {
        Self {
            tokens: Lexer::new(src).peekable(),
        }
    }

    /// Parse the input token stream into a tree of [Nodes][Node].
    pub fn parse(mut self) -> Result<Node, ParseError> {
        self.parse_expr(0)
    }

    fn parse_expr(&mut self, min_bp: u32) -> Result<Node, ParseError> {
        let mut lhs: Node = match self.tokens.next() {
            Some(tok) => match tok {
                Token::Number(numstr) => Node::Number(str_to_num(&numstr)?),
                Token::Ident(name) => Node::Variable(Variable::from(name)),
                Token::GroupBegin => {
                    // when left paren encountered, parse sub-expression
                    let lhs = self.parse_expr(0)?;
                    // consume right paren
                    match self.tokens.next() {
                        Some(Token::GroupEnd) => (),
                        _ => return Err(ParseError::UnmatchedGroup),
                    }
                    lhs
                }
                Token::Operator(op) => {
                    if let Some(op) = op.try_to_unary() {
                        let ((), r_bp) = op.prefix_binding_power();
                        let expr = self.parse_expr(r_bp)?;
                        Node::from(UnaryExpression {
                            operation: op,
                            expr: Box::new(expr),
                        })
                    } else {
                        return Err(ParseError::ExpectedOperand(tok.into()));
                    }
                }
                Token::GroupEnd => return Err(ParseError::ExpectedOperand(tok.into())),
                Token::Illegal(c) => return Err(ParseError::IllegalToken(c)),
            },
            None => return Err(ParseError::EndOfStream),
        };

        // Process operations until end of input stream
        while let Some(tok) = self.tokens.peek() {
            let mut implicit = false;
            let op: Operation = match *tok {
                Token::Operator(op) => op,
                Token::Ident(_) => {
                    implicit = true;
                    Operation::Multiply
                }
                Token::GroupEnd => break,
                Token::Number(_) | Token::GroupBegin => {
                    return Err(ParseError::ExpectedToken("operator", tok.into()))
                }
                Token::Illegal(c) => return Err(ParseError::IllegalToken(c)),
            };

            let (l_bp, r_bp) = op.infix_binding_power();
            if l_bp < min_bp {
                break;
            }

            // Consume operator token
            if !implicit {
                self.tokens.next();
            }

            let rhs = self.parse_expr(r_bp)?;

            lhs = Node::BinaryExpression(BinaryExpression {
                operation: op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });
        }

        Ok(lhs)
    }
}

fn str_to_num(src: &str) -> Result<BigRational, ParseError> {
    match src.parse::<f64>() {
        Ok(float) => match BigRational::from_float(float) {
            Some(rat) => Ok(rat),
            None => Err(ParseError::ParseNumberLiteral(src.to_owned())),
        },
        Err(_) => Err(ParseError::ParseNumberLiteral(src.to_owned())),
    }
}

pub trait BindingPower {
    fn infix_binding_power(&self) -> (u32, u32);
    fn prefix_binding_power(&self) -> ((), u32);
}

impl BindingPower for Operation {
    fn infix_binding_power(&self) -> (u32, u32) {
        use Operation::*;
        match self {
            Assign | AssignUnit => (12, 1),
            Add | Subtract => (3, 4),
            ConvertUnits => (5, 6),
            Multiply | Divide => (6, 7),
            Raise => (9, 10),
            UnaryAdd | UnarySubtract => {
                panic!("Expected (binary) infix operator, got unary operator")
            }
        }
    }

    fn prefix_binding_power(&self) -> ((), u32) {
        use Operation::*;
        match self {
            UnaryAdd | UnarySubtract => ((), 11),
            Add | Subtract | Multiply | Divide | Raise | Assign | AssignUnit | ConvertUnits => {
                panic!("Expected (unary) prefix operator, got (binary) infix operator")
            }
        }
    }
}

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ParseError {
    #[error("Expected token, found end of stream")]
    EndOfStream,

    #[error("Expected operand, found {0} token instead")]
    ExpectedOperand(&'static str),

    #[error("Expected {0} token, found {1} token instead")]
    ExpectedToken(&'static str, &'static str),

    #[error("Illegal token '{0}' in input")]
    IllegalToken(char),

    #[error("Failed to parse '{0}' as a number literal")]
    ParseNumberLiteral(String),

    #[error("Unmatched group (possibly missing parenthesis)")]
    UnmatchedGroup,
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn parse_valid_expressions() {
        macro_rules! binexpr {
            ( $op:literal $lhs:tt $rhs:tt ) => {
                Node::BinaryExpression(BinaryExpression {
                    operation: Operation::from_str(&$op).unwrap(),
                    lhs: Box::new(subexpr!($lhs)),
                    rhs: Box::new(subexpr!($rhs)),
                })
            };
        }
        macro_rules! subexpr {
            ( ( $($e:tt)+ ) ) => {
                binexpr!( $($e)+ )
            };
            ( $num:literal ) => { Node::Number(BigRational::from_float($num as f64).unwrap()) };
            ( $name:ident ) => { Node::Variable(Variable::from(stringify!($name))) };
        }

        let cases: Vec<(&'static str, Node)> = vec![
            ("1", subexpr!(1)),
            ("var", subexpr!(var)),
            ("1 + 2", binexpr!("+" 1 2)),
            ("1 + 2 * 3", binexpr!("+" 1 ("*" 2 3))),
            (
                "1 + 2 * 3 - 4 ^ 6 / 5",
                binexpr!("-" ("+" 1 ("*" 2 3)) ("/" ("^" 4 6) 5)),
            ),
            (
                "1 + abc * 3 - 4 ^ variable / 5",
                binexpr!("-" ("+" 1 ("*" abc 3)) ("/" ("^" 4 variable) 5)),
            ),
            ("100 m", binexpr!("*" 100 m)),
            ("10 ft + 90 m", binexpr!("+" ("*" 10 ft) ("*" 90 m))),
            ("123 kg m/s^2", binexpr!("/" ("*" ("*" 123 kg) m) ("^" s 2))),
            ("(1 + 2) * 3", binexpr!("*" ("+" 1 2) 3)),
            ("foo = bar", binexpr!("=" foo bar)),
            ("foo = 1 + 2^3", binexpr!("=" foo ("+" 1 ("^" 2 3)))),
            ("a = b = c", binexpr!("=" a ("=" b c))),
            ("1 + a = 2 + 3", binexpr!("+" 1 ("=" a ("+" 2 3)))),
            ("1.5", subexpr!(1.5)),
            (".0125", subexpr!(0.0125)),
            ("180.", subexpr!(180)),
        ];

        for (input, expected_result) in cases {
            println!("Testing expression: '{}'", input);
            assert_eq!(Parser::new(input).parse().unwrap(), expected_result);
        }
    }

    #[test]
    fn parse_invalid_expressions() {
        macro_rules! testfor {
            ( $( $err:pat => [ $($input:literal),+ ] );+ ; ) => { $(
                $(
                    println!("Testing expression '{}'", $input);
                    assert!(matches!(Parser::new($input).parse().unwrap_err(), $err));
                )+
            )+ };
        }

        testfor! {
            ParseError::EndOfStream => ["", "\t \t\n\n  ", "1 +", "1 + 8 ^ ", "+", "-", "1 * +"];
            ParseError::ExpectedOperand("operator") => ["*", "/"];
            ParseError::ExpectedToken("operator", _got) => ["1 a 2"];
            ParseError::IllegalToken(_) => ["123 ? 456", "&", "#001"];
            ParseError::ParseNumberLiteral(_) => ["1.2.3"];
        }
    }
}
