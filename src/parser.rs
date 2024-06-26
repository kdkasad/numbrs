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

//! # Expression parsing
//!
//! Handles parsing a token stream generated by a [`Lexer`] into an
//! [AST][crate::ast].

use std::{iter::Peekable, str::FromStr};

use num::BigRational;
use thiserror::Error;

use crate::{
    ast::{BinaryExpression, FunctionCall, Node, UnaryExpression, Variable},
    functions::Function,
    lexer::{Lexer, Token},
    operation::Operation,
};

/// # Expression parser
///
/// Used to parse a [`Token`] stream into an [AST][crate::ast].
///
/// This type is generic over `Iterator<Item = Token>`, so it can be used to
/// parse any stream of tokens.
///
/// A default [`Parser::new()`] function exists which takes a string reference
/// and creates a [`Lexer`] over it, then creates a [`Parser`] over that.
/// `From<Lexer<'_>>` is also implemented, so the caller can manually create a
/// [`Lexer`] if needed.
///
/// To perform the parsing, call [`Parser::parse()`].
#[derive(Debug)]
pub struct Parser<I>
where
    I: Iterator<Item = Token>,
{
    tokens: Peekable<I>,
}

impl<'a> Parser<Lexer<'a>> {
    /// Create a new [`Parser`] for a string slice.
    ///
    /// Under the hood, this creates a new [`Lexer`] for the string and then
    /// calls [`Parser::from()`] on that.
    pub fn new(src: &'a str) -> Self {
        Self::from(Lexer::new(src))
    }
}

impl<I> From<I> for Parser<I>
where
    I: Iterator<Item = Token>,
{
    /// Create a new [`Parser`] from a [`Lexer`].
    fn from(token_steam: I) -> Self {
        Self {
            tokens: token_steam.peekable(),
        }
    }
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    /// Parse the input token stream into an [AST][1].
    ///
    /// Returns the root [`Node`] on success and a [`ParseError`] on failure.
    ///
    /// [1]: crate::ast
    pub fn parse(mut self) -> Result<Node, ParseError> {
        let result = self.parse_expr(0)?;
        if let Some(token) = self.tokens.next() {
            // The only cases where this will happen is if the next token is a group close token or a list separator
            Err(match token {
                Token::GroupEnd => ParseError::UnmatchedGroup,
                Token::ListSeparator(c) => ParseError::IllegalToken(c),
                token => unreachable!("Expected end of input, found token: {:?}", token),
            })
        } else {
            Ok(result)
        }
    }

    fn parse_expr(&mut self, min_bp: u32) -> Result<Node, ParseError> {
        let mut lhs: Node = match self.tokens.next() {
            Some(tok) => match tok {
                Token::Number(numstr) => Node::Number(str_to_num(&numstr)?),
                Token::Ident(name) => {
                    if let Some(Token::GroupBegin) = self.tokens.peek() {
                        // Next token is a group opener, so treat this as a
                        // function call if the ident matches a function name.
                        if let Ok(function) = Function::from_str(&name) {
                            // Consume group begin
                            self.tokens.next();
                            let mut args = Vec::new();
                            loop {
                                let arg = self.parse_expr(0)?;
                                args.push(arg);
                                // Consume separator or group end
                                match self.tokens.next() {
                                    Some(Token::GroupEnd) => break,
                                    Some(Token::ListSeparator(_)) => (),
                                    _ => return Err(ParseError::UnmatchedGroup),
                                }
                            }
                            Node::FunctionCall(FunctionCall::new(function, args))
                        } else {
                            Node::Variable(Variable::from(name))
                        }
                    } else {
                        Node::Variable(Variable::from(name))
                    }
                }
                Token::GroupBegin => {
                    // when left paren encountered, parse sub-expression
                    let lhs = self.parse_expr(0)?;
                    // consume right paren
                    match self.tokens.next() {
                        Some(Token::GroupEnd) => (),
                        Some(Token::ListSeparator(c)) => return Err(ParseError::IllegalToken(c)),
                        _ => return Err(ParseError::UnmatchedGroup),
                    }
                    lhs
                }
                Token::Operator(op) => {
                    if let Some(op) = op.try_to_unary() {
                        let ((), r_bp) = prefix_binding_power(op);
                        let expr = self.parse_expr(r_bp)?;
                        Node::from(UnaryExpression {
                            operation: op,
                            expr: Box::new(expr),
                        })
                    } else {
                        return Err(ParseError::ExpectedOperand(tok.into()));
                    }
                }
                Token::GroupEnd | Token::ListSeparator(_) => {
                    return Err(ParseError::ExpectedOperand(tok.into()))
                }
                Token::Illegal(c) => return Err(ParseError::IllegalToken(c)),
            },
            None => return Err(ParseError::EndOfStream),
        };

        // Process operations until end of input stream
        while let Some(tok) = self.tokens.peek() {
            let mut implicit = false;
            let op: Operation = match *tok {
                Token::Operator(op) => op,
                Token::Ident(_) | Token::GroupBegin => {
                    implicit = true;
                    Operation::Multiply
                }
                Token::GroupEnd | Token::ListSeparator(_) => break,
                Token::Number(_) => return Err(ParseError::ExpectedToken("operator", tok.into())),
                Token::Illegal(c) => return Err(ParseError::IllegalToken(c)),
            };

            let (l_bp, r_bp) = infix_binding_power(op);
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

fn infix_binding_power(op: Operation) -> (u32, u32) {
    use Operation::*;
    match op {
        Assign | AssignUnit => (105, 10),
        Add | Subtract => (30, 35),
        ConvertUnits => (40, 45),
        Multiply | Divide => (50, 55),
        Raise => (60, 65),
        UnaryAdd | UnarySubtract => {
            panic!("Expected (binary) infix operator, got unary operator")
        }
    }
}

fn prefix_binding_power(op: Operation) -> ((), u32) {
    use Operation::*;
    match op {
        UnaryAdd | UnarySubtract => ((), 110),
        Add | Subtract | Multiply | Divide | Raise | Assign | AssignUnit | ConvertUnits => {
            panic!("Expected (unary) prefix operator, got (binary) infix operator")
        }
    }
}

/// # Parser error
///
/// Represents an error that occurred during parsing of a [`Token`] stream.
#[derive(Error, Debug, PartialEq, Eq)]
pub enum ParseError {
    /// # Unexpected end of stream
    ///
    /// Occurs when the [`Parser`]'s token stream ends while it is still
    /// expecting more tokens.
    #[error("Expected token, found end of stream")]
    EndOfStream,

    /// # Expected operand
    ///
    /// Occurs when an operand-like token was expected but some other token was
    /// found.
    #[error("Expected operand, found {0} token instead")]
    ExpectedOperand(&'static str),

    /// # Expected other token
    ///
    /// Occurs when a specific kind of token is expected, but another kind is
    /// found.
    #[error("Expected {0} token, found {1} token instead")]
    ExpectedToken(&'static str, &'static str),

    /// # Illegal token
    ///
    /// Occurs when the token stream contains an illegal token
    /// ([`Token::Illegal`]).
    #[error("Illegal token '{0}' in input")]
    IllegalToken(char),

    /// # Failed to parse number literal
    ///
    /// Occurs when a [`Token::Number`] is found but it contains a string which
    /// cannot be parsed into a number.
    #[error("Failed to parse '{0}' as a number literal")]
    ParseNumberLiteral(String),

    /// # Unmatched group
    ///
    /// Occurs when unmatched parentheses or braces are found in the token
    /// stream.
    #[error("Unmatched group (possibly missing parenthesis)")]
    UnmatchedGroup,
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
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
            ( (f: $($e:tt)+ ) ) => {
                func!( $($e)+ )
            };
            ( ( $($e:tt)+ ) ) => {
                binexpr!( $($e)+ )
            };
            ( $num:literal ) => { Node::Number(BigRational::from_float($num as f64).unwrap()) };
            ( $name:ident ) => { Node::Variable(Variable::from(stringify!($name))) };
        }

        macro_rules! func {
            ( $name:ident $( $arg:tt ),+ ) => {
                Node::FunctionCall(FunctionCall {
                    function: Function::from_str(stringify!($name)).unwrap(),
                    args: vec![$( subexpr!($arg), )+],
                })
            };
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
            ("a * b = c + d", binexpr!("*" a ("=" b ("+" c d)))),
            ("1.5", subexpr!(1.5)),
            (".0125", subexpr!(0.0125)),
            ("180.", subexpr!(180)),
            // TODO: add tests for units
            ("1/360 circle", binexpr!("*" ("/" 1 360) circle)),
            ("sin(1 + 2)", func!(sin ("+" 1 2))),
            (
                "cos ( pi + 3 degrees )",
                func!(cos ("+" pi ("*" 3 degrees))),
            ),
            ("gcd(10 + 5, 12)", func!(gcd ("+" 10 5), 12)),
            ("factorial(1,2,3,4,5)", func!(factorial 1, 2, 3, 4, 5)),
            ("fact(1,2,3,4,5)", func!(fact 1, 2, 3, 4, 5)),
            ("a * b / gcd(a, b)", binexpr!("/" ("*" a b) (f: gcd a, b))),
            (
                "choose(foo = 1 + 2^3)",
                func!(choose ("=" foo ("+" 1 ("^" 2 3)))),
            ),
            (
                "snooze(foo = 1 + 2^3)", // Not a function identifier
                binexpr!("*" snooze ("=" foo ("+" 1 ("^" 2 3)))),
            ),
        ];

        for (input, expected_result) in cases {
            assert_eq!(
                Parser::new(input).parse().unwrap(),
                expected_result,
                "Testing expression: '{}'",
                input
            );
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
            ParseError::EndOfStream => ["", "\t \t\n\n  ", "1 +", "1 + 8 ^ ", "+", "-", "1 * +", "("];
            ParseError::ExpectedOperand("operator") => ["*", "/"];
            ParseError::ExpectedOperand("list separator") => ["sin(,)", ",", "lcm(,12)"];
            ParseError::ExpectedOperand("group close") => [")", "gcd(12,)", "cos()"];
            ParseError::ExpectedToken("operator", _got) => ["1 a 2"];
            ParseError::IllegalToken(_) => ["123 ? 456", "&", "#001", "1234 , 5678"];
            ParseError::ParseNumberLiteral(_) => ["1.2.3"];
            ParseError::UnmatchedGroup => ["1 + (2", "1 + 2)", "1 + (2 + 3", "1 + 2) + 3"];
        }
    }
}
