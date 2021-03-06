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

use crate::ast::{Associativity, Node, Operation};
use crate::scanner;
use crate::scanner::Scanner;
use crate::token::{NumberBase, Token};
use bigdecimal::BigDecimal;
use std::fmt;
use std::iter::Peekable;
use std::str::FromStr;
use thiserror::Error;

type Result<T> = std::result::Result<T, Error>;

pub struct Parser<'a> {
    s: Peekable<Scanner<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            s: Scanner::new(src).peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Node> {
        use Expectation::*;
        use Operation::*;
        use Token::*;

        let mut expect: Expectation = OperandToken;
        let mut output: Vec<Node> = Vec::new();
        let mut opstack: Vec<Token> = Vec::new();

        while let Some(tok) = self.s.next() {
            if let Some(Illegal(scanerr)) = self.s.peek() {
                return Err(Error::Scanner(*scanerr));
            }

            match tok {
                Number(base, digits) => {
                    output.push(digits_to_node(&digits, base)?);
                    expect = OperatorToken;
                }

                Operator(Add | Subtract) if expect == OperandToken => {
                    let newtok = Operator(match tok {
                        Operator(Add) => UnaryAdd,
                        Operator(Subtract) => UnarySubtract,
                        _ => unreachable!(),
                    });
                    opstack.push(newtok);
                }

                Operator(_) if expect == OperatorToken => {
                    Self::handle_operator(&mut expect, tok, &mut output, &mut opstack)?;
                }

                LParen if expect == OperandToken => opstack.push(tok),

                RParen if expect == OperatorToken => {
                    Self::handle_rparen(&mut expect, tok, &mut output, &mut opstack)?;
                }

                Ident(_) if expect == OperandToken => {
                    Self::handle_ident(&mut expect, tok, &mut output)?;
                }

                _ => return Err(Error::UnexpectedToken(expect, tok)),
            }
        }

        // if reached end of input and expecting operand, input is invalid
        if expect == OperandToken {
            return Err(Error::UnexpectedEndOfInput(expect));
        }

        Self::process_operator_stack(&mut output, &mut opstack)
    }

    fn handle_ident(expect: &mut Expectation, curtok: Token, output: &mut Vec<Node>) -> Result<()> {
        if *expect == Expectation::OperatorToken {
            return Err(Error::UnexpectedToken(*expect, curtok));
        }

        let name = match curtok {
            Token::Ident(s) => s,
            _ => return Err(Error::UnexpectedToken(*expect, curtok)),
        };
        output.push(Node::Variable(name));

        *expect = Expectation::OperatorToken;
        Ok(())
    }

    fn process_operator_stack(output: &mut Vec<Node>, opstack: &mut Vec<Token>) -> Result<Node> {
        while let Some(top) = opstack.pop() {
            if let Token::LParen = top {
                return Err(Error::UnmatchedParentheses);
            }
            Self::add_to_output(output, top)?;
        }

        if output.len() != 1 {
            return Err(Error::MultipleRoots);
        }

        Ok(output.pop().unwrap())
    }

    fn handle_operator(
        expect: &mut Expectation,
        curtok: Token,
        output: &mut Vec<Node>,
        opstack: &mut Vec<Token>,
    ) -> Result<()> {
        // this check is redundant as handle_operator is never called when expecting an operand
        if *expect == Expectation::OperandToken {
            return Err(Error::UnexpectedToken(*expect, curtok));
        }

        // extract operation from operator token
        let curop = match curtok {
            Token::Operator(op) => op,
            _ => return Err(Error::UnexpectedToken(*expect, curtok)),
        };

        while let Some(top) = opstack.last() {
            // continue only if the top operator is not an opening parenthesis
            if let Token::LParen = top {
                break;
            }

            // extract operation from operator token
            let topop = match top {
                Token::Operator(op) => op,
                _ => return Err(Error::UnexpectedToken(*expect, top.clone())),
            };

            // if the top operator has greater precedence
            // OR
            // the top operator has equal precedence and is the current operator is
            // left-associative
            if topop.precedence() > curop.precedence()
                || (topop.precedence() == curop.precedence()
                    && curop.associativity() == Associativity::Left)
            {
                // unwrap is safe because the loop ensures opstack is not empty:
                Self::add_to_output(output, opstack.pop().unwrap())?;
            } else {
                break;
            }
        }

        opstack.push(curtok);
        *expect = Expectation::OperandToken;

        Ok(())
    }

    fn handle_rparen(
        expect: &mut Expectation,
        curtok: Token,
        output: &mut Vec<Node>,
        opstack: &mut Vec<Token>,
    ) -> Result<()> {
        // this check is redundant as handle_rparen is never called when expecting an operand
        if *expect == Expectation::OperandToken {
            return Err(Error::UnexpectedToken(*expect, curtok));
        }

        // search for left parenthesis
        let mut found = false;
        while let Some(top) = opstack.pop() {
            if let Token::LParen = top {
                found = true;
                break;
            }
            Self::add_to_output(output, top)?;
        }

        if opstack.is_empty() && !found {
            // if stack is empty, no matching parenthesis was found
            return Err(Error::UnmatchedParentheses);
        }

        *expect = Expectation::OperatorToken;
        Ok(())
    }

    fn add_to_output(output: &mut Vec<Node>, tok: Token) -> Result<()> {
        use Operation::*;
        use Token::*;

        match tok {
            Operator(op) => match op {
                UnaryAdd | UnarySubtract => {
                    if output.is_empty() {
                        return Err(Error::MissingOperands(op));
                    }
                    let expr = output.pop().unwrap();
                    output.push(Node::Unary {
                        operation: op,
                        expr: Box::from(expr),
                    });
                }

                _ => {
                    if output.len() < 2 {
                        return Err(Error::MissingOperands(op));
                    }
                    let rhs = output.pop().unwrap();
                    let lhs = output.pop().unwrap();

                    output.push(Node::Binary {
                        lhs: Box::from(lhs),
                        rhs: Box::from(rhs),
                        operation: op,
                    });
                }
            },

            _ => return Err(Error::UnexpectedToken(Expectation::OperatorToken, tok)),
        }

        Ok(())
    }
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Expectation {
    OperandToken,
    OperatorToken,
}

impl fmt::Display for Expectation {
    fn fmt(&self, f: &mut fmt::Formatter) -> std::result::Result<(), fmt::Error> {
        use Expectation::*;
        write!(
            f,
            "{} token",
            match self {
                OperandToken => "operand",
                OperatorToken => "operator",
            }
        )
    }
}

fn digits_to_node(digits: &str, base: NumberBase) -> Result<Node> {
    let value: BigDecimal;
    match base {
        NumberBase::Decimal => match BigDecimal::from_str(digits) {
            Ok(n) => value = n,
            Err(_) => return Err(Error::ParseDecimal(digits.to_owned())),
        },
        _ => value = BigDecimal::from(u64::from_str_radix(digits, base as u32)?),
    }

    Ok(Node::from(value))
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("error scanning input: {0}")]
    Scanner(#[from] scanner::Error),

    #[error("unmatched parentheses")]
    UnmatchedParentheses,

    #[error("unexpected end of input: expected {0}")]
    UnexpectedEndOfInput(Expectation),

    #[error("failed to parse decimal number '{0}'")]
    ParseDecimal(String),

    #[error("failed to parse integer: {0}")]
    ParseInt(#[from] std::num::ParseIntError),

    #[error("unexpected token: expected {0:?}, found {1:?}")]
    UnexpectedToken(Expectation, Token),

    #[error("not enough operands for operation '{0}'")]
    MissingOperands(Operation),

    #[error("input does not form a single syntax tree. Did you forget some operators?")]
    MultipleRoots,
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::num::IntErrorKind;

    #[test]
    fn parse_numbers() {
        let cases = [
            ("deadbeef", NumberBase::Hexadecimal, bigdec!(3735928559)),
            ("1.2345", NumberBase::Decimal, bigdec!(1.2345)),
            ("00101010011010", NumberBase::Binary, bigdec!(2714)),
        ];
        for (digits, base, value) in cases {
            assert_eq!(digits_to_node(digits, base).unwrap(), Node::from(value));
        }
    }

    #[test]
    fn parse_mega_expression() {
        use crate::ast::Node::*;
        use crate::ast::Operation::*;

        let input = "a := (b := _) + 1 + 0b00000010 - 3.14159 ^ 2 ^ 3 * -0x123456789abcdef";
        let expected = Binary {
            operation: Assign,
            lhs: Box::new(Variable("a".to_string())),
            rhs: Box::new(Binary {
                operation: Subtract,
                lhs: Box::new(Binary {
                    operation: Add,
                    lhs: Box::new(Binary {
                        operation: Add,
                        lhs: Box::new(Binary {
                            operation: Assign,
                            lhs: Box::new(Variable("b".to_string())),
                            rhs: Box::new(Variable("_".to_string())),
                        }),
                        rhs: Box::new(Number(bigdec!(1))),
                    }),
                    rhs: Box::new(Number(bigdec!(2))),
                }),
                rhs: Box::new(Binary {
                    operation: Multiply,
                    lhs: Box::new(Binary {
                        operation: Exponent,
                        lhs: Box::new(Number(bigdec!(3.14159))),
                        rhs: Box::new(Binary {
                            operation: Exponent,
                            lhs: Box::new(Number(bigdec!(2))),
                            rhs: Box::new(Number(bigdec!(3))),
                        }),
                    }),
                    rhs: Box::new(Unary {
                        operation: UnarySubtract,
                        expr: Box::new(Number(bigdec!(81985529216486895))),
                    }),
                }),
            }),
        };
        assert_eq!(Parser::new(input).parse().unwrap(), expected);
    }
}
