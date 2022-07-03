use crate::ast::Operation;
use std::fmt;
use std::{iter::Peekable, str::Chars};
use thiserror::Error;

type Stream<'a> = Peekable<Chars<'a>>;

#[derive(Debug)]
pub struct Scanner<'a> {
    stream: Stream<'a>,
}

impl<'a> Scanner<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            stream: src.chars().peekable(),
        }
    }

    fn collect_digits(&mut self, base: NumberBase) -> String {
        let mut s = String::with_capacity(128); // assume most numbers are <128 chars
        loop {
            if let Some(&c) = self.stream.peek() {
                if c.is_digit(base as u32) || c == '.' {
                    s.push(c);
                    self.stream.next(); // consume digit
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        return s;
    }
}

impl Iterator for Scanner<'_> {
    type Item = Token;
    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        if let Some(c) = self.stream.next() {
            if c.is_whitespace() {
                // consume whitespace and parse the next token
                return self.next();
            }

            if let Ok(op) = Operation::try_from(c) {
                return Some(Token::Operation(op));
            }

            if c == '0' {
                if let Some(&next) = self.stream.peek() {
                    if next == 'x' || next == 'b' {
                        let base = match self.stream.next().unwrap() {
                            'b' => NumberBase::Binary,
                            'x' => NumberBase::Hexadecimal,
                            _ => NumberBase::Decimal, // won't be reached
                        };
                        return Some(Token::Number(base, self.collect_digits(base)));
                    } else if next.is_digit(NumberBase::Decimal as u32) || next == '.' {
                        // since leading zeroes don't matter, we can consume
                        // this one and continue scanning at the next digit.
                        return self.next();
                    } else {
                        return Some(Token::Number(NumberBase::Decimal, "0".to_string()));
                    }
                }
            }

            if c.is_digit(NumberBase::Decimal as u32) || c == '.' {
                let mut digits = self.collect_digits(NumberBase::Decimal);
                digits.insert(0, c);
                return Some(Token::Number(NumberBase::Decimal, digits));
            }

            Some(match c {
                '(' => Token::LParen,
                ')' => Token::RParen,
                _ => Token::Illegal(Error::Unknown),
            })
        } else {
            None
        }
    }
}

#[derive(Copy, Clone, Debug, Error, PartialEq)]
pub enum Error {
    #[error("invalid digit '{0}' for {1}")]
    InvalidDigitForBase(char, NumberBase),
    #[error("unknown error during scanning")]
    Unknown,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Illegal(Error),

    Operation(Operation),
    LParen,
    RParen,

    Number(NumberBase, String),
}

impl From<Operation> for Token {
    fn from(op: Operation) -> Self {
        Token::Operation(op)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scan() {
        use crate::ast::Operation::*;
        use NumberBase::*;
        use Token::*;

        let srcs = [
            "1 + 0b10 * 0x3 / 4.2",
            "1+0b10*0x3/4.2",
            "1    +    0b10  *   0x3/4.2    ",
        ];
        for src in srcs {
            let s = Scanner::new(src);
            let tokens: Vec<Token> = s.collect();

            let expected = vec![
                Number(Decimal, "1".to_string()),
                Operation(Add),
                Number(Binary, "10".to_string()),
                Operation(Multiply),
                Number(Hexadecimal, "3".to_string()),
                Operation(Divide),
                Number(Decimal, "4.2".to_string()),
            ];
            assert_eq!(tokens, expected);
        }

        let src = "3 * (2 + 1) / 0.5";
        let expected = vec![
            Number(Decimal, "3".to_string()),
            Operation(Multiply),
            LParen,
            Number(Decimal, "2".to_string()),
            Operation(Add),
            Number(Decimal, "1".to_string()),
            RParen,
            Operation(Divide),
            Number(Decimal, ".5".to_string()),
        ];
        let tokens: Vec<Token> = Scanner::new(src).collect();
        assert_eq!(tokens, expected);
    }
}
