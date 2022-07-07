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

use crate::ast::Operation;
use crate::token::{NumberBase, Token};
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

    #[allow(clippy::needless_return)]
    fn collect_nondecimal_digits(&mut self, base: NumberBase) -> Result<String, Error> {
        let mut s = String::with_capacity(128); // assume most numbers are <128 chars
        while let Some(&c) = self.stream.peek() {
            // Don't collect leading zeroes
            if c == '0' && s.is_empty() {
                self.stream.next(); // consume zero
                match self.stream.peek() {
                    Some(&c) if c.is_ascii_alphanumeric() => continue,
                    Some(_) | None => {
                        // If the zero is the last digit in the literal, keep it
                        return Ok("0".to_string());
                    }
                }
            }

            if c.is_ascii_alphanumeric() {
                if !c.is_digit(base as u32) {
                    return Err(Error::InvalidDigit(base, c));
                }
                s.push(c);
                self.stream.next(); // consume digit
            } else {
                break;
            }
        }

        if s.is_empty() {
            Err(Error::EmptyNumberLiteral)
        } else {
            Ok(s)
        }
    }

    #[allow(clippy::needless_return)]
    fn collect_decimal_digits(&mut self, start: char) -> Result<String, Error> {
        let mut s = String::with_capacity(128); // assume most numbers are <128 chars
        s.push(start);

        let mut frac = start == '.';

        // True if literal is in scientific notation. Gets set once the 'e' is encountered,
        // meaning we are scanning the exponent.
        let mut scientific = false;

        while let Some(&c) = self.stream.peek() {
            // Ignore leading zeroes in integer component
            if c == '0' && s.is_empty() && !frac {
                self.stream.next(); // consume zero
                match self.stream.peek() {
                    Some(&c) if !frac && c == '.' => {
                        self.stream.next(); // consume '.'
                        let post = self.collect_decimal_digits(c)?;
                        if post.is_empty() {
                            // The literal is a zero ending in a decimal point
                            return Ok("0".to_owned());
                        } else {
                            return Ok(post);
                        }
                    }
                    Some(&c) if c.is_ascii_alphanumeric() => continue,
                    Some(_) | None => return Ok("0".to_owned()), // is literal '0'
                }
            }

            if c.is_ascii_digit() || c == '.' {
                if c == '.' {
                    if scientific {
                        return Err(Error::FractionalExponent);
                    }
                    if frac {
                        return Err(Error::MultipleDecimalPoints);
                    } else {
                        frac = true;
                    }
                }
                s.push(c);
                self.stream.next(); // consume digit
            } else if c == 'e' {
                self.stream.next(); // consume digit
                if scientific {
                    return Err(Error::InvalidDigit(NumberBase::Decimal, c));
                }
                s.push(c);
                scientific = true;
            } else {
                break;
            }
        }

        if s.is_empty() || s == "." {
            Err(Error::EmptyNumberLiteral)
        } else {
            Ok(s)
        }
    }

    #[allow(clippy::needless_return)]
    fn collect_ident_chars(&mut self, first_char: char) -> String {
        let mut s = String::with_capacity(128); // assume most names are <128 chars
        s.push(first_char);
        while let Some(&c) = self.stream.peek() {
            if c.is_valid_ident() {
                s.push(c);
                self.stream.next(); // consume char
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

            // operator
            if let Ok(op) = Operation::try_from(c) {
                return Some(Token::Operator(op));
            }

            // begin number
            if c == '0' {
                if let Some(&next) = self.stream.peek() {
                    if next == 'x' || next == 'b' {
                        let base = match self.stream.next().unwrap() {
                            'b' => NumberBase::Binary,
                            'x' => NumberBase::Hexadecimal,
                            _ => unreachable!(),
                        };
                        return Some(match self.collect_nondecimal_digits(base) {
                            Ok(digits) => Token::Number(base, digits),
                            Err(err) => Token::Illegal(err),
                        });
                    } else if next.is_digit(NumberBase::Decimal as u32) || next == '.' {
                        // since leading zeroes don't matter, we can consume
                        // this one and continue scanning at the next digit.
                        return self.next();
                    } else {
                        return Some(Token::Number(NumberBase::Decimal, "0".to_string()));
                    }
                }
            }

            // begin decimal number
            if c.is_digit(NumberBase::Decimal as u32) || c == '.' {
                return Some(match self.collect_decimal_digits(c) {
                    Ok(digits) => Token::Number(NumberBase::Decimal, digits),
                    Err(err) => Token::Illegal(err),
                });
            }

            // assignment operator
            if c == ':' {
                if let Some(&next) = self.stream.peek() {
                    return Some(if next == '=' {
                        self.stream.next();
                        Token::Operator(Operation::Assign)
                    } else {
                        Token::Illegal(Error::IllegalInput(c))
                    });
                }
            }

            // parse as ident token
            if c.is_valid_ident_start() {
                let name = self.collect_ident_chars(c);
                return Some(Token::Ident(name));
            }

            Some(match c {
                '(' => Token::LParen,
                ')' => Token::RParen,
                _ => Token::Illegal(Error::IllegalInput(c)),
            })
        } else {
            None
        }
    }
}

trait CharIdentExt {
    fn is_valid_ident(&self) -> bool;
    fn is_valid_ident_start(&self) -> bool;
}

impl CharIdentExt for char {
    fn is_valid_ident(&self) -> bool {
        self.is_ascii_alphanumeric() || *self == '_'
    }

    fn is_valid_ident_start(&self) -> bool {
        self.is_ascii_alphabetic() || *self == '_'
    }
}

#[derive(Copy, Clone, Debug, Error, PartialEq)]
pub enum Error {
    #[error("invalid character '{0}' in input")]
    IllegalInput(char),

    #[error("invalid digit for {0} literal: '{1}'")]
    InvalidDigit(NumberBase, char),

    #[error("multiple decimal points in literal")]
    MultipleDecimalPoints,

    #[error("Fractional exponent found. Only integer exponents are supported.")]
    FractionalExponent,

    #[error("empty number literal (no digits)")]
    EmptyNumberLiteral,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Operation::*;
    use pretty_assertions::assert_eq;
    use NumberBase::*;
    use Token::*;

    #[test]
    fn scan_varying_whitespace() {
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
                Operator(Add),
                Number(Binary, "10".to_string()),
                Operator(Multiply),
                Number(Hexadecimal, "3".to_string()),
                Operator(Divide),
                Number(Decimal, "4.2".to_string()),
            ];
            assert_eq!(tokens, expected);
        }
    }

    #[test]
    fn scan_group() {
        let src = "3 * (2 + 1) / 0.5";
        let expected = vec![
            Number(Decimal, "3".to_string()),
            Operator(Multiply),
            LParen,
            Number(Decimal, "2".to_string()),
            Operator(Add),
            Number(Decimal, "1".to_string()),
            RParen,
            Operator(Divide),
            Number(Decimal, ".5".to_string()),
        ];
        let tokens: Vec<Token> = Scanner::new(src).collect();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn scan_assign() {
        let src = "name := 3 * (2 + 1) / 0.5";
        let expected = vec![
            Ident("name".to_string()),
            Operator(Assign),
            Number(Decimal, "3".to_string()),
            Operator(Multiply),
            LParen,
            Number(Decimal, "2".to_string()),
            Operator(Add),
            Number(Decimal, "1".to_string()),
            RParen,
            Operator(Divide),
            Number(Decimal, ".5".to_string()),
        ];
        let tokens: Vec<Token> = Scanner::new(src).collect();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn scan_nested_assign() {
        let src = "a := b := c";
        let expected = vec![
            Ident("a".to_string()),
            Operator(Assign),
            Ident("b".to_string()),
            Operator(Assign),
            Ident("c".to_string()),
        ];
        let tokens: Vec<Token> = Scanner::new(src).collect();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn scan_operators() {
        let src = "+ - * / ^ :=";
        let expected = [
            Operator(Add),
            Operator(Subtract),
            Operator(Multiply),
            Operator(Divide),
            Operator(Exponent),
            Operator(Assign),
        ];
        let output: Vec<Token> = Scanner::new(src).collect();
        assert_eq!(output, expected);
    }

    #[test]
    fn scan_idents() {
        let src = "this is a _test";
        let expected = [
            Ident("this".to_string()),
            Ident("is".to_string()),
            Ident("a".to_string()),
            Ident("_test".to_string()),
        ];
        let output: Vec<Token> = Scanner::new(src).collect();
        assert_eq!(output, expected);

        let src = "th|s ha$ inv@lid characters";
        let expected = [
            Ident("th".to_string()),
            Illegal(Error::IllegalInput('|')),
            Ident("s".to_string()),
            Ident("ha".to_string()),
            Illegal(Error::IllegalInput('$')),
            Ident("inv".to_string()),
            Illegal(Error::IllegalInput('@')),
            Ident("lid".to_string()),
            Ident("characters".to_string()),
        ];
        let output: Vec<Token> = Scanner::new(src).collect();
        assert_eq!(output, expected);
    }

    #[test]
    fn invalid_number_literals() {
        let cases = [
            (
                "0xfail",
                vec![
                    Number(Hexadecimal, "fa".to_string()),
                    Illegal(Error::InvalidDigit(Hexadecimal, 'i')),
                    Ident("l".to_string()),
                ],
            ),
            (
                "0b321",
                vec![
                    Illegal(Error::InvalidDigit(Binary, '3')),
                    Number(Decimal, "21".to_string()),
                ],
            ),
            (
                "0xtest",
                vec![
                    Illegal(Error::InvalidDigit(Hexadecimal, 't')),
                    Ident("est".to_string()),
                ],
            ),
            (
                "0.1.2",
                vec![
                    Illegal(Error::MultipleDecimalPoints),
                    Number(Decimal, ".2".to_string()),
                ],
            ),
            (
                "4.3e2.7",
                vec![
                    Illegal(Error::FractionalExponent),
                    Number(Decimal, ".7".to_string()),
                ],
            ),
            (
                "0x + 1",
                vec![
                    Illegal(Error::EmptyNumberLiteral),
                    Operator(Add),
                    Number(Decimal, "1".to_string()),
                ],
            ),
            (
                "0.a",
                vec![
                    Number(Decimal, "0".to_string()),
                    Illegal(Error::InvalidDigit(Decimal, 'a')),
                ],
            ),
            (".", vec![Illegal(Error::EmptyNumberLiteral)]),
        ];
        for (input, expected) in cases {
            let tokens: Vec<Token> = Scanner::new(input).collect();
            assert_eq!(tokens, expected);
        }
    }

    #[test]
    fn valid_number_literals() {
        let cases = [
            ("123", Decimal, "123"),
            ("0123", Decimal, "123"),
            ("0000000123", Decimal, "123"),
            ("0000000.123", Decimal, ".123"),
            ("00000001.23", Decimal, "1.23"),
            ("0.1", Decimal, ".1"),
            ("0", Decimal, "0"),
            ("0.0", Decimal, ".0"),
            ("0.", Decimal, "0"),
            ("0x0", Hexadecimal, "0"),
            ("0xabc", Hexadecimal, "abc"),
            ("0x000abc", Hexadecimal, "abc"),
            ("0xDeA94dfC", Hexadecimal, "DeA94dfC"),
            ("0b0", Binary, "0"),
            ("0b101010110", Binary, "101010110"),
            ("0b000001", Binary, "1"),
            ("4e5", Decimal, "4e5"),
            ("1.23e2", Decimal, "1.23e2"),
        ];
        for (input, e_base, e_output) in cases {
            let tokens: Vec<Token> = Scanner::new(input).collect();
            dbg!(input);
            assert_eq!(tokens.len(), 1);
            assert_eq!(tokens[0], Number(e_base, e_output.to_string()));
        }
    }
}
