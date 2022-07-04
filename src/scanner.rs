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
    fn collect_digits(&mut self, base: NumberBase) -> String {
        let mut s = String::with_capacity(128); // assume most numbers are <128 chars
        while let Some(&c) = self.stream.peek() {
            if c.is_digit(base as u32) || (base == NumberBase::Decimal && c == '.') {
                s.push(c);
                self.stream.next(); // consume digit
            } else {
                break;
            }
        }
        return s;
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

            // begin decimal number
            if c.is_digit(NumberBase::Decimal as u32) || c == '.' {
                let mut digits = self.collect_digits(NumberBase::Decimal);
                digits.insert(0, c);
                return Some(Token::Number(NumberBase::Decimal, digits));
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Operation::*;
    use NumberBase::*;
    use Token::*;

    #[test]
    fn scan() {
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
}
