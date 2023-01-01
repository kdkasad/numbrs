/*

lexer.rs - Numbrs input lexer
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

/*!

# Expression lexer

Transforms expression strings into [`Token`] streams.

## Examples

```
use numbrs::{
    lexer::{Lexer, Token::*},
    operation::Operation::*,
};

let mut lexer = Lexer::new("1 + 2 * 3");
assert_eq!(lexer.next(), Some(Number(String::from("1"))));
assert_eq!(lexer.next(), Some(Operator(Add)));
assert_eq!(lexer.next(), Some(Number(String::from("2"))));
assert_eq!(lexer.next(), Some(Operator(Multiply)));
assert_eq!(lexer.next(), Some(Number(String::from("3"))));
assert_eq!(lexer.next(), None);
```

*/

use std::{
    iter::Peekable,
    str::{Chars, FromStr},
};

use strum_macros::{Display, IntoStaticStr};

use crate::operation::Operation;

/// # Expression syntax token
///
/// Represents a single piece of an expression in text syntax.
#[derive(Clone, Display, Debug, IntoStaticStr, PartialEq, Eq)]
#[strum(serialize_all = "lowercase")]
pub enum Token {
    /// ## Operator token
    ///
    /// Represents an operator, like `+` or `*`.
    Operator(Operation),

    /// ## Number token
    ///
    /// Represents a number literal, like `123` or `8.2`.
    Number(String),

    /// ## Identifier token
    ///
    /// Represents an identifier, i.e. a variable/unit name, like `meter`.
    #[strum(serialize = "identifier")]
    Ident(String),

    /// ## Begin group token
    ///
    /// Begins a group. This is usually the left paren, `(`.
    #[strum(serialize = "group open")]
    GroupBegin,

    /// ## End group token
    ///
    /// Ends a group. This is usually the right paren, `)`.
    #[strum(serialize = "group close")]
    GroupEnd,

    /// ## Illegal token
    ///
    /// Represents an unexpected token which is not supposed to exist where it
    /// does.
    Illegal(char),
}

/// # Expression lexer
///
/// Processes input strings into [`Token`] streams.
///
/// This struct implements [`Iterator<Item = Token>`][2], so it can be used like
/// any other iterator over [`Token`]s.
///
/// See [module-level documentation][1] for an example usage.
///
/// [1]: crate::lexer
/// [2]: Iterator
#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    /// Create a new [`Lexer`] for an expression string.
    pub fn new(src: &'a str) -> Self {
        Self {
            chars: src.chars().peekable(),
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.chars.peek() {
            Some(&c) => {
                if c.is_whitespace() {
                    // consume whitespace and continue
                    collect_chars(&mut self.chars, |c| c.is_whitespace());
                    self.next()
                } else if c.is_numeric() || c == '.' {
                    // c is a digit
                    // collect contiguous digits and return number token
                    Some(Token::Number(collect_chars(&mut self.chars, |&c| {
                        c.is_numeric() || c == '.'
                    })))
                } else if "({[".contains(c) {
                    self.chars.next();
                    Some(Token::GroupBegin)
                } else if ")}]".contains(c) {
                    self.chars.next();
                    Some(Token::GroupEnd)
                } else if let Ok(op) = Operation::from_str(&c.to_string()) {
                    // c is an operator character
                    // consume the character and return and operator token
                    self.chars.next();
                    Some(Token::Operator(op))
                } else if c.is_alphabetic() || c == '_' {
                    // c is alphabetic or underscore
                    // collect contiguous ident chars
                    let ident = collect_chars(&mut self.chars, |&c| c.is_alphabetic() || c == '_');
                    Some(if let Ok(op) = Operation::from_str(&ident) {
                        // check if identifier is a word operator
                        Token::Operator(op)
                    } else {
                        // else, return identifier token
                        Token::Ident(ident)
                    })
                } else if c == ':' {
                    self.chars.next(); // consume ':'
                    if let Some('=') = self.chars.peek() {
                        self.chars.next(); // consume '='
                        Some(Token::Operator(Operation::AssignUnit))
                    } else {
                        Some(Token::Illegal(c))
                    }
                } else {
                    // unrecognized character
                    // consume the character and return an illegal token
                    self.chars.next();
                    Some(Token::Illegal(c))
                }
            }

            // End of token stream
            None => None,
        }
    }
}

/// Collect a series of contiguous characters matching a predicate into a string
///
/// # Example
///
/// ```ignore
/// # use numbrs::lexer::collect_chars;
/// let mut chars = "123 + 456".chars().collect::<Vec<_>>().into_iter().peekable();
/// assert_eq!(&collect_chars(&mut chars, |c| c.is_numeric()),  "123");
/// assert_eq!(&collect_chars(&mut chars, |c| !c.is_numeric()), " + ");
/// assert_eq!(&collect_chars(&mut chars, |c| c.is_numeric()),  "456");
/// ```
fn collect_chars<F>(chars: &mut Peekable<Chars>, predicate: F) -> String
where
    F: Fn(&char) -> bool,
{
    let mut res = String::new();
    while let Some(c) = chars.next_if(&predicate) {
        res.push(c);
    }
    res
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    /// Tests the [Lexer]'s ability to tokenize input strings
    #[test]
    fn tokenize() {
        macro_rules! toks {
            ( o $b:literal ) => {
                Token::Operator(Operation::from_str($b).unwrap())
            };
            ( n $b:literal ) => {
                Token::Number(stringify!($b).to_string())
            };
            ( i $b:ident ) => {
                Token::Ident(stringify!($b).to_string())
            };
            ( il $b:tt ) => {
                Token::Illegal(stringify!($b).chars().next().unwrap())
            };
            ( lp $b:tt ) => { Token::GroupBegin };
            ( rp $b:tt ) => { Token::GroupEnd };
            ( $( $a:tt $b:tt ),+ $(,)? ) => {
                vec![ $( toks!($a $b), )+ ]
            }
        }
        let cases: Vec<(&'static str, Vec<Token>)> = vec![
            ("1 2 3", toks!(n 1, n 2, n 3)),
            ("1 + 2 + 3", toks!(n 1, o "+", n 2, o "+", n 3)),
            ("", vec![]),
            ("a", toks!(i a,)),
            ("magic", toks!(i magic,)),
            ("123magic", toks!(n 123, i magic)),
            ("snake_case", toks!(i snake_case,)),
            ("123 + ?", toks!(n 123, o "+", il ?)),
            (".", vec![Token::Number(".".to_string())]),
            (
                "(1 + 2) * 3",
                toks!(lp ., n 1, o "+", n 2, rp ., o "*", n 3),
            ),
            ("foo = bar", toks!(i foo, o "=", i bar)),
            (
                "foo = 1 + 2^3",
                toks!(i foo, o "=", n 1, o "+", n 2, o "^", n 3),
            ),
            ("1.23", toks!(n 1.23,)),
            ("1.35 m", toks!(n 1.35, i m)),
        ];
        for (src, toks) in cases {
            let result: Vec<Token> = Lexer::new(src).collect();
            assert_eq!(result, toks);
        }
    }
}
