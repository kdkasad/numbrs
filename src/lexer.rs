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

Lexer for Numbrs.
Transforms input strings into token streams.

# Examples

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

use std::{iter::Peekable, str::FromStr, vec::IntoIter};

use strum_macros::{Display, IntoStaticStr};

use crate::operation::Operation;

#[derive(Display, Debug, IntoStaticStr, PartialEq, Eq)]
#[strum(serialize_all = "lowercase")]
pub enum Token {
    Operator(Operation),
    Number(String),

    #[strum(serialize = "identifier")]
    Ident(String),

    Illegal(char),
}

/// Input lexer. Processes input strings into token streams.
/// 
/// See [module-level][crate::lexer] documentation for examples.
pub struct Lexer {
    chars: Peekable<IntoIter<char>>,
}

impl Lexer {
    /// Create a new Lexer using a string reference as the source
    pub fn new(src: &str) -> Self {
        Self {
            chars: src.chars().collect::<Vec<_>>().into_iter().peekable(),
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.chars.peek() {
            Some(&c) => {
                if c.is_whitespace() {
                    // consume whitespace and continue
                    collect_chars(&mut self.chars, |c| c.is_whitespace());
                    self.next()
                } else if c.is_numeric() {
                    // c is a digit
                    // collect contiguous digits and return number token
                    Some(Token::Number(collect_chars(&mut self.chars, |c| {
                        c.is_numeric()
                    })))
                } else if let Ok(op) = Operation::from_str(&c.to_string()) {
                    // c is an operator character
                    // consume the character and return and operator token
                    self.chars.next().unwrap();
                    Some(Token::Operator(op))
                } else if c.is_alphabetic() || c == '_' {
                    // c is alphabetic or underscore
                    // collect contiguous ident chars and return ident token
                    Some(Token::Ident(collect_chars(&mut self.chars, |c| {
                        c.is_alphabetic() || c == '_'
                    })))
                } else {
                    // unrecognized character
                    // consume the character and return an illegal token
                    self.chars.next().unwrap();
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
/// use numbrs::lexer::collect_chars;
///
/// let mut chars = "123 + 456".chars().collect::<Vec<_>>().into_iter().peekable();
/// assert_eq!(collect_chars(&mut chars, |c| c.is_numeric()), "123".to_string());
/// assert_eq!(collect_chars(&mut chars, |c| !c.is_numeric()), " + ".to_string());
/// assert_eq!(collect_chars(&mut chars, |c| c.is_numeric()), "456".to_string());
/// ```
fn collect_chars<F>(chars: &mut Peekable<IntoIter<char>>, predicate: F) -> String
where
    F: Fn(char) -> bool,
{
    let mut res = String::new();
    while let Some(&c) = chars.peek() {
        if predicate(c) {
            // use chars.next() to advance the iterator
            res.push(chars.next().unwrap());
        } else {
            break;
        }
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
            ( o $b:tt ) => {
                Token::Operator(Operation::from_str(stringify!($b)).unwrap())
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
            ( $( $a:tt $b:tt ),+ $(,)? ) => {
                vec![ $( toks!($a $b), )+ ]
            }
        }
        let cases: Vec<(&'static str, Vec<Token>)> = vec![
            ("1 2 3", toks!(n 1, n 2, n 3)),
            ("1 + 2 + 3", toks!(n 1, o +, n 2, o +, n 3)),
            ("", vec![]),
            ("a", toks!(i a,)),
            ("magic", toks!(i magic,)),
            ("123magic", toks!(n 123, i magic)),
            ("snake_case", toks!(i snake_case,)),
            ("123 + ?", toks!(n 123, o +, il ?)),
            (".", toks!(il . ,)),
        ];
        for (src, toks) in cases {
            let result: Vec<Token> = Lexer::new(src).collect();
            assert_eq!(result, toks);
        }
    }
}
