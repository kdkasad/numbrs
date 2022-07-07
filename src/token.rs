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
                Hexadecimal => "hexadecimal",
            },
            *self as u32
        )
    }
}
