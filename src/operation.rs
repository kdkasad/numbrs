/*

operation.rs - Operations for Numbrs
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

// Disable warning about unary operator patterns being unreachable
#![allow(unreachable_patterns)]

use strum_macros::{Display, EnumString};
use thiserror::Error;

#[derive(Copy, Clone, Debug, EnumString, Display, PartialEq, Eq)]
pub enum Operation {
    #[strum(serialize = "+")]
    Add,
    #[strum(serialize = "-")]
    Subtract,

    #[strum(serialize = "*")]
    Multiply,
    #[strum(serialize = "/")]
    Divide,

    #[strum(serialize = "^")]
    Raise,

    #[strum(serialize = "+")]
    UnaryAdd,
    #[strum(serialize = "-")]
    UnarySubtract,
}

impl Operation {
    pub fn unary_try_from(opstr: &str) -> Result<Self, OperationError> {
        use Operation::*;
        match opstr {
            "+" => Ok(UnaryAdd),
            "-" => Ok(UnarySubtract),
            _ => Err(OperationError::InvalidOperatorString(opstr.to_owned())),
        }
    }

    pub fn is_binary(self) -> bool {
        use Operation::*;
        match self {
            Add | Subtract | Multiply | Divide | Raise => true,
            UnaryAdd | UnarySubtract => false,
        }
    }

    pub fn is_unary(self) -> bool {
        !self.is_binary()
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum OperationError {
    #[error("Invalid operator string: '{0}'")]
    InvalidOperatorString(String),
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn operator_names() {
        assert_eq!(Operation::Add.to_string(), "+");
        assert_eq!(Operation::Subtract.to_string(), "-");
        assert_eq!(Operation::Multiply.to_string(), "*");
        assert_eq!(Operation::Divide.to_string(), "/");
        assert_eq!(Operation::Raise.to_string(), "^");
        assert_eq!(Operation::UnaryAdd.to_string(), "+");
        assert_eq!(Operation::UnarySubtract.to_string(), "-");
    }

    #[test]
    fn unary_from_string() {
        macro_rules! cases {
            ( $( $a:tt $b:ident ),+ $(,)? ) => {
                $( assert_eq!(Operation::unary_try_from(stringify!($a)).unwrap(), Operation::$b); )+
            };
        }
        cases! {
            + UnaryAdd,
            - UnarySubtract,
        };

        assert_eq!(
            Operation::unary_try_from("~=").unwrap_err(),
            OperationError::InvalidOperatorString(String::from("~="))
        )
    }
}
