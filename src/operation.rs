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

//! Mathematical operations

use thiserror::Error;

// We put Operation in its own module because the strum macros cause warnings
// which cannot be disabled for just the Operation type since they occur within
// the procedural macros. We have to allow the unreachable_patterns diagnostic
// for the containing module, so we isolate it into its own module.
mod inner {
    // Disable warning about unary operator patterns being unreachable
    #![allow(unreachable_patterns)]

    use strum_macros::{Display, EnumString};

    /// # Mathematical operation
    ///
    /// Represents an operation that is performed between two numbers or values.
    #[derive(Copy, Clone, Debug, EnumString, Display, PartialEq, Eq)]
    pub enum Operation {
        /// ## Arithmetic addition
        #[strum(serialize = "+")]
        Add,

        /// ## Artithmetic subtraction
        #[strum(serialize = "-")]
        Subtract,

        /// ## Artithmetic multiplication
        #[strum(serialize = "*")]
        Multiply,

        /// ## Artithmetic division
        #[strum(serialize = "/")]
        Divide,

        /// ## Artithmetic exponentiation
        ///
        /// E.g. *A* to the power of *B*
        #[strum(serialize = "^")]
        Raise,

        /// ## Variable assignment
        ///
        /// Assigns the RHS value to the identifier provided as the LHS
        #[strum(serialize = "=")]
        Assign,

        /// ## Unit assignment
        ///
        /// Same as variable assignment, but makes the identifier a unit rather
        /// than a plain number or quantity.
        #[strum(serialize = ":=")]
        AssignUnit,

        /// ## Unary addition
        ///
        /// Same as [`Add`][`Self::Add`] but for one operand rather than two.
        /// This really does nothing, as it just adds zero to the operand.
        #[strum(serialize = "+")]
        UnaryAdd,

        /// ## Unary subtraction
        ///
        /// Same as [`Subtract`][`Self::Subtract`] but for one operand rather
        /// than two. This subtracts the operand from zero, essentially negating
        /// it.
        #[strum(serialize = "-")]
        UnarySubtract,

        /// ## Convert units
        ///
        /// Convert the quantity or unit(s) on the LHS to the same value
        /// represented in the unit(s) specified on the RHS.
        #[strum(serialize = "to")]
        ConvertUnits,
    }
}
pub use inner::Operation;

impl Operation {
    /// Attempt to create a unary [`Operation`] variant from an operator string.
    ///
    /// Returns an [`Operation`] on success.
    ///
    /// ## Errors
    ///
    /// Returns an [`OperationError::InvalidOperatorString`] if the given
    /// operator string does not map to a unary operation.
    pub fn unary_try_from(opstr: &str) -> Result<Self, OperationError> {
        use Operation::*;
        match opstr {
            "+" => Ok(UnaryAdd),
            "-" => Ok(UnarySubtract),
            _ => Err(OperationError::InvalidOperatorString(opstr.to_owned())),
        }
    }

    /// Test whether this [`Operation`] is a binary operation, i.e. one which
    /// accepts two operands.
    pub fn is_binary(self) -> bool {
        use Operation::*;
        match self {
            Add | Subtract | Multiply | Divide | Raise | Assign | AssignUnit | ConvertUnits => true,
            UnaryAdd | UnarySubtract => false,
        }
    }

    /// Test whether this [`Operation`] is a unary operation, i.e. one which
    /// accepts one operand.
    pub fn is_unary(self) -> bool {
        !self.is_binary()
    }

    /// Try to convert a binary operation to the equivalent unary operation.
    ///
    /// # Example
    ///
    /// ```
    /// use numbrs::operation::Operation;
    ///
    /// let binop = Operation::Add;
    /// let unop = binop.try_to_unary();
    /// assert!(matches!(unop, Some(Operation::UnaryAdd)));
    ///
    /// let binop = Operation::Multiply;
    /// let unop = binop.try_to_unary();
    /// assert!(matches!(unop, None));
    /// ```
    pub fn try_to_unary(self) -> Option<Self> {
        use Operation::*;
        match self {
            UnaryAdd | UnarySubtract => Some(self),
            Add => Some(UnaryAdd),
            Subtract => Some(UnarySubtract),
            Multiply | Divide | Raise | Assign | AssignUnit | ConvertUnits => None,
        }
    }
}

/// # Operation error
///
/// Represents an error relating to usage of the [`Operation`] type.
#[derive(Debug, Error, PartialEq, Eq)]
pub enum OperationError {
    /// # Invalid operator string
    ///
    /// Occurs when attempting to create an [`Operation`] variant with a string
    /// that does not represent a valid operator.
    #[error("Invalid operator string: '{0}'")]
    InvalidOperatorString(String),
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn operator_names() {
        assert_eq!(Operation::Add.to_string(), "+");
        assert_eq!(Operation::Subtract.to_string(), "-");
        assert_eq!(Operation::Multiply.to_string(), "*");
        assert_eq!(Operation::Divide.to_string(), "/");
        assert_eq!(Operation::Raise.to_string(), "^");
        assert_eq!(Operation::UnaryAdd.to_string(), "+");
        assert_eq!(Operation::UnarySubtract.to_string(), "-");
        assert_eq!(Operation::Assign.to_string(), "=");
        assert_eq!(Operation::AssignUnit.to_string(), ":=");
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
            Operation::unary_try_from("~=")
                .expect_err("Creating Operation from invalid operator succeeded"),
            OperationError::InvalidOperatorString(String::from("~="))
        )
    }
}
