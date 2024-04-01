/*

function.rs - Math functions for Numbrs
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

//! Mathematical functions

use num::{BigRational, FromPrimitive, Signed, ToPrimitive};
use strum_macros::{Display, EnumString};

use crate::{ast::Value, eval::EvalError};

/// # Mathematical functions
///
/// The functions that can be used in the calculator.
#[derive(EnumString, Display, Debug, PartialEq, Clone, Copy)]
pub enum Function {
    /// # Sine function
    ///
    /// Expects 1 argument.
    #[strum(serialize = "sin")]
    Sine,

    /// # Cosine function
    ///
    /// Expects 1 argument.
    #[strum(serialize = "cos")]
    Cosine,

    /// # Absolute value function
    ///
    /// Expects 1 argument.
    /// Returns the distance of the argument from zero.
    #[strum(serialize = "abs")]
    AbsoluteValue,

    /// # Square root function
    ///
    /// Expects 1 argument.
    #[strum(serialize = "sqrt")]
    SquareRoot,

    /// # Natural logarithm function
    ///
    /// Expects 1 argument.
    #[strum(serialize = "ln")]
    NaturalLogarithm,
}

impl Function {
    /// Returns the number of arguments a particular function accepts.
    fn number_of_args(&self) -> usize {
        use Function::*;
        match self {
            Sine | Cosine | AbsoluteValue | SquareRoot | NaturalLogarithm => 1,
        }
    }

    /// # Evaluate a function with a given set of arguments.
    ///
    /// ## Errors
    ///
    /// Returns [`EvalError::InvalidFunctionArguments`] if the number of
    /// arguments provided doesn't match the number expected for the function
    /// being called.
    pub fn eval(&self, mut args: Vec<BigRational>) -> Result<Value, EvalError> {
        use Function::*;
        if args.len() != self.number_of_args() {
            return Err(EvalError::NumberOfFunctionArguments(
                *self,
                self.number_of_args(),
                args.len(),
            ));
        }
        let arg = args.swap_remove(0);
        Ok(match self {
            Sine => sin(arg)?,
            Cosine => cos(arg)?,
            AbsoluteValue => abs(arg),
            SquareRoot => sqrt(arg)?,
            NaturalLogarithm => ln(arg)?,
        }
        .into())
    }
}

fn sin(arg: BigRational) -> Result<BigRational, EvalError> {
    match arg.to_f64() {
        Some(float) => {
            let sin = float.sin();
            match BigRational::from_f64(sin) {
                Some(rat) => Ok(rat),
                None => Err(EvalError::Overflow(arg)),
            }
        }
        None => Err(EvalError::Overflow(arg)),
    }
}

fn cos(arg: BigRational) -> Result<BigRational, EvalError> {
    match arg.to_f64() {
        Some(float) => {
            let cos = float.cos();
            match BigRational::from_f64(cos) {
                Some(rat) => Ok(rat),
                None => Err(EvalError::Overflow(arg)),
            }
        }
        None => Err(EvalError::Overflow(arg)),
    }
}

fn abs(arg: BigRational) -> BigRational {
    arg.abs()
}

fn ln(arg: BigRational) -> Result<BigRational, EvalError> {
    match arg.to_f64() {
        Some(float) => {
            let result = float.ln();
            match BigRational::from_f64(result) {
                Some(rat) => Ok(rat),
                None => Err(EvalError::Overflow(arg)),
            }
        }
        None => Err(EvalError::Overflow(arg)),
    }
}

fn sqrt(arg: BigRational) -> Result<BigRational, EvalError> {
    match arg.to_f64() {
        Some(float) => {
            let result = float.sqrt();
            match BigRational::from_f64(result) {
                Some(rat) => Ok(rat),
                None => Err(EvalError::Overflow(arg)),
            }
        }
        None => Err(EvalError::Overflow(arg)),
    }
}
