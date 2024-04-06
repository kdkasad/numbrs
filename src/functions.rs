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

use num::{BigInt, BigRational, FromPrimitive, Integer, One, Signed, ToPrimitive, Zero};
use strum_macros::{Display, EnumString};
use thiserror::Error;

use crate::ast::Value;

/// # Mathematical functions
///
/// The functions that can be used in the calculator.
#[derive(EnumString, Display, Debug, PartialEq, Clone, Copy)]
pub enum Function {
    /// ## Sine function
    ///
    /// Expects 1 argument.
    /// Calculates the sine of that argument in radians.
    #[strum(serialize = "sin")]
    Sine,

    /// ## Cosine function
    ///
    /// Expects 1 argument.
    /// Calculates the cosine of that argument in radians.
    #[strum(serialize = "cos")]
    Cosine,

    /// ## Absolute value function
    ///
    /// Expects 1 argument.
    /// Returns the distance of the argument from zero.
    #[strum(serialize = "abs")]
    AbsoluteValue,

    /// ## Square root function
    ///
    /// Expects 1 argument.
    /// Returns the square root of the argument.
    #[strum(serialize = "sqrt")]
    SquareRoot,

    /// ## Natural logarithm function
    ///
    /// Expects 1 argument.
    /// Returns the natural logarithm of the argument.
    #[strum(serialize = "ln")]
    NaturalLogarithm,

    /// ## Greatest common denominator
    ///
    /// Expects 2 *integer* arguments.
    /// Returns the greatest common demoninator of the two arguments.
    #[strum(serialize = "gcd")]
    GCD,

    /// ## Least common multiple
    ///
    /// Expects 2 *integer* arguments.
    /// Returns the least common multiple of the two arguments.
    #[strum(serialize = "lcm")]
    LCM,

    /// ## Combinatorics choose function
    ///
    /// Expects 2 *integer* arguments.
    /// Returns the number of unique ways to choose R items from N items,
    /// where N is the first argument and R is the second.
    /// If N < R, returns 0.
    ///
    /// See [https://en.wikipedia.org/wiki/Combination].
    #[strum(serialize = "choose")]
    Choose,

    /// ## Combinatorics permutation function
    ///
    /// Expects 2 *integer* arguments.
    /// Returns the number of permutations of R items from N items,
    /// where N is the first argument and R is the second.
    /// If N < R, returns 0.
    ///
    /// See [https://en.wikipedia.org/wiki/Permutation].
    #[strum(serialize = "permute")]
    Permute,

    /// ## Factorial function
    ///
    /// Expects 1 *positive integer* argument.
    /// Returns the factorial of the argument, i.e. (1 × 2 × 3 × ... × N).
    /// If N < 1, returns 1.
    #[strum(serialize = "factorial")]
    Factorial,
}

impl Function {
    /// Returns the number of arguments a particular function accepts.
    fn number_of_args(&self) -> usize {
        use Function::*;
        match self {
            Sine | Cosine | AbsoluteValue | SquareRoot | NaturalLogarithm | Factorial => 1,
            GCD | LCM | Choose | Permute => 2,
        }
    }

    /// # Evaluate a function with a given set of arguments.
    ///
    /// ## Errors
    ///
    /// Returns a [`FunctionCallError`] if evaluation of a function fails.
    pub fn eval(&self, mut args: Vec<BigRational>) -> Result<Value, FunctionCallError> {
        use Function::*;
        if args.len() != self.number_of_args() {
            return Err(FunctionCallError::NumberOfArguments(
                *self,
                self.number_of_args(),
                args.len(),
            ));
        }
        macro_rules! arg {
            () => {
                args.remove(0)
            };
        }
        Ok(match self {
            Sine => sin(arg!())?,
            Cosine => cos(arg!())?,
            AbsoluteValue => abs(arg!()),
            SquareRoot => sqrt(arg!())?,
            NaturalLogarithm => ln(arg!())?,
            GCD => gcd(arg!(), arg!())?,
            LCM => lcm(arg!(), arg!())?,
            Choose => choose(arg!(), arg!())?,
            Permute => permute(arg!(), arg!())?,
            Factorial => factorial(arg!())?,
        }
        .into())
    }
}

/// # Function call error
///
/// Represents an error that has occurred during the calling or computation of a
/// function.
#[derive(Error, Debug)]
pub enum FunctionCallError {
    /// Function argument number mismatch.
    ///
    /// This error occurs when a function is called with the wrong number of arguments.
    #[error("Function `{0}` expected {1} arguments, but got {2}")]
    NumberOfArguments(Function, usize, usize),

    /// Non-integer argument.
    ///
    /// This error occurs when a function that expects an integer argument is
    /// called with a non-integer argument.
    #[error("Function `{0}` expected an integer, but got `{1}`")]
    NonIntegerArgument(Function, BigRational),

    /// # Overflow occurred during evaluation
    #[error("Number `{0}` too large. Overflow occurred during function computation.")]
    Overflow(BigRational),
}

/// Check to make sure `n` is an integer.
///
/// Returns:
///  - If `n` is an integer, returns `Ok( () )`.
///  - If `n` is not an integer, returns `Err( FunctionCallError::NonIntegerArgument )`.
///    The `func` argument is used as the function field in the error.
fn check_integer(func: Function, n: BigRational) -> Result<BigRational, FunctionCallError> {
    if n.is_integer() {
        Ok(n)
    } else {
        Err(FunctionCallError::NonIntegerArgument(func, n))
    }
}

fn sin(arg: BigRational) -> Result<BigRational, FunctionCallError> {
    match arg.to_f64() {
        Some(float) => {
            let sin = float.sin();
            match BigRational::from_f64(sin) {
                Some(rat) => Ok(rat),
                None => Err(FunctionCallError::Overflow(arg)),
            }
        }
        None => Err(FunctionCallError::Overflow(arg)),
    }
}

fn cos(arg: BigRational) -> Result<BigRational, FunctionCallError> {
    match arg.to_f64() {
        Some(float) => {
            let cos = float.cos();
            match BigRational::from_f64(cos) {
                Some(rat) => Ok(rat),
                None => Err(FunctionCallError::Overflow(arg)),
            }
        }
        None => Err(FunctionCallError::Overflow(arg)),
    }
}

fn abs(arg: BigRational) -> BigRational {
    arg.abs()
}

fn ln(arg: BigRational) -> Result<BigRational, FunctionCallError> {
    match arg.to_f64() {
        Some(float) => {
            let result = float.ln();
            match BigRational::from_f64(result) {
                Some(rat) => Ok(rat),
                None => Err(FunctionCallError::Overflow(arg)),
            }
        }
        None => Err(FunctionCallError::Overflow(arg)),
    }
}

fn sqrt(arg: BigRational) -> Result<BigRational, FunctionCallError> {
    match arg.to_f64() {
        Some(float) => {
            let result = float.sqrt();
            match BigRational::from_f64(result) {
                Some(rat) => Ok(rat),
                None => Err(FunctionCallError::Overflow(arg)),
            }
        }
        None => Err(FunctionCallError::Overflow(arg)),
    }
}

fn gcd(a: BigRational, b: BigRational) -> Result<BigRational, FunctionCallError> {
    let mut x = check_integer(Function::GCD, a)?.to_integer();
    let mut y = check_integer(Function::GCD, b)?.to_integer();

    // We could use the num::Integer::gcd() function, but I want to implement it myself.
    while !y.is_zero() {
        let r = x.mod_floor(&y);
        x = y;
        y = r;
    }
    Ok(x.into())
}

fn lcm(a: BigRational, b: BigRational) -> Result<BigRational, FunctionCallError> {
    Ok(&a * &b / gcd(a, b)?)
}

fn choose(n: BigRational, r: BigRational) -> Result<BigRational, FunctionCallError> {
    let n = check_integer(Function::Choose, n)?;
    let r = check_integer(Function::Choose, r)?;
    Ok(permute(n, r.to_owned())? / factorial(r)?)
}

fn permute(n: BigRational, r: BigRational) -> Result<BigRational, FunctionCallError> {
    let n = check_integer(Function::Permute, n)?;
    let r = check_integer(Function::Permute, r)?;
    if n < r {
        return Ok(BigRational::zero());
    }
    let n_int = n.to_integer();
    let mut denom: BigInt = &n_int - r.to_integer() + 1;
    let mut result = BigInt::one();
    while denom <= n_int {
        result *= &denom;
        denom += 1;
    }
    Ok(result.into())
}

fn factorial(n: BigRational) -> Result<BigRational, FunctionCallError> {
    let n_int = check_integer(Function::Factorial, n)?.to_integer();
    let mut result = BigInt::one();
    let mut term = BigInt::one() + 1;
    while term <= n_int {
        result *= &term;
        term += 1;
    }
    Ok(result.into())
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::rat_util_macros::rat;

    #[test]
    fn gcd() {
        macro_rules! case {
            ($a:literal, $b:literal = $expected:literal) => {
                assert_eq!(super::gcd(rat!($a), rat!($b)).unwrap(), rat!($expected));
            };
        }

        case!(0, 0 = 0);
        case!(1, 15 = 1);
        case!(34985, 1 = 1);
        case!(12, 15 = 3);
        case!(15, 12 = 3);
        case!(-5, 25 = 5);
    }
}
