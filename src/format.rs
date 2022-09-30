/*

format.rs - Formatting for printing values in Numbrs
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

//! Formatting functions for [Value] struct and sub-types

use num::{BigRational, ToPrimitive};

use crate::ast::{Quantity, Value};

pub trait Formatter {
    fn format(&self, precision: usize) -> String;
}

impl Formatter for BigRational {
    fn format(&self, precision: usize) -> String {
        if self.is_integer() {
            self.numer().to_string()
        } else if let Some(float) = self.to_f64() {
            format!("{:.*}", precision, float)
        } else {
            // TODO: find a better way to format large rationals
            format!("{}/{}", self.numer(), self.denom())
        }
    }
}

impl Formatter for Quantity {
    fn format(&self, precision: usize) -> String {
        format!("{} {}", self.mag.format(precision), self.units)
    }
}

impl Formatter for Value {
    fn format(&self, precision: usize) -> String {
        match self {
            Value::Number(rat) => rat.format(precision),
            Value::Quantity(q) => q.format(precision),
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;
    use crate::rat_util_macros::rat;

    #[test]
    fn to_dec_string() {
        const PRECISION: usize = 3;
        let cases: Vec<(BigRational, &'static str)> = vec![(rat!(7, 2), "3.500")];
        for (rat, expected_result) in cases {
            assert_eq!(rat.format(PRECISION), expected_result);
        }
    }
}
