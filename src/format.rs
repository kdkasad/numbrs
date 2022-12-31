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
    fn format(&self, precision: isize) -> String;
}

impl Formatter for f64 {
    fn format(&self, precision: isize) -> String {
        if precision >= 0 {
            format!("{:.*}", precision as usize, self)
        } else {
            // Flip precision
            let precision: i32 = precision.unsigned_abs() as i32;
            let mut num = *self;
            num /= 10_f64.powi(precision);
            num = num.round();
            num *= 10_f64.powi(precision);
            if num == 0.0 {
                // Fixes negative zero (-0) case
                num = 0.0;
            }
            format!("{:.0}", num)
        }
    }
}

impl Formatter for BigRational {
    // TODO: support negative precision, i.e. rounding to whole places
    fn format(&self, precision: isize) -> String {
        if !self.is_integer() || precision != 0 {
            match self.to_f64() {
                Some(float) => float.format(precision),
                None => {
                    // TODO: find a better way to format large rationals
                    format!("{}/{}", self.numer(), self.denom())
                }
            }
        } else {
            // self.is_integer() && precision == 0
            self.numer().to_string()
        }
    }
}

impl Formatter for Quantity {
    fn format(&self, precision: isize) -> String {
        format!("{} {}", self.mag.format(precision), self.units)
    }
}

impl Formatter for Value {
    fn format(&self, precision: isize) -> String {
        match self {
            Value::Number(rat) => rat.format(precision),
            Value::Quantity(q) => q.format(precision),
            Value::Unit(units) => units.to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use ::pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn number_formatting() {
        macro_rules! cases {
            [
                $( $prec:literal => {
                    $( $val:literal => $exp:literal ),+ $(,)?
                } );+ $(;)?
            ] => {
                [ $( $(
                    (stringify!($val), $crate::rat_util_macros::ratf!($val as f64), $prec, $exp),
                    (
                        stringify!($val),
                        $crate::rat_util_macros::ratf!(-$val as f64),
                        $prec,
                        if $exp.chars().all(|c| c == '0' || c == '.') { $exp } else { concat!("-", $exp) }
                    ),
                 )+ )+ ]
            };
        }

        let cases = cases! [
            -2 => {
                1234 => "1200",
                1256 => "1300",
                1234.5678 => "1200",
                1256.3478 => "1300",
                1 => "0",
                0 => "0",
                12 => "0",
                56 => "100",
                12.34 => "0",
                56.78 => "100",
            };
            -1 => {
                12 => "10",
                5 => "10",
                7 => "10",
                1 => "0",
                4 => "0",
                0 => "0",
                1234 => "1230",
                2345.678 => "2350",
            };
            0 => {
                0 => "0",
                1 => "1",
                29384 => "29384",
                123.345 => "123",
            };
            1 => {
                0 => "0.0",
                1 => "1.0",
                123 => "123.0",
                123.5 => "123.5",
                123.456 => "123.5",
                123.123 => "123.1",
            };
            2 => {
                0 => "0.00",
                1 => "1.00",
                123 => "123.00",
                987.65 => "987.65",
                123.456 => "123.46",
                123.123 => "123.12",
            };
        ];

        for (valstr, value, precision, expected) in cases {
            println!("Rounding {} with precision {} ...", valstr, precision);
            assert_eq!(expected, value.format(precision));
        }
    }
}
