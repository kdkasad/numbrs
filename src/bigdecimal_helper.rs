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

use bigdecimal::BigDecimal;

#[allow(unused_macros)]
macro_rules! bigdec {
    ($lit:literal) => {{
        use std::str::FromStr;
        bigdecimal::BigDecimal::from_str(stringify!($lit)).expect("Invalid decimal float literal")
    }};
}

pub trait BigDecimalPowExt {
    fn pow(self, exp: u32) -> BigDecimal;
}

impl BigDecimalPowExt for BigDecimal {
    fn pow(self, exp: u32) -> Self {
        let (mut digits, mut scale) = self.into_bigint_and_exponent();
        digits = digits.pow(exp);
        scale *= exp as i64;
        BigDecimal::new(digits, scale)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn bigdecimal_pow() {
        macro_rules! case {
            ( $a:literal, $b:literal ) => {
                (bigdec!($a), $a as f64, $b)
            };
        }

        let cases: [(BigDecimal, f64, u32); 4] = [
            case!(2.5, 2),
            case!(2, 2),
            case!(10.2349, 2),
            case!(29384293, 2),
        ];

        for (bd, f, exp) in cases {
            let bd = bd.pow(exp);
            let f = f.powf(exp.into());
            assert_eq!(bd.to_string(), f.to_string());
        }
    }
}
