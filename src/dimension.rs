/*

dimension.rs - Dimension handling for Numbrs
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

//! Dimension handling for Numbrs.
//!
//! Dimensions are how units map to physical quantities.

use std::{
    cmp::min,
    ops::{Index, IndexMut, Mul, MulAssign},
};

use strum::{EnumCount, IntoEnumIterator};
use strum_macros::{Display, EnumCount as EnumCountMacro, EnumIter, EnumVariantNames};

/// Base quantity/dimension type.
///
/// Each variant represents a [basic physical dimension/quantity][1].
///
/// [1]: https://en.wikipedia.org/wiki/Physical_quantity#Dimensions
#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumCountMacro, EnumIter, EnumVariantNames, Display)]
#[strum(serialize_all = "SCREAMING_SNAKE_CASE")]
pub enum BaseQuantity {
    Length,
    Mass,
    Time,
    Current,
    Temperature,
    AmountOfSubstance,
    LuminousIntensity,
    Data,
}

/// Dimension specifier.
///
/// Specifies a dimension of measure for units by storing an array of exponents
/// indexed by base quantities.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Dimension([i32; BaseQuantity::COUNT]);

impl Dimension {
    /// Test whether the dimension is pure.
    ///
    /// A *pure* dimension is one with zero exponent for all base quantities. In
    /// other words, a quantity with a pure dimension is just a numerical value.
    pub fn is_pure(&self) -> bool {
        self.0.iter().all(|x| *x == 0)
    }

    /// Create a new empty dimension
    pub const fn new() -> Self {
        Self([0; BaseQuantity::COUNT])
    }

    /// Raise a dimension to an integer power.
    ///
    /// Effectively multiplies each base quantity's exponent by the power.
    pub fn pow(mut self, pow: i32) -> Self {
        for bq in BaseQuantity::iter() {
            self[bq] *= pow;
        }
        self
    }
}

impl From<&[i32]> for Dimension {
    /// Creates a [Dimension] from a slice of [i32]'s.
    ///
    /// If the slice is smaller than the number of [base quantities][1], the
    /// dimension for each missing quantity is set to 0. If the slice is larger,
    /// the extra elements are ignored.
    ///
    /// [1]: BaseQuantity
    fn from(src: &[i32]) -> Self {
        let mut new = Self::new();
        let max = min(src.len(), BaseQuantity::COUNT);
        let src = &src[0..max];
        let dest = &mut new.0[0..max];
        dest.copy_from_slice(src);
        new
    }
}

impl Default for Dimension {
    fn default() -> Self {
        Self::new()
    }
}

impl Mul for Dimension {
    type Output = Self;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn mul(self, rhs: Self) -> Self::Output {
        let mut result = Dimension::new();
        for bq in BaseQuantity::iter() {
            result[bq] = self[bq] + rhs[bq];
        }
        result
    }
}

impl MulAssign for Dimension {
    #[allow(clippy::suspicious_op_assign_impl)]
    fn mul_assign(&mut self, rhs: Self) {
        for bq in BaseQuantity::iter() {
            self[bq] += rhs[bq]
        }
    }
}

impl Index<BaseQuantity> for Dimension {
    type Output = i32;

    fn index(&self, index: BaseQuantity) -> &Self::Output {
        &self.0[index as usize]
    }
}

impl IndexMut<BaseQuantity> for Dimension {
    fn index_mut(&mut self, index: BaseQuantity) -> &mut Self::Output {
        &mut self.0[index as usize]
    }
}

impl From<[i32; BaseQuantity::COUNT]> for Dimension {
    fn from(src: [i32; BaseQuantity::COUNT]) -> Self {
        Self(src)
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::{assert_eq, assert_ne};

    use super::*;

    /// Test multiplication of [Dimensions][Dimension]
    #[test]
    fn multiplication() {
        let a = Dimension::from(&[1, 2, 3][..]);
        let b = Dimension::from(&[4, 5, 6][..]);
        assert_eq!(a * b, Dimension::from(&[5, 7, 9][..]));
        assert_eq!(a.0[..3], [1, 2, 3]);
        assert_eq!(b.0[..3], [4, 5, 6]);
    }

    /// Test raising a [Dimension] to a power
    #[test]
    fn exponentiation() {
        let a = Dimension::from(&[1, 2, 3][..]);
        let pow = 4;
        assert_eq!(a.pow(pow), Dimension::from(&[4, 8, 12][..]));
        assert_eq!(a.0[..3], [1, 2, 3]);
    }

    /// Test checking if a [Dimension] is pure
    #[test]
    fn purity() {
        let dim = Dimension::from(&[0; BaseQuantity::COUNT][..]);
        assert!(dim.is_pure());
    }

    /// Test equality checking for [Dimensions][Dimension]
    #[test]
    fn equality() {
        let a = Dimension::from(&[1, 2, 3][..]);
        let b = Dimension::from(&[1, 2, 3][..]);
        let c = Dimension::from(&[4, 5, 6][..]);
        let d = Dimension::from(&[3, 2, 1][..]);
        assert_eq!(a, b);
        assert_ne!(a, c);
        assert_ne!(a, d);
        assert_eq!(d, d);
    }
}
