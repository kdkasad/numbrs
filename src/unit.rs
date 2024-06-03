/*

unit.rs - Unit handling for Numbrs
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

//! # Unit handling
//!
//! Units are named amounts of physical quantities that make it easier to work
//! with physical quantities.
//!
//! ## What makes a [`Unit`]
//!
//! A unit has three parts:
//!   1. A name
//!   2. An exponent
//!   3. A scale
//!   4. An offset
//!   5. A dimension
//!
//! The *name* of a unit is a string which represents the unit in expressions.
//!
//! The *exponent* of a unit is the exponent a given unit has within a [unit
//! list](#unit-lists). See the explanation of unit lists below for more
//! details.
//!
//! The *scale* of a unit is the ratio between the unit and the underlying base
//! quantity. This is also sometimes called the *magnitude*.
//!
//! The *offset* of a unit is a constant offset from the underlying base
//! quantity. For example, Celsius and Kelvin units have the same magnitude, but
//! Celsius is offset by 273.15 from Kelvin.
//!
//! The *dimension* of a unit defines the physical quantity that it represents.
//! A Newton (a unit of force) would have the dimension `L * M * T^-2`, where
//! `L` represents Length, `M` represents Mass, and `T` represents Time.
//!
//! ## Unit lists
//!
//! Quantities are often expressed as an amount of multiple units, not just one.
//! For example, *60 km/hr* is a single quantity with two units, *km* and *hr*.
//! Similarly, work can be specified in Newton-meters, e.g. *14 N m*, which is a
//! single quantity with two units *N* and *m*.
//!
//! Each unit in a unit list is multiplied together.
//!
//! This concept of unit lists is implemented by the [`Units`] type. It is very
//! similarly named to the [`Unit`] type, which is a little confusing. In nearly
//! all cases, if you're specifying an amount of physical quantities, you want
//! [`Units`] and not [`Unit`].
//!
//! The *exponent* component of each unit is especially useful for unit lists.
//! The unit list *km/hr* (kilometers per hour) has two units, *km* and *hr*,
//! with exponents *1* and *-1*, respectively. This prevents us from having to
//! define a new unit equal to *hr^-1* in order to include it in the unit list.

use std::{
    fmt,
    fmt::Display,
    iter::IntoIterator,
    ops::{Deref, DerefMut, Div, DivAssign, Mul, MulAssign},
};

use num::BigRational;

use crate::{ast::Quantity, dimension::Dimension, eval::EvalError, rat_util_macros::rat};

/// # Unit of measurement
///
/// See the [module-level documentation][1] for details.
///
/// [1]: crate::unit
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Unit {
    /// Name of the [`Unit`]; a string which represents the unit in expressions
    pub name: String,

    /// The *exponent* of a unit is the exponent a given unit has within a [unit
    /// list][1]. See the [module-level documentation][1] for more explanation.
    ///
    /// [1]: crate::unit#unit-lists
    pub exponent: i32,

    /// The ratio between the unit and the underlying base quantities it
    /// represents. This is also sometimes called the *magnitude*.
    pub scale: BigRational,

    /// The *offset* of a unit is a constant offset from the underlying base
    /// quantity. For example, Celsius and Kelvin units have the same magnitude, but
    /// Celsius is offset by 273.15 from Kelvin.
    pub offset: BigRational,

    /// The *dimension* of a unit defines the physical quantity that it
    /// represents. A Newton (a unit of force) would have the dimension `L * M *
    /// T^-2`, where `L` represents Length, `M` represents Mass, and `T`
    /// represents Time.
    pub dimension: Dimension,
}

impl Unit {
    /// # Test whether two units conform.
    ///
    /// Units conform when their dimensions are equal, i.e. they represent the
    /// same physical quantities. For example, *miles/hour* and *meters/second*
    /// conform because they both represent distance (or length) over time.
    pub fn conforms_to(&self, other: &Self) -> bool {
        self.dimension == other.dimension
    }

    /// # Create an new unit
    pub fn new(
        name: &str,
        exponent: i32,
        scale: BigRational,
        offset: BigRational,
        dimension: Dimension,
    ) -> Self {
        Self {
            name: name.to_owned(),
            exponent,
            scale,
            offset,
            dimension,
        }
    }
}

impl Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.exponent {
            0 => Ok(()),
            1 => write!(f, "{}", self.name),
            _ => write!(f, "{}^{}", self.name, self.exponent),
        }
    }
}

/// # List of units
///
/// See the [module-level documentation][1] for details.
///
/// [1]: crate::unit
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Units(Vec<Unit>);

// Allow dereferencing the units object to reach the underlying vector
impl Deref for Units {
    type Target = Vec<Unit>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// Same as above but mutable
impl DerefMut for Units {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Units {
    /// # Calculate the dimension of a list of units.
    ///
    /// Raises each unit's dimension to its exponent, then multiplies the
    /// results together.
    pub fn dimension(&self) -> Dimension {
        self.0
            .iter()
            .map(|unit| unit.dimension.pow(unit.exponent))
            .fold(Dimension::new(), |sum, item| sum * item)
    }

    /// # Test if a unit list conforms to another unit list.
    ///
    /// See [`Unit::conforms_to()`] for the meaning of conformity.
    pub fn conforms_to(&self, other: &Self) -> bool {
        self.dimension() == other.dimension()
    }

    /// # Create a new empty unit list
    pub const fn new() -> Self {
        Self(Vec::new())
    }

    /// # Tests if a list of units is dimensionless
    ///
    /// A unit list is dimensionless if it has a [pure dimension][1].
    ///
    /// [1]: Dimension::is_pure()
    pub fn is_dimensionless(&self) -> bool {
        self.dimension().is_pure()
    }

    /// # Raise the unit list to an integer power
    pub fn pow_assign(&mut self, exp: i32) {
        self.0.iter_mut().for_each(|unit| unit.exponent *= exp);
    }

    /// # Scale `n` by the unit list's scale
    ///
    /// Multiply `n` times the combined scales of the units. This gets the
    /// quantity `n` in terms of base physical quantity amounts.
    ///
    /// For example, if the unit list `cheese` has a scale of 15, then
    /// `cheese.scale(rat!(3))` would return `45`.
    pub fn scale(&self, n: &BigRational) -> BigRational {
        if self.0.is_empty() {
            return n.clone();
        }

        let mut result = n.clone();
        if self.0.len() == 1 && self.0[0].exponent == 1 {
            let unit = &self.0[0];
            result += &unit.offset;
            result *= &unit.scale;
        } else {
            for unit in &self.0 {
                result *= unit.scale.pow(unit.exponent);
            }
        }
        result
    }

    /// # Inverse of the [`scale()`][Self::scale] function.
    pub fn descale(&self, n: &BigRational) -> BigRational {
        if self.0.is_empty() {
            return n.clone();
        }

        let mut result = n.clone();
        if self.0.len() == 1 && self.0[0].exponent == 1 {
            let unit = &self.0[0];
            result /= &unit.scale;
            result -= &unit.offset;
        } else {
            for unit in &self.0 {
                result /= unit.scale.pow(unit.exponent);
            }
        }
        result
    }

    /// # Get the combined scale of the unit list.
    ///
    /// Raises the scale of each unit to that unit's exponent, then multiplies
    /// all of the results together.
    pub fn aggregate_scales(&self) -> BigRational {
        let mut result = rat!(1);
        for unit in &self.0 {
            result *= unit.scale.pow(unit.exponent);
        }
        result
    }

    /// # Create a new unit from this unit list.
    ///
    /// Creates a new [`Unit`] named `name` which has an identical scale and
    /// dimension to this [`Units`] list.
    pub fn collapse_to(&self, name: &str) -> Unit {
        Unit::new(name, 1, self.aggregate_scales(), rat!(0), self.dimension())
    }
}

impl Default for Units {
    fn default() -> Self {
        Self::new()
    }
}

impl Mul for Units {
    type Output = Self;

    fn mul(mut self, rhs: Self) -> Self::Output {
        self *= rhs;
        self
    }
}

impl MulAssign for Units {
    #[allow(clippy::suspicious_op_assign_impl)]
    fn mul_assign(&mut self, rhs: Self) {
        for unit in rhs.0 {
            if let Some(idx) = self
                .0
                .iter()
                .position(|x| x.name == unit.name && x.dimension == unit.dimension)
            {
                self.0[idx].exponent += unit.exponent;
            } else {
                self.0.push(unit);
            }
        }
    }
}

impl Div for Units {
    type Output = Self;

    fn div(mut self, rhs: Self) -> Self::Output {
        self /= rhs;
        self
    }
}

impl DivAssign for Units {
    fn div_assign(&mut self, rhs: Self) {
        for mut unit in rhs.0 {
            if self.0.contains(&unit) {
                // We are sure the element exists because self.0.contains(...)
                // is true, so we can unwrap.
                #[allow(clippy::unwrap_used)]
                self.0
                    .remove(self.0.iter().position(|x| *x == unit).unwrap());
            } else if let Some(idx) = self
                .0
                .iter()
                .position(|x| x.name == unit.name && x.dimension == unit.dimension)
            {
                self.0[idx].exponent -= unit.exponent;
            } else {
                unit.exponent = -unit.exponent;
                self.0.push(unit);
            }
        }
    }
}

impl<T> From<T> for Units
where
    T: IntoIterator<Item = Unit>,
{
    fn from(slice: T) -> Self {
        Self(slice.into_iter().collect())
    }
}

impl Display for Units {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.len() == 2
            && ((self.0[0].exponent == 1 && self.0[1].exponent == -1)
                || (self.0[1].exponent == 1 && self.0[0].exponent == -1))
        {
            write!(f, "{}/{}", self.0[0].name, self.0[1].name)
        } else {
            write!(
                f,
                "{}",
                self.0
                    .iter()
                    .filter(|unit| unit.exponent != 0)
                    .map(|unit| unit.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            )
        }
    }
}

/// Convert a magnitude between two conforming units.
pub fn convert(n: &BigRational, from: &Units, to: &Units) -> Result<BigRational, EvalError> {
    if !from.conforms_to(to) {
        Err(EvalError::ConvertNonConformingUnits(
            Box::new(from.clone()),
            Box::new(to.clone()),
        ))
    } else {
        Ok(to.descale(&from.scale(n)))
    }
}

impl Quantity {
    /// Convert this quantity to another set of units.
    ///
    /// The current and destination unit sets must conform. If they don't, an
    /// error is returned.
    pub fn convert_to(mut self, units: Units) -> Result<Self, EvalError> {
        if self.units.conforms_to(&units) {
            self.mag = convert(&self.mag, &self.units, &units)?;
            self.units = units;
            Ok(self)
        } else {
            Err(EvalError::ConvertNonConformingUnits(
                Box::new(self.units),
                Box::new(units),
            ))
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;
    use crate::rat_util_macros::rat;

    fn newton_parts() -> Units {
        Units::from([
            Unit {
                name: String::from("m"),
                exponent: 1,
                scale: rat!(1),
                offset: rat!(0),
                dimension: Dimension::from(&[1, 0, 0][..]),
            },
            Unit {
                name: String::from("kg"),
                exponent: 1,
                scale: rat!(1),
                offset: rat!(0),
                dimension: Dimension::from(&[0, 1, 0][..]),
            },
            Unit {
                name: String::from("s"),
                exponent: -2,
                scale: rat!(1),
                offset: rat!(0),
                dimension: Dimension::from(&[0, 0, 1][..]),
            },
        ])
    }

    fn newton() -> Units {
        Units::from([Unit {
            name: String::from("N"),
            exponent: 1,
            scale: rat!(1),
            offset: rat!(0),
            dimension: Dimension::from(&[1, 1, -2][..]),
        }])
    }

    /// Tests collecting a [Units]'s dimensions into one dimension.
    /// Conveniently also tests the multiplication and exponentiation functions
    /// for [Dimensions][Dimension].
    ///
    /// [1]: crate::dimension::Dimension::Mul::mul
    /// [2]: crate::dimension::Dimension::exp
    #[test]
    fn units_dimension() {
        let newton = newton_parts();
        assert_eq!(newton.dimension(), Dimension::from(&[1, 1, -2][..]));
    }

    /// Tests conformity checking
    #[test]
    fn conformity() {
        let n1 = newton_parts();
        let n2 = newton();
        assert!(n1.conforms_to(&n2));
        assert!(n2.conforms_to(&n1));
    }

    /// Tests checking of dimensionlessness
    #[test]
    fn dimensionless() {
        let a = Units::new();
        let b = Units::from([Unit {
            name: String::new(),
            exponent: 1,
            scale: rat!(1),
            offset: rat!(0),
            dimension: Dimension::new(),
        }]);
        let c = Units::from([Unit {
            name: String::new(),
            exponent: 0,
            scale: rat!(1),
            offset: rat!(0),
            dimension: Dimension::from(&[1, 1, 1][..]),
        }]);
        assert!(a.is_dimensionless());
        assert!(b.is_dimensionless());
        assert!(c.is_dimensionless());
    }

    macro_rules! units {
        ( $( $a:tt, $b:literal),* ) => {
            Units(vec![
                $(Unit {
                    name: stringify!($a).to_string(),
                    exponent: $b,
                    scale: rat!(1),
                    offset: rat!(0),
                    dimension: Dimension::new(),
                },)*
            ])
        };
    }

    /// Tests [Display] implementation for [Units]
    #[test]
    fn units_display() {
        let tests: Vec<(Units, &str)> = vec![
            (units!(m, 1, s, -1), "m/s"),
            (units!(m, 1), "m"),
            (units!(m, 2, s, -1), "m^2 s^-1"),
        ];
        for (units, expectation) in tests {
            assert_eq!(units.to_string(), expectation);
        }
    }

    /// Test division of [Units]
    #[test]
    fn units_division() {
        let mut a = Units(vec![Unit {
            name: "m".to_string(),
            exponent: 2,
            scale: rat!(1),
            offset: rat!(0),
            dimension: Dimension::from(&[1, 0, 0][..]),
        }]);

        let b = Units(vec![Unit {
            name: "m".to_string(),
            exponent: 1,
            scale: rat!(1),
            offset: rat!(0),
            dimension: Dimension::from(&[1, 0, 0][..]),
        }]);

        a /= b.clone();

        let expected_result = Units(vec![Unit {
            name: "m".to_string(),
            exponent: 1,
            scale: rat!(1),
            offset: rat!(0),
            dimension: Dimension::from(&[1, 0, 0][..]),
        }]);

        assert_eq!(a, expected_result);

        a = Units(vec![
            Unit {
                name: "m".to_string(),
                exponent: 1,
                scale: rat!(1),
                offset: rat!(0),
                dimension: Dimension::from(&[1, 0, 0][..]),
            },
            Unit {
                name: "m".to_string(),
                exponent: 1,
                scale: rat!(1),
                offset: rat!(0),
                dimension: Dimension::from(&[1, 0, 0][..]),
            },
        ]);

        a /= b;

        assert_eq!(a, expected_result);
    }

    /// Test conversion of pure quantities
    #[test]
    fn convert_pure() {
        let a = Units::from([Unit::new("a", 1, rat!(5), rat!(0), Dimension::default())]);
        let b = Units::from([Unit::new("b", 1, rat!(3), rat!(0), Dimension::default())]);

        let mut val = rat!(3);
        val = convert(&val, &a, &b).unwrap();
        assert_eq!(val, rat!(5));
    }
}
