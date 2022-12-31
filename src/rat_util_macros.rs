/*

rat_util_macros.rs - Utility macros for rational types
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

#![allow(unused_macros)]

//! Helper macros for creating [BigRational][br] structs
//!
//! [br]: num_rational::BigRational

/// Create a [BigRational] from integer(s).
///
/// Takes a numerator and a denominator argument. The denominator can be omitted
/// to create a [BigRational] that represents an integer.
macro_rules! rat {
    ( $a:expr ) => {
        ::num::BigRational::from_integer(::num::BigInt::from($a))
    };
    ( $a:expr , $b:expr ) => {
        ::num::BigRational::new(::num::BigInt::from($a), ::num::BigInt::from($b))
    };
}

/// Create a [BigRational] from a floating-point value.
///
/// This is just a shortcut for [BigRational::from_f64()].
///
/// # Panics
///
/// This macro unwraps the [Result] returned by [BigRational::from_f64()], so it
/// will panic if an [Err][Result::Err] variant is returned.
macro_rules! ratf {
    ( $f:expr ) => {
        ::num::BigRational::from_float($f).unwrap()
    };
}

// Required to be able to import macros in other modules
#[allow(unused_imports)]
pub(crate) use {rat, ratf};
