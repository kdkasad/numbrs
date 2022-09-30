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

//! Helper macros for creating [BigInt][1] and [BigRational][2] instances
//!
//! [1]: num_bigint::BigInt
//! [2]: num_rational::BigRational

macro_rules! bigint {
    ( $a:expr ) => {
        num::BigInt::from($a as i64)
    };
}

macro_rules! rat {
    ( $a:expr ) => {
        num::BigRational::from_integer(num::BigInt::from($a))
    };
    ( $a:expr , $b:expr ) => {
        num::BigRational::new(num::BigInt::from($a), num::BigInt::from($b))
    };
}

// Required to be able to import macros in other modules
pub(crate) use {bigint, rat};
