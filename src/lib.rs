/*

lib.rs - Numbrs library crate
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

// Don't allow unwrapping Results and Options
#![deny(clippy::unwrap_used)]
// Warn about additional compiler diagnostics. These don't affect the behavior
// or performance of Numbrs, but they generally lead to cleaner code.
#![warn(
    missing_docs,
    missing_copy_implementations,
    missing_debug_implementations,
    unreachable_pub,
    unused_extern_crates,
    macro_use_extern_crate
)]

//! # Numbrs
//!
//! Numbrs is a calculator that supports unit conversions.
//!
//! See the `README.md` file in the source repository for detailed documentation
//! about Numbrs.
//!
//! See the [`Runtime`][1] struct for a runtime which can be used to integrate
//! Numbrs into an application.
//!
//! [1]: crate::runtime::Runtime

pub mod affixes;
pub mod ast;
pub mod dimension;
pub mod eval;
pub mod format;
pub mod functions;
pub mod lexer;
pub mod operation;
pub mod parser;
mod rat_util_macros;
pub mod runtime;
pub mod unit;
