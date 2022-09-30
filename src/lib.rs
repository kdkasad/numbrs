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

extern crate num;
extern crate strum;
extern crate strum_macros;
extern crate thiserror;

#[cfg(test)]
extern crate pretty_assertions;

pub mod ast;
pub mod dimension;
pub mod eval;
pub mod lexer;
pub mod operation;
pub mod parser;
mod rat_util_macros;
pub mod unit;
