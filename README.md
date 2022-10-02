# Numbrs

A text-based calculator written in Rust.

Numbrs consists of both a library and a binary crate.
The library contains all of the functions for processing input and performing
calculations. The binary is a command-line application that provides a user
interface to the Numbrs library.

## Installation

Currently, Numbrs must be built from sources.
Don't worry, though—it's easy.
Cargo is used for this.

### Building

Simply clone the Git repository or download a release tarball. Then run `cargo
build --release` to build the program:

	$ git clone https://github.com/kdkasad/numbrs
	$ cd numbrs
	$ cargo build --release

### Installing

Once built, copy `target/release/numbrs` to a directory in your `PATH` and make
it executable:

	# install -m 755 target/release/numbrs /usr/local/bin/numbrs

## Usage

Numbrs is a text-based calculator. To use it, just type an expression and the
result will be printed.

### Operators

Numbrs supports many operators which are used to perform operations on numbers.

#### Grouping

Parentheses, braces, and brackets are supported for grouping expressions.  
Example:

	$ numbrs
	> (1 + 2) * 3 + 4
	13
	> (1 + 2) * [3 + 4]
	21
	> 1 + 2 * {3 + 4}
	15

#### Binary operators

Binary operators accept 2 operands using infix notation.  
Example:

	$ numbrs
	> 1 + 2
	3

| Operator | Operation       | Precedence | Associativity |
| ---      | ---             | ---        | ---           |
| `:=`     | Assign variable | (10, 1)\*  | Right         |
| `+`      | Add             | 3          | Left          |
| `-`      | Subtract        | 3          | Left          |
| `*`      | Multiply        | 5          | Left          |
| `/`      | Divide          | 5          | Left          |
| `^`      | Exponentiate (raise to power) | 7 | Right    |

Operators with higher precedence will be executed first. Operator precedence in
Numbrs follows standard mathematical order of operations when possible.  
For example, `1 + 2 * 3 + 4` is equivalent to `(1 + (2 * 3)) + 4`.

\* The assignment operator is special: it has a priority of 10 on the left side
and 1 on the right side. This means `a + b := c + d` is equivalent to
`a * (b := (c + d))`.

### Variables

Values can be stored in variables. Variable names can contain letters, numbers,
and underscores. However, a variable name cannot start with a number.

The `:=` assignment operator is used to assign variables. It has lower
precedence than all other operators. It is also right-associative, meaning
`a := b := c` is equivalent to `a := (b := c)`.

To unassign/clear a variable, assign it a value of `_` (underscore).
See the [Special Variables](#special-variables) section below for details.

#### Special Variables

The default Numbrs runtime has some special variables. These
variables can be used like normal variables, but they have some
special behaviors.

##### Underscore (`_`)
The underscore variable has a numerical value of 0, but it can be used
to unassign/clear variables. When assigned to an existing variable, that
variable will be removed from the environment.  
Example:

	$ numbrs
	> foo := 123
	123
	> foo
	123
	> foo := _
	0
	> foo
	Error: undefined variable: 'foo'

The underscore cannot be assigned. An error message will be printed if you
attempt to:

	$ numbrs
	> _ := 123
	Error: Can't assign special variable `_`

##### `_prec`
See the [Output Formatting/Precision](#output-formattingprecision)
section below.

#### Output Formatting/Precision

It is possible to specify the precision with which to output numbers using the
`_prec` variable.
When `_prec` is set, results will be printed with `_prec` decimal places.
By default, this variable is set to the value `5`.

The number stored in `_prec` must be a natural number (i.e. positive integer).
If a non-natural value is assigned, an error occurs and a message is printed.

## Why is it called Numbrs?

Numbrs is inspired by <https://github.com/nkanaev/numb> and is written in Rust.
The "numb" part comes from the original program and "rs" is the file
extension/common abbreviation for Rust. Together, they form "numbrs".
Conveniently, it can be pronounced like the word "numbers".

## Copyright

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
