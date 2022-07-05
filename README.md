# numbrs

A text-based calculator written in Rust

## Installation

Numbrs currently is not in any package managers and must be built from source.
Cargo must be installed for this.

### Building

Simply clone the Git repository or download a release tarball. Then run `cargo
build --release` to build the program:

	$ git clone https://github.com/kdkasad/numbrs
	$ cd numbrs
	$ cargo build --release

### Installing

Once built, copy `target/release/numbrs` to a directory in your `PATH` and make
it executable:

	$ install -m 755 target/release/numbrs /usr/local/bin/numbrs

## Usage

Numbrs is a text-based calculator. To use it, just type an expression and the
result will be printed.

### Operators

Parentheses are supported for grouping expressions.  
Example:

	$ numbrs
	> (1 + 2) * 3 + 4
	13

#### Binary operators

Binary operators accept 2 operands using infix notation.  
Example:

	$ numbrs
	> 1 + 2
	3

| Operator | Operation       | Precedence | Associativity |
| ---      | ---             | ---        | ---           |
| `+`      | Add             | 20         | Left          |
| `-`      | Subtract        | 20         | Left          |
| `*`      | Multiply        | 40         | Left          |
| `/`      | Divide          | 40         | Left          |
| `:=`     | Assign variable | 100        | Right         |

Operators with higher precedence will be executed first. Operator precedence in
numbrs follows standard mathematical order of operations.  
For example, `1 + 2 * 3 + 4` is equivalent to `1 + (2 * 3) + 4`.

#### Unary operators

Unary operators only support one operand which comes after the operator.  
Example:

	$ numbrs
	> -2
	-2
	> 3 * -4
	-12
	> -3 * +4
	-12

| Operator | Operation | Precedence |
| ---      | ---       | ---        |
| `+`      | Add       | 60         |
| `-`      | Subtract  | 60         |

### Variables

Values can be stored in variables. Variable names can contain letters, numbers,
and underscores. However, a variable name cannot start with a number.

The `:=` assignment operator is used to assign variables. It has higher
precedence than all other operators. It is also right-associative, meaning `a
:= b := c` is equivalent to `a := (b := c)`.

To unassign/clear a variable, assign it a value of `_`. See the [Special
Variables](#special-variables) section below for details.

#### Special Variables

Special variables cannot be assigned. An error message will be printed if you
attempt to:

	$ numbrs
	> _ := 123
	Error: can't assign special variable: '_'

**`_` (underscore)**: The underscore has a numerical value of 0, but it can be used
to unassign/clear variables. When assigned to an existing variable, that
variable will then be removed from the runtime.  
Example:

	$ numbrs
	> foo := 123
	123
	> foo + 1
	124
	> foo := _
	0
	> foo
	Error: undefined variable: 'foo'

## Why is it called numbrs?

Numbrs is inspired by <https://github.com/nkanaev/numb> and is written in Rust.
The "numb" part comes from the original program and "rs" is the file
extension/common abbreviation for Rust. Together, they form "numbrs" which is
conveniently similar to the word "numbers".

## Copyright

Copyright (C) 2022  Kian Kasad

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, version 3 of the License.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
