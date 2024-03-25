# Numbrs

A text-based calculator written in Rust.

Numbrs consists of both a library and a binary crate.
The library contains all of the functions for processing input and performing
calculations. The binary is a command-line application that provides a user
interface to the Numbrs library.

## Project goal

The goal of Numbrs is twofold:
  1. For me to learn and practice programming in Rust.
  2. To build a calculator which can handle the following features:
     - [x] Arithmetic
     - [ ] Bitwise operations
     - [ ] Number base conversion
     - [x] Unit/quantity conversion:
        - [x] Define units
        - [x] Create quantities
        - [x] Perform arithmetic on quantities
        - [x] Convert quantities between units
        - [ ] (optional) Define custom base quantities (e.g. currency)
    - [ ] GCD and LCM calculations
    - [ ] Modular arithmetic

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

Parentheses, braces, and brackets (`()`, `[]`, and `{}`, respectively) are supported for grouping expressions.  
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

| Operator  | Operation                     | Binding power | Associativity |
| ---       | ---                           | ---           | ---           |
| `=`       | Assign variable               | (10, 1)\*     | Right         |
| `:=`      | Assign unit                   | (10, 1)\*     | Right         |
| `to`      | Convert units\*\*             | 4             | Left          |
| `+`       | Add                           | 3             | Left          |
| `-`       | Subtract                      | 3             | Left          |
| `*`       | Multiply                      | 5             | Left          |
| `/`       | Divide                        | 5             | Left          |
| `^`       | Exponentiate (raise to power) | 6             | Right         |

Operators with higher binding power will be executed first. Operator binding power in
Numbrs follows standard mathematical order of operations when possible.  
For example, `1 + 2 * 3 + 4` is equivalent to `(1 + (2 * 3)) + 4`.

\* The assignment operators are special: they have a priority of 10 on the left
side and 1 on the right side. This means `a + b = c + d` is equivalent to `a *
(b = (c + d))`.

\*\* The `to` operator is discussed in more detail in the
[Converting Units section](#converting-units).

#### Unary (prefix) operators

Unary operators support one operand and are specified in prefix notation.  
Example:

    $ numbrs
    > -1
    -1
    > 2 * -3
    -6

| Operator | Operation | Binding power |
| ---      | ---       | ---           |
| `+`      | Add       | 11            |
| `-`      | Subtract  | 11            |

Both unary additions and subtractions use 0 as the LHS, so `-4` is equivalent to
`0 - 4`.

All prefix operators have higher binding power than all infix operators, so
`-4 <operator> 2` will always result in `(-4) <operator> 2` and not
`-(4 <operator> 2)`.

#### Implicit operations

It is often useful to omit the multiplication symbol between terms to improve
readability. This is common in the world of mathematics, and is supported in
Numbrs.

For example, to specify a quantity of "3 meters", it's unwieldy to write it as
`3 * m`. If two numbers or variables are placed next to each other without an
operator in between, multiplication is implied. This means you can simply write
`3 m` to mean "3 meters".

The same applies for two units or variables, so a Newton can be represented as
`kg m/s^2` rather than `kg * m/s^2`. The expression `2 3` is also interpreted as
`2 * 3`, but that is disgusting syntax and should not be used.

### Variables

Values can be stored in variables. Variable names can contain letters, numbers,
and underscores. However, a variable name cannot start with a number.

The `=` assignment operator is used to assign variables. It has lower binding
power than all other operators on its right side, and higher than all others on
its left. This means that everything to the right of the `=` symbol will be used
as the value, but only one term on the left is used as the identifier name to
assign to. This makes assignment right-associative, meaning `a = b = c` is
equivalent to `a = (b = c)`.

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
    > foo = 123
    123
    > foo
    123
    > foo = _
    0
    > foo
    Error: undefined variable: 'foo'

The underscore cannot be assigned. An error message will be printed if you
attempt to:

    $ numbrs
    > _ = 123
    Error: Can't assign special variable `_`

##### `_prec`
See the [Output Formatting/Precision](#output-formattingprecision)
section below.

#### Output Formatting/Precision

It is possible to specify the precision with which to output numbers using the
`_prec` variable.
When `_prec` is set, results will be printed with `_prec` decimal places.
By default, this variable is set to the value `5`.

The number stored in `_prec` must be an integer.
If a non-integer value is assigned, an error occurs and a message is printed.

The positive value *N* will round to *N* places to the right of the decimal
point. The negative value *-N* will round so that *N* places to the left of the
decimal point contain zeros.

### Units

Numbrs supports processing amounts of physical quantities, commonly known as
units. For example, the meter (m) is the standard unit of the physical quantity
length.

#### Base quantities

Units are defined as a number times either another unit or a physical quantity.
The physical quantities currently defined in Numbrs are:

 - Length
 - Mass
 - Time
 - Current
 - Temperature
 - Amount of substance
 - Luminous intensity
 - (Digital) Data

Each physical quantity is expressed as "screaming snake case" variable. For
example, the physical quantity variable for length is `LENGTH` and the one for
Amount of Substance is `AMOUNT_OF_SUBSTANCE`.

A physical quantity has no amount; it is just the quantity. However, the base
unit for each physical quantity is defined as 1 of the physical quantity
variable. For example, `1 LENGTH` is functionally equivalent to `1 m`. For this
reason, the physical quantity variables shouldn't be used except for defining
base units.

A quantity is a number times a physical quantity or a unit. A quantity can also be
a number times multiple units multiplied together.

For example, `2 meters^3`, `2 m * m * m`, `2 m^3`, and `2 m^5 / 1 m^2` are all
different ways of expressing the same quantity.

Any quantity can be a unit. A unit is really just a quantity that is given a
name. Under the hood, the `newton` unit is just defined as `1 kg m s^-2`.

#### Defining units

Defining a unit is similar to assigning a variable, except unit assignments use
the `:=` operator.

The following snippet would define a new unit `foot` based off of the existing
unit `meter`. Then it defines a new unit `feet` which is equivalent to `foot`.

    $ numbrs
    > foot := meter / 3.2808
    foot
    > feet := foot
    feet

#### Unit prefixes and suffixes

Units are often referred to with prefixes. For example, the "kilogram" is just
the "gram" with a prefix of "kilo" meaning 1,000. Numbrs automatically
translates prefixes, so they do not need to be defined manually.

Abbreviated prefixes are also supported, e.g. "k" for "kilo" and "M" for "mega".
Prefixes (like all identifier names) are case-sensitive. "mg" means "milligrams"
whereas "Mg" means "megagrams".

IEC prefixes are also supported, so "kibi" is like "kilo" only it scales the
value by 1024 rather than 1000. This means "kB" and "KiB" are different. They
represent 1000 bytes and 1024 bytes, respectively.

For example, the following snippet will work even though `kg` is not explicitly defined:

    $ numbrs
    > gram := MASS
    gram
    > g := gram
    g
    > 1 kg to gram
    1000 gram

Plural suffixes for units are also supported in Numbrs. If a token in an
expression ends with an `s` and is not defined, the `s` is removed and the
resulting token is used. This allows one to specify the quantity "14 meters" as
`14 meters` even though the unit `meters` is not explicitly defined.

#### Converting units

The most useful feature of Numbrs' unit processing is the ability to combine
them and convert between them. Units can be converted using the `to` keyword. It
acts like an operator with a binding power in between addition and
multiplication. This is because units are often defined as multiple different
units multiplied together. Of course, parentheses can always be used to manually
specify grouping.

The following example should make this a bit clearer:

    $ numbrs
    > a = 2 kg
    2 kg
    > b = 4 m/s^2
    4 m s^-2
    > c = a * b to newtons
    8 newton
    > c / 3 to kg m/s^2

This will define a few variables with units, then finds the value of `c` divided
by 3 expressed in the units `kg m s^-2`. The `to` operator has a lower binding
power than multiplication and division, so it is parsed as `(c / 3) to (kg
m/s^2)`. However, `to` has higher binding power than addition, so `c + 1 N to kg
m/s^2` is parsed as `c + (1 N to (kg m/s^2))`.

Only units representing equivalent physical quantities can be converted. You
cannot convert, for example, 1 meter to minutes:

    $ numbrs
    > 1 meter to minutes
    Error: Evaluation error: Cannot convert between units that describe different quantities: `meter` and `minute`

## Why is it called Numbrs?

Numbrs is inspired by
[nkanaev's *numb* program](https://github.com/nkanaev/numb)
and is written in Rust. The "numb" part of the name comes from the original
program and "rs" is the file extension/common abbreviation for Rust. Together,
they form "numbrs". Conveniently, it can be pronounced like the word "numbers".

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
