/*

affixes.rs - Unit prefix/suffix handling for Numbrs
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

use std::collections::HashMap;

use num::BigRational;
use once_cell::sync::Lazy;

use crate::{ast::Value, rat_util_macros::rat, unit::Unit};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Prefix {
    text: String,
    scale: BigRational,
    standalone: bool,
}

impl Prefix {
    /// Create a new prefix.
    pub fn new(text: &str, scale: BigRational, standalone: bool) -> Self {
        Self {
            text: text.to_owned(),
            scale,
            standalone,
        }
    }

    /// Get a reference to the prefix's text.
    pub fn text(&self) -> &str {
        self.text.as_ref()
    }

    /// Get a reference to the prefix's scale.
    pub fn scale(&self) -> &BigRational {
        &self.scale
    }

    /// Whether or not the [`Prefix`] also acts as a standalone variable
    /// representing its scale as a numeric value.
    ///
    /// For example, the prefix `kilo` is standalone, but `k` is not:
    /// ```txt
    /// $ numbrs
    /// > kilo
    /// 1000
    /// > k
    /// Error: Evaluation error: `k` not defined
    /// ```
    pub fn standalone(&self) -> bool {
        self.standalone
    }
}

/// Look up a unit in the environment with prefix and suffix substitution.
pub fn resolve_unit(name: &str, env: &HashMap<String, Value>) -> Option<Unit> {
    if let Some(unit) = try_get_unit(name, env) {
        return Some(unit);
    }

    // TODO: don't transform units which don't support suffixes, e.g. `kg`
    for (suffix, substitution) in standard_suffixes() {
        if let Some(base) = name.strip_suffix(suffix) {
            let mut new_name = String::with_capacity(name.len());
            new_name.push_str(base);
            new_name.push_str(substitution);
            if let Some(unit) = try_get_unit(&new_name, env) {
                return Some(unit);
            }
        }
    }

    None
}

/// Attempt to look up a unit with prefix substitution.
fn try_get_unit(name: &str, env: &HashMap<String, Value>) -> Option<Unit> {
    if let Some(Value::Unit(units)) = env.get(name) {
        return Some(units.collapse_to(name));
    }

    // Search for existing unit by stripping available prefixes
    for prefix in standard_prefixes() {
        if let Some(s) = name.strip_prefix(prefix.text()) {
            if let Some(Value::Unit(units)) = env.get(s) {
                if (*units).len() > 1 || (*units)[0].offset != rat!(0) {
                    return None;
                }
                // If a unit definition is found, create a new Unit with the
                // prefixed name and adjusted scale.
                let mut new = units.collapse_to(name);
                new.scale *= prefix.scale();
                return Some(new);
            }
        }
    }

    None
}

/// Attempt to get the scale value of a [Prefix].
pub fn try_get_prefix_scale(name: &str) -> Option<BigRational> {
    Some(
        standard_prefixes()
            .iter()
            .find(|prefix| prefix.standalone() && prefix.text() == name)?
            .scale()
            .clone(),
    )
}

macro_rules! prefixes {
    ( $( $name:literal $base:literal^$scale:literal $standalone:tt ),* $(,)? ) => {
        [ $(
            Prefix::new(
                $name,
                rat!($base).pow($scale),
                stringify!($standalone) == "y"
            ),
        )* ]
    };
}

/// # Standard prefixes for units.
/// 
/// This function returns a static slice of [`Prefix`] items which makes up the
/// list of standard unit prefixes.
///
/// Contains both [SI][1] and [IEC][2] prefixes.
/// Mostly derived from <https://frinklang.org/frinkdata/units.txt>.
///
/// [1]: https://en.wikipedia.org/wiki/International_System_of_Units#Prefixes
/// [2]: https://en.wikipedia.org/wiki/Byte#Units_based_on_powers_of_2
pub fn standard_prefixes() -> &'static [Prefix] {
    static PREFIXES: Lazy<[Prefix; 59]> = Lazy::new(|| {
        prefixes![
            "yotta"  10^24  y,
            "zetta"  10^21  y,
            "exa"    10^18  y,
            "peta"   10^15  y,
            "tera"   10^12  y,
            "giga"    10^9  y,
            "mega"    10^6  y,
            "myria"   10^4  y,
            "kilo"    10^3  y,
            "hecto"   10^2  y,
            "deca"    10^1  y,
            "deka"    10^1  y,
            "deci"   10^-1  y,
            "centi"  10^-2  y,
            "milli"  10^-3  y,
            "micro"  10^-6  y,
            "nano"   10^-9  y,
            "pico"  10^-12  y,
            "femto" 10^-15  y,
            "atto"  10^-18  y,
            "zepto" 10^-21  y,
            "yocto" 10^-24  y,

            "Y"  10^24   n, // yotta
            "Z"  10^21   n, // zetta
            "E"  10^18   n, // exa
            "P"  10^15   n, // peta
            "T"  10^12   n, // tera
            "G"   10^9   n, // giga
            "M"   10^6   n, // mega
            "k"   10^3   n, // kilo
            "h"   10^2   n, // hecto
            "da"  10^1   n, // deka
            "d"  10^-1   n, // deci
            "c"  10^-2   n, // centi
            "m"  10^-3   n, // milli
            "µ"  10^-6   n, // micro
            "u"  10^-6   n, // micro (ASCII alternative)
            "n"  10^-9   n, // nano
            "p"  10^-12  n, // pico
            "f"  10^-15  n, // femto
            "a"  10^-18  n, // atto
            "z"  10^-21  n, // zepto
            "y"  10^-24  n, // yocto

            "kibi"  1024^1  y,
            "mebi"  1024^2  y,
            "gibi"  1024^3  y,
            "tebi"  1024^4  y,
            "pebi"  1024^5  y,
            "exbi"  1024^6  y,
            "zebi"  1024^7  y,
            "yobi"  1024^8  y,

            "Ki"  1024^1  n, // kibi
            "Mi"  1024^2  n, // mebi
            "Gi"  1024^3  n, // gibi
            "Ti"  1024^4  n, // tebi
            "Pi"  1024^5  n, // pebi
            "Ei"  1024^6  n, // exbi
            "Zi"  1024^7  n, // zebi
            "Yi"  1024^8  n, // yobi
        ]
    });
    &*PREFIXES
}

/// # Standard suffixes for units
/// 
/// This function returns a static slice of string pairs.
/// 
/// Each pair contains a plural suffix and its equivalent singular form.
///
/// For example, `henries` is the plural of `henry` so the suffix `ies` maps to
/// `y`. `bytes` is the plural of `byte`, where the suffix `s` maps to nothing
/// (i.e. an empty string).
pub fn standard_suffixes() -> &'static [(&'static str, &'static str)] {
    &[("s", ""), ("es", ""), ("ies", "y")]
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Just test the first two prefixes to make sure the assignment works.
    #[test]
    fn std_prefixes() {
        assert!(standard_prefixes().contains(&Prefix::new("yotta", rat!(10).pow(24), true)));
        assert!(standard_prefixes().contains(&Prefix::new("k", rat!(1000), false)));
    }
}
