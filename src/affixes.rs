use std::collections::HashMap;

use num::BigRational;

use crate::{rat_util_macros::rat, ast::Value, unit::Unit};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Prefix {
    text: String,
    scale: BigRational,
    standalone: bool,
}

impl Prefix {
    /// Create a new prefix.
    pub fn new<T: ToString>(text: T, scale: BigRational, standalone: bool) -> Self {
        Self {
            text: text.to_string(),
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

    /// Get the prefix's standalone status.
    pub fn standalone(&self) -> bool {
        self.standalone
    }
}

/// Look up a unit in the environment with prefix and suffix substitution.
pub fn resolve_unit(name: &str, env: &HashMap<String, Value>) -> Option<Unit> {
    if let Some(unit) = try_get_unit(name, env) {
        return Some(unit);
    }

    for suffix in (*SUFFIX_MAP).keys() {
        let sub: &str = (*SUFFIX_MAP).get(suffix).unwrap();
        if let Some(part) = name.strip_suffix(suffix) {
            let mut new_name = String::with_capacity(name.len());
            new_name.push_str(part);
            new_name.push_str(sub);
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
        return Some(units.collapse_to(name.to_string()));
    }

    // Search for existing unit by stripping available prefixes
    for prefix in (*SI_PREFIXES).iter() {
        if let Some(s) = name.strip_prefix(prefix.text()) {
            if let Some(Value::Unit(units)) = env.get(s) {
                if (*units).len() > 1 || (*units)[0].offset != rat!(0) {
                    return None;
                }
                // If a unit definition is found, create a new Unit with the
                // prefixed name and adjusted scale.
                let mut new = units.collapse_to(name.to_string());
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
        (*SI_PREFIXES)
            .iter()
            .find(|prefix| prefix.standalone() && prefix.text() == name)?
            .scale()
            .clone(),
    )
}


macro_rules! prefixes {
    ( $( $name:literal $scale:literal $standalone:tt ),* $(,)? ) => {
        vec![ $(
            Prefix::new(
                $name,
                rat!(10).pow($scale),
                stringify!($standalone) == "y"
            ),
        )* ]
    };
}

lazy_static! {
    /// Standard prefixes for the International System of units.
    /// Retrieved from <https://frinklang.org/frinkdata/units.txt>.
    pub static ref SI_PREFIXES: Vec<Prefix> = prefixes![
        "yotta"  24  y,
        "zetta"  21  y,
        "exa"    18  y,
        "peta"   15  y,
        "tera"   12  y,
        "giga"    9  y,
        "mega"    6  y,
        "myria"   4  y,
        "kilo"    3  y,
        "hecto"   2  y,
        "deca"    1  y,
        "deka"    1  y,
        "deci"   -1  y,
        "centi"  -2  y,
        "milli"  -3  y,
        "micro"  -6  y,
        "nano"   -9  y,
        "pico"  -12  y,
        "femto" -15  y,
        "atto"  -18  y,
        "zepto" -21  y,
        "yocto" -24  y,

        "Y"  24   n, // yotta
        "Z"  21   n, // zetta
        "E"  18   n, // exa
        "P"  15   n, // peta
        "T"  12   n, // tera
        "G"   9   n, // giga
        "M"   6   n, // mega
        "k"   3   n, // kilo
        "h"   2   n, // hecto
        "da"  1   n, // deka
        "d"  -1   n, // deci
        "c"  -2   n, // centi
        "m"  -3   n, // milli
        "µ"  -6   n, // micro
        "u"  -6   n, // micro (ASCII alternative)
        "n"  -9   n, // nano
        "p"  -12  n, // pico
        "f"  -15  n, // femto
        "a"  -18  n, // atto
        "z"  -21  n, // zepto
        "y"  -24  n, // yocto
    ];

    /// List of suffixes which map to singular suffix translation.
    /// 
    /// For example, "henries" is the plural of "henry" so the suffix "ies" maps to "y".
    /// In contrast, ""
    pub static ref SUFFIX_MAP: HashMap<String, String> = {
        let mut map: HashMap<String, String> = HashMap::new();
        map.insert("s".to_string(), "".to_string());
        map.insert("es".to_string(), "".to_string());
        map.insert("ies".to_string(), "y".to_string());
        map
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Just test the first two prefixes to make sure the assignment works.
    #[test]
    fn std_prefixes() {
        assert!(SI_PREFIXES.contains(&Prefix::new("yotta", rat!(10).pow(24), true)));
        assert!(SI_PREFIXES.contains(&Prefix::new("k", rat!(1000), false)));
    }
}
