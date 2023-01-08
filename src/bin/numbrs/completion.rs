use std::{cell::RefCell, rc::Rc};

use numbrs::{affixes::Prefix, ast::Value, runtime::Runtime};
use rustyline::{
    completion::{Completer, Pair},
    highlight::Highlighter,
    hint::Hinter,
    validate::Validator,
    Context, Helper, Result,
};

pub struct IdentCompleter<'a> {
    runtime: Rc<RefCell<Runtime>>,
    prefixes: &'a [Prefix],
}

impl<'a> IdentCompleter<'a> {
    pub fn new(runtime: Rc<RefCell<Runtime>>, prefixes: &'a [Prefix]) -> Self {
        Self { runtime, prefixes }
    }
}

impl<'a> Completer for IdentCompleter<'a> {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &Context<'_>,
    ) -> Result<(usize, Vec<Self::Candidate>)> {
        if let Some(c) = line.chars().nth(pos) {
            if !c.is_whitespace() {
                // Don't return suggestions if cursor is in the middle of a word
                return Ok((0, Vec::new()));
            }
        }

        let word_start = find_word_start(line, pos);
        let mut matches = Vec::new();

        if word_start == pos {
            // Cursor in whitespace

            // Add all environment entries (variables and units)
            matches.extend(
                self.runtime
                    .borrow()
                    .env
                    .iter()
                    .map(|(name, value)| create_pair_from_value(name, value)),
            );

            // Add all standalone prefixes
            matches.extend(
                self.prefixes
                    .iter()
                    .filter(|&prefix| prefix.standalone())
                    .map(|prefix| create_pair_from_prefix(prefix.text())),
            );
        } else {
            // Cursor in partial identifier
            let part = &line[word_start..pos];

            // Add all directly-matching identifiers
            matches.extend(
                self.runtime
                    .borrow()
                    .env
                    .iter()
                    .filter(|(name, _)| name.starts_with(part))
                    .map(|(name, value)| create_pair_from_value(name, value)),
            );

            // Add all directly-matching standalone prefixes
            matches.extend(
                self.prefixes
                    .iter()
                    .filter(|prefix| prefix.standalone())
                    .filter(|prefix| prefix.text().starts_with(part))
                    .map(|prefix| create_pair_from_prefix(prefix.text())),
            );

            // Try to look up with prefix removal
            for prefix in self.prefixes {
                if let Some(rest) = part.strip_prefix(prefix.text()) {
                    if rest.is_empty() {
                        continue;
                    }
                    matches.extend(
                        self.runtime
                            .borrow()
                            .env
                            .iter()
                            .filter(|(_, value)| matches!(value, Value::Unit(_)))
                            .filter(|(name, _)| name.starts_with(rest))
                            .map(|(name, _)| {
                                let replacement = [prefix.text(), name].concat();
                                Pair {
                                    display: format!("{} (unit)", replacement),
                                    replacement,
                                }
                            }),
                    );
                }
            }
        };

        matches.sort_unstable_by(|this, next| this.display.cmp(&next.display));
        Ok((word_start, matches))
    }
}

impl Highlighter for IdentCompleter<'_> {}
impl Hinter for IdentCompleter<'_> {
    type Hint = String;
}
impl Validator for IdentCompleter<'_> {}
impl Helper for IdentCompleter<'_> {}

fn create_pair_from_prefix(name: &str) -> Pair {
    Pair {
        replacement: name.to_owned(),
        display: format!("{} (prefix)", name),
    }
}

fn create_pair_from_value(name: &str, value: &Value) -> Pair {
    Pair {
        replacement: name.to_owned(),
        display: format!(
            "{} ({})",
            name,
            match value {
                Value::Unit(_) => "unit",
                Value::Number(_) | Value::Quantity(_) => "value",
            }
        ),
    }
}

#[inline]
fn is_ident_char(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn find_word_start(line: &str, pos: usize) -> usize {
    line[..pos]
        .rfind(|c| !is_ident_char(c))
        .map(|start| start + 1)
        .unwrap_or(0)
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    #[test]
    fn find_word_start() {
        let cases = [("abc", 0), ("123 abc", 4), ("2kg", 1), ("17.8_meters", 4)];

        for (line, expected_result) in cases {
            let start = super::find_word_start(line, line.chars().count());
            assert_eq!(
                expected_result, start,
                "Finding start of word for '{}'. Expected {}, got {}.",
                line, expected_result, start
            );
        }
    }
}
