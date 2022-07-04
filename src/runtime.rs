use crate::eval::Eval;
use crate::parser::Parser;
use crate::{eval, parser};
use decimal::d128;
use std::collections::HashMap;
use std::error::Error;
use thiserror::Error;

pub struct Runtime {
    env: HashMap<String, d128>,
}

impl Runtime {
    pub fn new() -> Self {
        Self {
            env: HashMap::new(),
        }
    }

    pub fn evaluate(&mut self, input: &str) -> Result<d128, Box<dyn Error>> {
        let value = Parser::new(input).parse()?.eval(&mut self.env)?;
        Ok(value)
    }
}

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("parse error: {0}")]
    Parse(#[from] parser::Error),

    #[error("evaluation error: {0}")]
    Eval(#[from] eval::NodeEvalError),
}
