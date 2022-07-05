use crate::eval::Eval;
use crate::parser::Parser;
use crate::{eval, parser};
use bigdecimal::BigDecimal;
use std::collections::HashMap;
use std::error::Error;
use thiserror::Error;

pub struct Runtime {
    env: HashMap<String, BigDecimal>,
}

impl Runtime {
    pub fn new() -> Self {
        let mut env = HashMap::new();
        env.insert("_".to_string(), bigdec!(0));
        Self { env }
    }

    pub fn evaluate(&mut self, input: &str) -> Result<BigDecimal, Box<dyn Error>> {
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
