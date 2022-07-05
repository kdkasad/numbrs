use crate::ast::{Node, Operation};
use bigdecimal::BigDecimal;
use std::collections::HashMap;
use thiserror::Error;

pub trait Eval {
    fn eval(&self, env: &mut HashMap<String, BigDecimal>) -> Result<BigDecimal, NodeEvalError>;
}

impl Eval for Node {
    fn eval(&self, env: &mut HashMap<String, BigDecimal>) -> Result<BigDecimal, NodeEvalError> {
        match self {
            Node::Binary { .. } => self.eval_binary_node(env),
            Node::Unary { .. } => self.eval_unary_node(env),
            Node::Number(value) => Ok(value.to_owned()),
            Node::Variable(_) => self.eval_variable_node(env),
        }
    }
}

impl Node {
    fn eval_variable_node(
        &self,
        env: &mut HashMap<String, BigDecimal>,
    ) -> Result<BigDecimal, NodeEvalError> {
        if let Node::Variable(name) = self {
            match env.get(name) {
                Some(value) => Ok(value.to_owned()),
                None => Err(NodeEvalError::UndefinedVariable(name.to_owned())),
            }
        } else {
            // function will never be called on a non-variable node
            unreachable!()
        }
    }

    fn eval_binary_node(
        &self,
        env: &mut HashMap<String, BigDecimal>,
    ) -> Result<BigDecimal, NodeEvalError> {
        use Operation::*;

        if let Node::Binary {
            operation,
            lhs,
            rhs,
        } = self
        {
            if let Assign = operation {
                if let Node::Variable(name) = &**lhs {
                    let rval = rhs.eval(env)?;
                    env.insert(name.to_owned(), rval.to_owned());
                    return Ok(rval);
                } else {
                    return Err(NodeEvalError::InvalidLHS);
                }
            }

            let lhs = lhs.eval(env)?;
            let rhs = rhs.eval(env)?;
            Ok(match operation {
                Add => lhs + rhs,
                Subtract => lhs - rhs,
                Multiply => lhs * rhs,
                Divide => lhs / rhs,

                Assign => unreachable!(),

                UnaryAdd | UnarySubtract => {
                    return Err(NodeEvalError::InvalidNodeOperation(
                        *operation,
                        self.variant_name(),
                    ))
                }
            })
        } else {
            // function will never be called on non-binary node
            unreachable!()
        }
    }

    fn eval_unary_node(
        &self,
        env: &mut HashMap<String, BigDecimal>,
    ) -> Result<BigDecimal, NodeEvalError> {
        use Operation::*;

        if let Node::Unary { operation, expr } = self {
            let value = expr.eval(env)?;
            Ok(match operation {
                UnaryAdd => value,
                UnarySubtract => -value,
                Add | Subtract | Multiply | Divide | Assign => {
                    return Err(NodeEvalError::InvalidNodeOperation(
                        *operation,
                        self.variant_name(),
                    ))
                }
            })
        } else {
            // function will never be called on non-unary node
            unreachable!()
        }
    }
}

#[derive(Error, Debug)]
pub enum NodeEvalError {
    #[error("invalid operation '{0:?}' for {1}")]
    InvalidNodeOperation(Operation, &'static str),

    #[error("invalid LHS for assignment operator")]
    InvalidLHS, // TODO: ideally should store the invalid node

    #[error("undefined variable: '{0}'")]
    UndefinedVariable(String),
}
