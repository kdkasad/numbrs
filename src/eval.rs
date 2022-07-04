use crate::ast::{Node, Operation};
use decimal::d128;
use thiserror::Error;

pub trait Eval {
    fn eval(&self) -> Result<d128, NodeEvalError>;
}

impl Eval for Node {
    fn eval(&self) -> Result<d128, NodeEvalError> {
        match self {
            Node::Binary { .. } => self.eval_binary_node(),
            Node::Unary { .. } => self.eval_unary_node(),
            Node::Number(value) => Ok(*value),
        }
    }
}

impl Node {
    fn eval_binary_node(&self) -> Result<d128, NodeEvalError> {
        use Operation::*;

        if let Node::Binary {
            operation,
            lhs,
            rhs,
        } = self
        {
            let lhs = lhs.eval()?;
            let rhs = rhs.eval()?;
            Ok(match operation {
                Add => lhs + rhs,
                Subtract => lhs - rhs,
                Multiply => lhs * rhs,
                Divide => lhs / rhs,
                _ => return Err(NodeEvalError::InvalidNodeOperation(*operation, self)),
            })
        } else {
            unreachable!()
        }
    }

    fn eval_unary_node(&self) -> Result<d128, NodeEvalError> {
        use Operation::*;

        if let Node::Unary { operation, expr } = self {
            let value = expr.eval()?;
            Ok(match operation {
                UnaryAdd => value,
                UnarySubtract => -value,
                _ => return Err(NodeEvalError::InvalidNodeOperation(*operation, self)),
            })
        } else {
            unreachable!()
        }
    }
}

#[derive(Error, Debug)]
pub enum NodeEvalError<'a> {
    #[error("invalid operation '{0:?}' for node {1:?}")]
    InvalidNodeOperation(Operation, &'a Node),
}
