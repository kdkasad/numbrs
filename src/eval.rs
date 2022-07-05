//
// numbrs - A text-based calculator
// Copyright (C) 2022  Kian Kasad
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, version 3 of the License.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//

use crate::ast::{Node, Operation};
use crate::textio::{COLOR_RST, COLOR_WARN};
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
                    if name == "_" {
                        return Err(NodeEvalError::CantAssign(name.to_owned()));
                    }

                    if let Node::Variable(rname) = &**rhs {
                        if rname == "_" {
                            if env.remove(name).is_none() {
                                // I don't like printing from a library module but I don't see a
                                // better way to implement this:
                                eprintln!(
                                    "{}Warning:{} variable '{}' is not defined.",
                                    COLOR_WARN, COLOR_RST, name
                                );
                            }
                            // TODO: rework structure so control flow continues to the evaluation
                            // below
                            return rhs.eval(env);
                        }
                    }

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

    #[error("invalid LHS for assignment operator. Can only assign to variables.")]
    InvalidLHS, // TODO: ideally should store the invalid node

    #[error("undefined variable: '{0}'")]
    UndefinedVariable(String),

    #[error("can't assign special variable: '{0}'")]
    CantAssign(String),
}
