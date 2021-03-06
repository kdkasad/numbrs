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
use crate::bigdecimal::ToPrimitive;
use crate::bigdecimal_helper::BigDecimalPowExt;
use crate::textio::{COLOR_RST, COLOR_WARN};
use bigdecimal::BigDecimal;
use std::collections::HashMap;
use thiserror::Error;

const PROTECTED_VARS: [&str; 1] = ["_"]; // list of variables names that can't be assigned to
pub const UNASSIGN_IDENT: &str = "_"; // identifier used to unassign/clear variables

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
                    for pvar in PROTECTED_VARS {
                        if name == pvar {
                            return Err(NodeEvalError::CantAssign(name.to_owned()));
                        }
                    }

                    if let Node::Variable(rname) = &**rhs {
                        if rname == UNASSIGN_IDENT {
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

                Exponent => {
                    if !rhs.is_integer() {
                        Err(NodeEvalError::IntegerOnlyOperation(*operation))?
                    } else {
                        let exp = match rhs.to_u32() {
                            Some(val) => val,
                            None => return Err(NodeEvalError::ParseUInt(rhs)),
                        };
                        lhs.pow(exp)
                    }
                }

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
                Add | Subtract | Multiply | Divide | Exponent | Assign => {
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

    #[error("{0:?} ({0}) operation is only supported for integers.")]
    IntegerOnlyOperation(Operation),

    #[error("failed to parse value as unsigned integer: {0}.")]
    ParseUInt(BigDecimal),
}

#[cfg(test)]
mod tests {
    extern crate rand;
    use super::*;
    use bigdecimal::num_bigint::RandBigInt;
    use pretty_assertions::{assert_eq, assert_ne};
    use rand::Rng;

    #[test]
    fn node_operation_validation() {
        macro_rules! case {
            ( $a:ident, $b:ident, $c:expr ) => {
                (
                    if stringify!($a) == "Unary" {
                        Node::Unary {
                            operation: Operation::$b,
                            expr: Box::new(Node::Number(bigdec!(1))),
                        }
                    } else if stringify!($a) == "Binary" {
                        Node::Binary {
                            operation: Operation::$b,
                            lhs: Box::new(Node::Number(bigdec!(1))),
                            rhs: Box::new(Node::Number(bigdec!(1))),
                        }
                    } else {
                        panic!("Invalid argument to case!(..) macro");
                    },
                    $c,
                )
            };
        }
        let cases = [
            // (node type, operation, should succeed)
            case!(Binary, Add, true),
            case!(Binary, Subtract, true),
            case!(Binary, Divide, true),
            case!(Binary, Multiply, true),
            case!(Binary, Assign, true),
            case!(Unary, Add, false),
            case!(Unary, Subtract, false),
            case!(Unary, Divide, false),
            case!(Unary, Multiply, false),
            case!(Unary, Assign, false),
            case!(Binary, UnaryAdd, false),
            case!(Binary, UnarySubtract, false),
            case!(Unary, UnaryAdd, true),
            case!(Unary, UnarySubtract, true),
        ];
        let mut env = HashMap::new();
        for (node, should_succeed) in cases {
            let res = node.eval(&mut env);
            let failed = matches!(res, Err(NodeEvalError::InvalidNodeOperation(..)));
            dbg!(node, should_succeed);
            assert_ne!(should_succeed, failed);
        }
    }

    #[test]
    fn test_assignment() {
        const VARNAME: &str = "test";
        let mut env = HashMap::new();
        assert!(matches!(env.get(VARNAME), None));
        let value = BigDecimal::new(rand::thread_rng().gen_bigint(5), rand::thread_rng().gen());
        Node::Binary {
            operation: Operation::Assign,
            lhs: Box::new(Node::Variable(VARNAME.to_string())),
            rhs: Box::new(Node::Number(value.to_owned())),
        }
        .eval(&mut env)
        .unwrap();
        assert_eq!(*env.get(VARNAME).unwrap(), value);
    }

    #[test]
    fn eval_binary() {
        let node = Node::Binary {
            operation: Operation::Add,
            lhs: Box::new(Node::Number(bigdec!(1))),
            rhs: Box::new(Node::Number(bigdec!(1))),
        };
        let mut env = HashMap::new();
        assert_eq!(node.eval(&mut env).unwrap(), bigdec!(2));
    }

    #[test]
    fn eval_recursion() {
        let node = Node::Binary {
            operation: Operation::Add,
            lhs: Box::new(Node::Number(bigdec!(1))),
            rhs: Box::new(Node::Binary {
                operation: Operation::Add,
                lhs: Box::new(Node::Number(bigdec!(1))),
                rhs: Box::new(Node::Number(bigdec!(1))),
            }),
        };
        let mut env = HashMap::new();
        assert_eq!(node.eval(&mut env).unwrap(), bigdec!(3));
    }

    #[test]
    fn eval_unary() {
        let node = Node::Unary {
            operation: Operation::UnarySubtract,
            expr: Box::new(Node::Number(bigdec!(0519))),
        };
        let mut env = HashMap::new();
        assert_eq!(node.eval(&mut env).unwrap(), bigdec!(-519));
    }

    #[test]
    fn eval_variable() {
        let mut env = HashMap::from([("foo".to_string(), bigdec!(2980))]);
        let node = Node::Variable("foo".to_string());
        assert_eq!(node.eval(&mut env).unwrap(), bigdec!(2980));
    }

    #[test]
    fn assign_protected() {
        for var in PROTECTED_VARS {
            let mut env = HashMap::from([(var.to_string(), bigdec!(0))]);
            let node = Node::Binary {
                operation: Operation::Assign,
                lhs: Box::new(Node::Variable(var.to_string())),
                rhs: Box::new(Node::Number(bigdec!(123))),
            };
            match node.eval(&mut env).unwrap_err() {
                NodeEvalError::CantAssign(name) => assert_eq!(name, var),
                other => panic!("expected NodeEvalError::CantAssign(..), got {:?}", other),
            }
        }
    }
}
