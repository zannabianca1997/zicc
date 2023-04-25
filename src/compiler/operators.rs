//! Operators

use thiserror::Error;

use crate::compiler::types::Data;

use super::types::Type;

type TCResult<T> = std::result::Result<T, TypeCheckError>;

#[derive(Debug, Error)]
enum TypeCheckError {
    #[error("Cannot dereference element of type {0}")]
    CannotDereference(Type),
    #[error("Cannot negate element of type {0}")]
    CannotNegate(Type),
    #[error("Cannot sum elements of types {0} and {1}")]
    CannotSum(Type, Type),
    #[error("Cannot subtract elements of type {1} from type {0}")]
    CannotSub(Type, Type),
    #[error("Cannot multiply elements of types {0} and {1}")]
    CannotMul(Type, Type),
    #[error("Cannot assign element of type {1} to element of type {0}")]
    CannotAssign(Type, Type),
    #[error("Cannot order element of type {0} with element of type {1}")]
    CannotOrd(Type, Type),
    #[error("Cannot compare element of type {0} to element of type {1}")]
    CannotCmp(Type, Type),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum UnaryOp {
    Dereference,
    Reference,
    Minus,
}
impl UnaryOp {
    /// Check the result type of this operand applied to a given type
    fn type_check(&self, operand: Type) -> TCResult<Type> {
        use TypeCheckError::*;
        match self {
            UnaryOp::Dereference => {
                if let Type::Data(Data::Pointer(dest)) = operand {
                    Ok(*dest)
                } else {
                    Err(CannotDereference(operand))
                }
            }
            UnaryOp::Reference => Ok(Type::Data(Data::Pointer(Box::new(operand)))),
            UnaryOp::Minus => {
                // All the scalar types are signed
                if operand.is_scalar() {
                    Ok(operand)
                } else {
                    Err(CannotNegate(operand))
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum BinaryOp {
    Sum,
    Sub,
    Mul,
    Gt,
    Ge,
    Eq,
    Ne,
    Lt,
    Le,
    Assign,
}
impl BinaryOp {
    /// Check the result type of this operand applied to a given types
    fn type_check(&self, lhs: Type, rhs: Type) -> TCResult<Type> {
        use TypeCheckError::*;
        match self {
            BinaryOp::Sum => {
                if lhs.is_scalar() && rhs.is_scalar() {
                    Ok(Type::Data(Data::Scalar))
                } else {
                    Err(CannotSum(lhs, rhs))
                }
            }
            BinaryOp::Sub => {
                if lhs.is_scalar() && rhs.is_scalar() {
                    Ok(Type::Data(Data::Scalar))
                } else {
                    Err(CannotSub(lhs, rhs))
                }
            }
            BinaryOp::Mul => {
                if lhs.is_scalar() && rhs.is_scalar() {
                    Ok(Type::Data(Data::Scalar))
                } else {
                    Err(CannotMul(lhs, rhs))
                }
            }
            BinaryOp::Assign => match (lhs, rhs) {
                (Type::Data(lhs), Type::Data(rhs)) if lhs == rhs => Ok(Type::Data(rhs)),
                (lhs, rhs) => Err(CannotAssign(lhs, rhs)),
            },
            BinaryOp::Gt | BinaryOp::Ge | BinaryOp::Lt | BinaryOp::Le => {
                if lhs.is_scalar() && rhs.is_scalar() {
                    Ok(Type::Data(Data::Scalar))
                } else {
                    Err(CannotOrd(lhs, rhs))
                }
            }
            BinaryOp::Eq | BinaryOp::Ne => match (lhs, rhs) {
                (Type::Data(lhs), Type::Data(rhs)) if lhs == rhs => Ok(Type::Data(Data::Scalar)),
                (lhs, rhs) => Err(CannotCmp(lhs, rhs)),
            },
        }
    }
}
