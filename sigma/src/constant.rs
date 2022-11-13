use std::ops::{Add, Mul};

use num::{BigInt, Zero};

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum Type {
    Integer,
    Bool,
}

impl Type {
    pub fn to_token(&self) -> String {
        match &self {
            Type::Integer => "integer".to_string(),
            Type::Bool => "bool".to_string(),
        }
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum Constant {
    Integer(BigInt),
    Bool(bool),
}
impl Constant {
    pub fn to_source(&self) -> String {
        match &self {
            Constant::Integer(v) => format!("{}", v),
            Constant::Bool(f) => format!("{}", f),
        }
    }
    pub fn get_type(&self) -> Type {
        match self {
            Self::Integer(_) => Type::Integer,
            Self::Bool(_) => Type::Bool,
        }
    }

    pub fn zero(t: &Type) -> Self {
        match t {
            Type::Integer => Constant::Integer(BigInt::zero()),
            _ => unreachable!(),
        }
    }
}

impl Add for Constant {
    type Output = Option<Self>;
    fn add(self, other: Self) -> Self::Output {
        if let (Constant::Integer(l), Constant::Integer(r)) = (&self, &other) {
            return Some(Constant::Integer(l + r));
        }
        None
    }
}
impl Mul for Constant {
    type Output = Option<Self>;
    fn mul(self, other: Self) -> Self::Output {
        if let (Constant::Integer(l), Constant::Integer(r)) = (&self, &other) {
            return Some(Constant::Integer(l * r));
        }
        None
    }
}
