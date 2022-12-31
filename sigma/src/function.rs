use std::ops::{Add, Deref, Mul, Sub};
use std::rc::Rc;

use num::integer::gcd;
use num::{zero, BigInt, BigRational, One, Zero};

use crate::polynomials::linear_polynomial::LinearPolynomial;
use crate::polynomials::polynomial::Polynomial;
use crate::variable::Variable;

#[derive(Clone, Debug)]
pub struct FunctionDeclare<'e> {
    pub name: String,
    pub args: Vec<Variable<'e>>,
    pub body: Function<'e>,
}

#[derive(Clone, Debug)]
pub struct Function<'e>(Rc<FunctionData<'e>>);

impl<'e> LinearPolynomial<Variable<'e>, BigInt> {
    pub fn coefficient_gcd(&self) -> BigInt {
        self.iter().fold(BigInt::zero(), |x, y| gcd(x, y.1.clone()))
    }
}

#[derive(Debug, Clone)]
pub struct IsNotNeg<'e> {
    pub p: LinearPolynomial<Variable<'e>, BigInt>,
}
impl<'e> IsNotNeg<'e> {
    pub fn new(p: LinearPolynomial<Variable<'e>, BigInt>) -> Self {
        if p.is_zero() {
            return Self { p: zero() };
        }

        let g = p.coefficient_gcd();

        Self {
            p: LinearPolynomial::from_iter(p.into_iter().map(|(m, c)| (m, c / g.clone()))),
        }
    }

    // x <- f / denom
    pub fn composite(
        &self,
        x: &Variable<'e>,
        f: &LinearPolynomial<Variable<'e>, BigInt>,
        denom: &BigInt,
    ) -> Self {
        Self::new((self.p.clone() * denom.clone()).composite(x, f))
    }
}

#[derive(Debug, Clone)]
pub struct IsDivisor<'e> {
    pub p: LinearPolynomial<Variable<'e>, BigInt>,
    pub c: BigInt,
}
impl<'e> IsDivisor<'e> {
    pub fn new(p: LinearPolynomial<Variable<'e>, BigInt>, c: BigInt) -> Self {
        assert_ne!(c, BigInt::zero());

        if p.is_zero() {
            return IsDivisor {
                p: Default::default(),
                c: BigInt::one(),
            };
        }
        let g = p.coefficient_gcd();

        Self {
            p: LinearPolynomial::from_iter(p.into_iter().map(|(m, c)| (m, c / g.clone()))),
            c: c.clone() / gcd(c, g),
        }
    }

    // x <- f / denom
    pub fn composite(
        &self,
        x: &Variable<'e>,
        f: &LinearPolynomial<Variable<'e>, BigInt>,
        denom: &BigInt,
    ) -> Self {
        Self::new(
            (self.p.clone() * denom.clone()).composite(x, f),
            self.c.clone() * denom.clone(),
        )
    }
}

#[derive(Debug, Clone)]
pub enum Condition<'e> {
    IsNotNeg(IsNotNeg<'e>),
    IsDivisor(IsDivisor<'e>),
}

#[derive(Debug)]
pub enum FunctionData<'e> {
    // value
    Polynomial {
        // evaluated p must be integer
        p: Polynomial<Variable<'e>, BigRational>,
    },
    // single op
    Neg {
        v: Function<'e>,
    },
    // binary op
    Add {
        l: Function<'e>,
        r: Function<'e>,
    },
    Mul {
        l: Function<'e>,
        r: Function<'e>,
    },
    // flow
    If {
        cond: Condition<'e>,
        f: Function<'e>,
    },
    LoopSum {
        i: Variable<'e>,
        l: Function<'e>,
        r: Function<'e>,
        f: Function<'e>,
    },
}

impl<'e> Deref for Function<'e> {
    type Target = Rc<FunctionData<'e>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<'e> From<Rc<FunctionData<'e>>> for Function<'e> {
    fn from(item: Rc<FunctionData<'e>>) -> Self {
        Function(item)
    }
}
impl<'e> Function<'e> {
    pub fn data(&self) -> &FunctionData<'e> {
        &self.0
    }
}

impl<'e> Function<'e> {
    pub fn new_polynomial_as_int(p: Polynomial<Variable<'e>, BigRational>) -> Self {
        Rc::new(FunctionData::Polynomial { p }).into()
    }
    pub fn new_add(l: Self, r: Self) -> Self {
        Rc::new(FunctionData::Add { l, r }).into()
    }
    pub fn new_mul(l: Self, r: Self) -> Self {
        Rc::new(FunctionData::Mul { l, r }).into()
    }
    pub fn new_if(cond: Condition<'e>, f: Self) -> Self {
        Rc::new(FunctionData::If { cond, f }).into()
    }
    pub fn new_neg(v: Self) -> Self {
        Rc::new(FunctionData::Neg { v }).into()
    }
    pub fn new_loop_sum(i: Variable<'e>, l: Self, r: Self, f: Self) -> Self {
        Rc::new(FunctionData::LoopSum { i, l, r, f }).into()
    }
}

impl<'e> Add for Function<'e> {
    type Output = Option<Self>;
    fn add(self, other: Self) -> Self::Output {
        Some(Function::new_add(self, other))
    }
}
impl<'e> Sub for Function<'e> {
    type Output = Option<Self>;
    fn sub(self, other: Self) -> Self::Output {
        Some(Function::new_add(self, Function::new_neg(other)))
    }
}
impl<'e> Mul for Function<'e> {
    type Output = Option<Self>;
    fn mul(self, other: Self) -> Self::Output {
        Some(Function::new_mul(self, other))
    }
}

impl<'e> Function<'e> {
    pub fn to_linear_polynomial(&self) -> Option<LinearPolynomial<Variable<'e>, BigInt>> {
        let FunctionData::Polynomial { p } = self.data() else {
            return None;
        };
        let Some(p) = p.to_linear_polynomial() else {
            return None;
        };
        if p.iter().any(|(_, c)| !c.is_integer()) {
            return None;
        }
        Some(LinearPolynomial::from_iter(
            p.into_iter().map(|(p, c)| (p, c.to_integer())),
        ))
    }
}
