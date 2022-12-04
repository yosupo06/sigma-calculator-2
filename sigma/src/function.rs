//use crate::sets_intersection;

use std::ops::{Add, Deref, Mul, Sub};
use std::rc::Rc;

use num::{BigInt, BigRational, Zero};

use crate::constant::Type;
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

#[derive(Debug)]
pub enum FunctionData<'e> {
    // value
    Bool {
        f: bool,
    },
    PolynomialAsInt {
        // evaluated p isn't integer => undefined
        p: Polynomial<Variable<'e>, BigRational>,
    },
    // single op
    Neg {
        v: Function<'e>,
    },
    Not {
        v: Function<'e>,
    },
    // binary op
    Add {
        t: Type,
        l: Function<'e>,
        r: Function<'e>,
    },
    Mul {
        t: Type,
        l: Function<'e>,
        r: Function<'e>,
    },
    // condition
    IsNotNeg {
        // p >= 0
        p: LinearPolynomial<Variable<'e>, BigInt>,
    },
    IsDivisor {
        // l % r == 0
        l: LinearPolynomial<Variable<'e>, BigInt>,
        r: BigInt,
    },
    // flow
    If {
        t: Type,
        cond: Function<'e>,
        f: Function<'e>,
    },
    LoopSum {
        t: Type,
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
    pub fn return_type(&self) -> &Type {
        match self.data() {
            FunctionData::Bool { .. } => &Type::Bool,
            FunctionData::PolynomialAsInt { .. } => &Type::Integer,
            FunctionData::Neg { .. } => &Type::Integer,
            FunctionData::Not { .. } => &Type::Bool,
            FunctionData::Add { t, .. } => t,
            FunctionData::Mul { t, .. } => t,
            FunctionData::IsNotNeg { .. } => &Type::Bool,
            FunctionData::IsDivisor { .. } => &Type::Bool,
            FunctionData::If { t, .. } => t,
            FunctionData::LoopSum { t, .. } => t,
        }
    }
    pub fn data(&self) -> &FunctionData<'e> {
        &self.0
    }
}

impl<'e> Function<'e> {
    pub fn new_bool(f: bool) -> Self {
        Rc::new(FunctionData::Bool { f }).into()
    }
    pub fn new_polynomial_as_int(p: Polynomial<Variable<'e>, BigRational>) -> Self {
        Rc::new(FunctionData::PolynomialAsInt { p }).into()
    }
    pub fn new_is_not_neg(p: LinearPolynomial<Variable<'e>, BigInt>) -> Self {
        Rc::new(FunctionData::IsNotNeg { p }).into()
    }
    pub fn new_is_divisor(l: LinearPolynomial<Variable<'e>, BigInt>, r: BigInt) -> Self {
        assert_ne!(r, BigInt::zero());
        Rc::new(FunctionData::IsDivisor { l, r }).into()
    }
    pub fn new_add(l: Self, r: Self) -> Self {
        assert_eq!(l.return_type(), r.return_type());
        Rc::new(FunctionData::Add {
            t: l.return_type().clone(),
            l,
            r,
        })
        .into()
    }
    pub fn new_mul(l: Self, r: Self) -> Self {
        assert_eq!(l.return_type(), r.return_type());
        Rc::new(FunctionData::Mul {
            t: l.return_type().clone(),
            l,
            r,
        })
        .into()
    }
    pub fn new_if(cond: Self, f: Self) -> Self {
        assert_eq!(cond.return_type(), &Type::Bool);
        Rc::new(FunctionData::If {
            t: f.return_type().clone(),
            cond,
            f,
        })
        .into()
    }
    pub fn new_neg(v: Self) -> Self {
        assert_eq!(v.return_type(), &Type::Integer);
        Rc::new(FunctionData::Neg { v }).into()
    }
    pub fn new_not(v: Self) -> Self {
        assert_eq!(v.return_type(), &Type::Bool);
        Rc::new(FunctionData::Not { v }).into()
    }
    pub fn new_loop_sum(i: Variable<'e>, l: Self, r: Self, f: Self) -> Self {
        assert_eq!(l.return_type(), &Type::Integer);
        assert_eq!(r.return_type(), &Type::Integer);

        Rc::new(FunctionData::LoopSum {
            t: f.return_type().clone(),
            i,
            l,
            r,
            f,
        })
        .into()
    }

    fn to_source_lines(&self) -> Vec<String> {
        let shift = |v: Vec<String>| -> Vec<String> {
            v.into_iter().map(|x| "  ".to_string() + &x).collect()
        };
        match self.data() {
            FunctionData::Bool { f } => vec![format!("({})", f)],
            FunctionData::PolynomialAsInt { p } => vec![format!("({})", p)],
            FunctionData::Add { l, r, .. } => {
                let mut ls = l.to_source_lines();
                let mut rs = r.to_source_lines();

                if ls.len() == 1 && rs.len() == 1 {
                    vec![format!("({}) + ({})", ls[0], rs[0])]
                } else {
                    ls.push("+".to_string());
                    ls.append(&mut rs);
                    ls
                }
            }
            FunctionData::Mul { l, r, .. } => {
                vec![format!(
                    "({} * {})",
                    l.to_source_s_line(),
                    r.to_source_s_line()
                )]
            }
            FunctionData::LoopSum { i, l, r, f, .. } => {
                let mut v = vec![format!(
                    "$({}_{} = {} .. {})",
                    i.name(),
                    i.id,
                    l.to_source_s_line(),
                    r.to_source_s_line(),
                )];
                v.append(&mut shift(f.to_source_lines()));
                v
            }
            FunctionData::If { cond, f, .. } => {
                let mut v = vec![format!("[{}]", cond.to_source_s_line())];
                v.append(&mut shift(f.to_source_lines()));
                v
            }
            FunctionData::IsDivisor { l, r } => {
                vec![format!(
                    "({} % {} == 0)",
                    Polynomial::<Variable<'e>, BigInt>::from(l.clone()),
                    r,
                )]
            }
            FunctionData::IsNotNeg { p } => {
                vec![format!(
                    "({} >= 0)",
                    Polynomial::<Variable<'e>, BigInt>::from(p.clone())
                )]
            }
            FunctionData::Neg { v } => vec![format!("-({})", v.to_source_s_line())],
            FunctionData::Not { v } => vec![format!("!({})", v.to_source_s_line())],
        }
    }

    pub fn to_source(&self) -> String {
        self.to_source_lines().join("\n")
    }
    pub fn to_source_s_line(&self) -> String {
        self.to_source_lines().join(" ")
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
