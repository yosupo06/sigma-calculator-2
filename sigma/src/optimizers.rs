use num::{BigRational, One, Signed, Zero};

use crate::{
    //    constant::{Type},
    eval::quick_eval_constant,
    function::{Condition, Function, FunctionData, FunctionDeclare, IsDivisor, IsNotNeg},
};

use self::replace::replace_all;

pub mod loop_optimizer;
pub mod replace;

pub trait OptimizeRule<'e> {
    fn optimize(&mut self, f: &Function<'e>) -> Option<Function<'e>>;

    fn or<T: OptimizeRule<'e>>(self, other: T) -> Or<Self, T>
    where
        Self: Sized,
        T: Sized,
    {
        Or(self, other)
    }
}
impl<'e, T> OptimizeRule<'e> for T
where
    T: FnMut(&Function<'e>) -> Option<Function<'e>>,
{
    fn optimize(&mut self, f: &Function<'e>) -> Option<Function<'e>> {
        self(f)
    }
}

pub struct Or<P1, P2>(P1, P2);
impl<'e, P1, P2> OptimizeRule<'e> for Or<P1, P2>
where
    P1: OptimizeRule<'e>,
    P2: OptimizeRule<'e>,
{
    fn optimize(&mut self, f: &Function<'e>) -> Option<Function<'e>> {
        self.0.optimize(f).or_else(|| self.1.optimize(f))
    }
}

pub fn constant_optimize_rule<'e>() -> impl OptimizeRule<'e> {
    |f: &Function<'e>| {
        if matches!(f.data(), FunctionData::Polynomial { .. }) {
            return None;
        }
        quick_eval_constant(f).map(|x| Function::new_polynomial_as_int(BigRational::from(x).into()))
    }
}

pub fn binop_optimize_rule<'e>() -> impl OptimizeRule<'e> {
    |f: &Function<'e>| {
        if let FunctionData::Add { l, r, .. } = f.data() {
            // a + 0 = 0 + a = a
            if let FunctionData::Polynomial { p } = l.data() {
                if p.is_zero() {
                    return Some(r.clone());
                }
            }
            if let FunctionData::Polynomial { p } = r.data() {
                if p.is_zero() {
                    return Some(l.clone());
                }
            }
        }
        None
    }
}

pub fn polynomial_optimize_rule<'e>() -> impl OptimizeRule<'e> {
    |f: &Function<'e>| match f.data() {
        FunctionData::Add { l, r, .. } => {
            if let (FunctionData::Polynomial { p: lp }, FunctionData::Polynomial { p: rp }) =
                (l.data(), r.data())
            {
                Some(Function::new_polynomial_as_int(lp.clone() + rp.clone()))
            } else {
                None
            }
        }
        FunctionData::Mul { l, r, .. } => {
            if let (FunctionData::Polynomial { p: lp }, FunctionData::Polynomial { p: rp }) =
                (l.data(), r.data())
            {
                Some(Function::new_polynomial_as_int(lp.clone() * rp.clone()))
            } else {
                None
            }
        }
        FunctionData::Neg { v } => {
            if let FunctionData::Polynomial { p } = v.data() {
                Some(Function::new_polynomial_as_int(-p.clone()))
            } else {
                None
            }
        }
        _ => None,
    }
}

pub fn obvious_if_optimize_rule<'e>() -> impl OptimizeRule<'e> {
    |f: &Function<'e>| {
        let FunctionData::If { cond, f: cf, .. } = f.data() else {
            return None;
        };
        if let FunctionData::Polynomial { p } = cf.data() {
            // ([x]0) => 0
            if p.is_zero() {
                return Some(cf.clone());
            }
        }
        if let Condition::IsDivisor(IsDivisor { p, c }) = cond {
            // [x % 1 == 0] is always true
            if c.is_one() {
                return Some(cf.clone());
            }
            // [0 % c == 0] is always true
            if p.is_zero() {
                return Some(cf.clone());
            }
        }
        if let Condition::IsNotNeg(IsNotNeg { p }) = cond {
            if let Some(c) = p.to_constant() {
                if c.is_negative() {
                    // [neg >= 0] is always false
                    return Some(Function::new_polynomial_as_int(BigRational::zero().into()));
                } else {
                    // [(not neg) >= 0] is always true
                    return Some(cf.clone());
                }
            }
        }
        None
    }
}

pub fn fully_optimize<'e>(
    mut f: FunctionDeclare<'e>,
    mut rule: impl OptimizeRule<'e>,
) -> FunctionDeclare<'e> {
    f.body = replace_all(&f.body, |f2| rule.optimize(f2)).unwrap_or(f.body);
    f
}
