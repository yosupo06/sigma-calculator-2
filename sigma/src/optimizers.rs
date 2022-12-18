use num::{BigRational, Zero};

use crate::{
    //    constant::{Type},
    eval::quick_eval_constant,
    function::{Function, FunctionData, FunctionDeclare},
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
        self.0.optimize(f).or(self.1.optimize(f))
    }
}

pub fn constant_optimize_rule<'e>() -> impl OptimizeRule<'e> {
    |f: &Function<'e>| {
        if matches!(f.data(), FunctionData::Polynomial { .. }) {
            return None;
        }
        if let Some(x) = quick_eval_constant(f) {
            Some(Function::new_polynomial_as_int(BigRational::from(x).into()))
        } else {
            None
        }
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

/*
pub struct ObviousIfOptimizer {}
impl<'e> Optimizer<'e> for ObviousIfOptimizer {
    fn optimize(&self, f: &Function<'e>) -> Option<Function<'e>> {
        if let FunctionData::If { cond, f: cf, .. } = f.data() {
            if let FunctionData::Bool { f: cond_f } = cond.data() {
                if *cond_f {
                    return Some(cf.clone());
                } else {
                    match cf.return_type() {
                        Type::Integer => {
                            return Some(Function::new_polynomial_as_int(Default::default()))
                        }
                        _ => unimplemented!(),
                    }
                }
            }

            if let Some(x) = quick_eval_constant(cf) {
                // if [xxx] 0 => 0
                if x == Constant::zero(cf.return_type()) {
                    return Some(cf.clone());
                }
            }
        }
        None
    }
}*/
/*
pub struct SimplifyConditionOptimizer {}
impl<'e> Optimizer<'e> for SimplifyConditionOptimizer {
    fn optimize(&self, f: &Function<'e>) -> Option<Function<'e>> {
        if let FunctionData::IsDivisor { l, r } = f.data() {
            if r.is_one() {
                return Some(Function::new_bool(true));
            }
            if l.is_zero() {
                return Some(Function::new_bool(true));
            }
            let p2 = LinearPolynomial::from_iter(l.iter().map(|(m, c)| {
                if c.is_integer() {
                    (m.clone(), BigRational::from((c.to_integer() % r + r) % r))
                } else {
                    (m.clone(), c.clone())
                }
            }));

            if l != &p2 {
                return Some(Function::new_is_divisor(p2, r.clone()));
            }

            let g = l.numer_gcd();
            if g != BigInt::one() {
                return Some(Function::new_is_divisor(
                    (l.clone() / BigRational::from(g.clone())).into(),
                    r.clone() / gcd(r.clone(), g),
                ));
            }
        }
        if let FunctionData::IsNotNeg { p } = f.data() {
            if p.is_zero() {
                return Some(Function::new_bool(true));
            }
            let g = p.numer_gcd();
            if g != BigInt::one() {
                return Some(Function::new_is_not_neg(
                    p.clone() / BigRational::from(g.clone()),
                ));
            }
        }
        None
    }
}
*/

pub fn fully_optimize<'e>(
    mut f: FunctionDeclare<'e>,
    mut rule: impl OptimizeRule<'e>,
) -> FunctionDeclare<'e> {
    f.body = replace_all(&f.body, |f2| rule.optimize(f2)).unwrap_or(f.body);
    f
}
