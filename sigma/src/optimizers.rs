use num::{BigRational, Zero};

use crate::{
    constant::{Constant, Type},
    eval::quick_eval_constant,
    function::{Function, FunctionData},
};

use self::replace::replace_all;

pub mod loop_optimizer;
pub mod replace;

pub trait Optimizer<'e> {
    fn optimize(&self, f: &Function<'e>) -> Option<Function<'e>>;
}

pub struct ConstantOptimizer {}
impl<'e> Optimizer<'e> for ConstantOptimizer {
    fn optimize(&self, f: &Function<'e>) -> Option<Function<'e>> {
        if matches!(f.data(), FunctionData::Bool { .. }) {
            return None;
        }
        if matches!(f.data(), FunctionData::PolynomialAsInt { .. }) {
            return None;
        }
        if let Some(x) = quick_eval_constant(f) {
            match x {
                Constant::Integer(x) => {
                    Some(Function::new_polynomial_as_int(BigRational::from(x).into()))
                }
                Constant::Bool(x) => Some(Function::new_bool(x)),
            }
        } else {
            None
        }
    }
}

pub struct ObviousBinOpOptimizer {}
impl<'e> Optimizer<'e> for ObviousBinOpOptimizer {
    fn optimize(&self, f: &Function<'e>) -> Option<Function<'e>> {
        if let FunctionData::Add { l, r, .. } = f.data() {
            if f.return_type() == &Type::Integer {
                // a + 0 = 0 + a = a
                if let FunctionData::PolynomialAsInt { p } = l.data() {
                    if p.is_zero() {
                        return Some(r.clone());
                    }
                }
                if let FunctionData::PolynomialAsInt { p } = r.data() {
                    if p.is_zero() {
                        return Some(l.clone());
                    }
                }
            }
        }
        None
    }
}

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
}
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
pub struct PolynomialOptimizer {}
impl<'e> Optimizer<'e> for PolynomialOptimizer {
    fn optimize(&self, f: &Function<'e>) -> Option<Function<'e>> {
        match f.data() {
            FunctionData::Add { l, r, .. } => {
                if let (
                    FunctionData::PolynomialAsInt { p: lp },
                    FunctionData::PolynomialAsInt { p: rp },
                ) = (l.data(), r.data())
                {
                    Some(Function::new_polynomial_as_int(lp.clone() + rp.clone()))
                } else {
                    None
                }
            }
            FunctionData::Mul { l, r, .. } => {
                if let (
                    FunctionData::PolynomialAsInt { p: lp },
                    FunctionData::PolynomialAsInt { p: rp },
                ) = (l.data(), r.data())
                {
                    Some(Function::new_polynomial_as_int(lp.clone() * rp.clone()))
                } else {
                    None
                }
            }
            FunctionData::Neg { v } => {
                if let FunctionData::PolynomialAsInt { p } = v.data() {
                    Some(Function::new_polynomial_as_int(-p.clone()))
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

pub struct ChainedOptimizer<'e> {
    pub optimizers: Vec<Box<dyn Optimizer<'e>>>,
}
impl<'e> Optimizer<'e> for ChainedOptimizer<'e> {
    fn optimize(&self, f: &Function<'e>) -> Option<Function<'e>> {
        for optimizer in &self.optimizers {
            if let Some(f) = optimizer.optimize(f) {
                return Some(f);
            }
        }
        None
    }
}

pub struct FullyOptimizer<T> {
    pub optimizer: T,
}
impl<'e, T> Optimizer<'e> for FullyOptimizer<T>
where
    T: Optimizer<'e>,
{
    fn optimize(&self, f: &Function<'e>) -> Option<Function<'e>> {
        replace_all(f, |f2| self.optimizer.optimize(f2))
    }
}
