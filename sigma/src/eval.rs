//use crate::sets_intersection;

use std::collections::HashMap;

use num::{BigInt, BigRational, One, Signed, Zero};

use crate::constant::Constant;
use crate::function::{Function, FunctionData, FunctionDeclare};

use crate::variable::Variable;

pub fn quick_eval_constant(f: &Function) -> Option<Constant> {
    match f.data() {
        FunctionData::PolynomialAsInt { p } => {
            if let Some(x) = p.to_constant() {
                if x.is_integer() {
                    Some(Constant::Integer(x.to_integer()))
                } else {
                    // TODO: ideally, undefined(=wirdcard)
                    Some(Constant::Integer(BigInt::zero()))
                }
            } else {
                None
            }
        }
        FunctionData::Add { l, r, .. } => {
            if let (Some(l), Some(r)) = (quick_eval_constant(l), quick_eval_constant(r)) {
                Some((l + r).unwrap())
            } else {
                None
            }
        }
        FunctionData::IsNotNeg { p } => p.to_constant().map(|x| Constant::Bool(!x.is_negative())),
        FunctionData::IsDivisor { l, r } => {
            l.to_constant().map(|x| Constant::Bool((x % r).is_zero()))
        }

        _ => None,
    }
}

pub fn eval<'e>(f: &Function<'e>, vals: &HashMap<Variable<'e>, BigInt>) -> Constant {
    match f.data() {
        FunctionData::PolynomialAsInt { p } => {
            let x: BigRational = p
                .iter()
                .map(|(m, c)| -> BigRational {
                    m.iter()
                        .map(|(v, r)| BigRational::from(vals.get(v).unwrap().pow(*r as u32)))
                        .fold(c.clone(), |acc, x| acc * x)
                })
                .sum();

            if x.is_integer() {
                Constant::Integer(x.to_integer())
            } else {
                // wirdcard
                Constant::Integer(BigInt::from(123))
            }
        }
        FunctionData::Add { l, r, .. } => (eval(l, vals) + eval(r, vals)).unwrap(),
        FunctionData::Mul { l, r, .. } => (eval(l, vals) * eval(r, vals)).unwrap(),
        FunctionData::Neg { v } => {
            let v = eval(v, vals);

            match v {
                Constant::Integer(v) => Constant::Integer(-v),
                Constant::Bool(v) => Constant::Bool(!v),
            }
        }
        FunctionData::If { cond, f, .. } => {
            let x = eval(cond, vals);
            if let Constant::Bool(x) = x {
                if x {
                    eval(f, vals)
                } else {
                    Constant::zero(f.return_type())
                }
            } else {
                unreachable!();
            }
        }
        FunctionData::LoopSum { i, l, r, f, .. } => {
            let l = eval(l, vals);
            let r = eval(r, vals);
            if let (Constant::Integer(l), Constant::Integer(r)) = (l, r) {
                let mut sum = Constant::zero(f.return_type());
                let mut j = l;
                let mut vals = vals.clone();
                while j <= r {
                    vals.insert(i.clone(), j.clone());
                    sum = (sum + eval(f, &vals)).unwrap();
                    j += BigInt::one();
                }
                sum
            } else {
                todo!()
            }
        }
        FunctionData::IsNotNeg { p } => Constant::Bool(!p.eval(vals).unwrap().is_negative()),
        FunctionData::IsDivisor { l, r } => Constant::Bool((l.eval(vals).unwrap() % r).is_zero()),
        _ => {
            println!("error: {}", f.to_source());
            todo!()
        }
    }
}

pub fn eval_function<'e>(f: &FunctionDeclare<'e>, vals: &Vec<BigInt>) -> Constant {
    assert_eq!(vals.len(), f.args.len());
    let map = HashMap::from_iter(
        f.args
            .iter()
            .zip(vals.iter())
            .map(|(x, y)| (x.clone(), y.clone())),
    );
    eval(&f.body, &map)
}
