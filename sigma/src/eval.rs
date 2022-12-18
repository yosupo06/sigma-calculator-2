use std::collections::HashMap;

use num::{BigInt, BigRational, One, Signed, Zero};

use crate::function::{Condition, Function, FunctionData, FunctionDeclare, IsDivisor, IsNotNeg};

use crate::variable::Variable;

pub fn quick_eval_constant(f: &Function) -> Option<BigInt> {
    match f.data() {
        FunctionData::Polynomial { p } => {
            if let Some(x) = p.to_constant() {
                assert!(x.is_integer());
                Some(x.to_integer())
            } else {
                None
            }
        }
        FunctionData::Add { l, r, .. } => {
            if let (Some(l), Some(r)) = (quick_eval_constant(l), quick_eval_constant(r)) {
                Some(l + r)
            } else {
                None
            }
        }
        _ => None,
    }
}

pub fn eval_condition<'e>(cond: &Condition<'e>, vals: &HashMap<Variable<'e>, BigInt>) -> bool {
    match cond {
        Condition::IsDivisor(IsDivisor { p, c }) => (p.eval(vals).unwrap() % c).is_zero(),
        Condition::IsNotNeg(IsNotNeg { p }) => !p.eval(vals).unwrap().is_negative(),
    }
}

pub fn eval<'e>(f: &Function<'e>, vals: &HashMap<Variable<'e>, BigInt>) -> BigInt {
    match f.data() {
        FunctionData::Polynomial { p } => {
            let x: BigRational = p
                .iter()
                .map(|(m, c)| -> BigRational {
                    m.iter()
                        .map(|(v, r)| BigRational::from(vals.get(v).unwrap().pow(*r as u32)))
                        .fold(c.clone(), |acc, x| acc * x)
                })
                .sum();

            assert!(x.is_integer());
            x.to_integer()
        }
        FunctionData::Add { l, r, .. } => eval(l, vals) + eval(r, vals),
        FunctionData::Mul { l, r, .. } => eval(l, vals) * eval(r, vals),
        FunctionData::Neg { v } => -eval(v, vals),
        FunctionData::If { cond, f, .. } => {
            if eval_condition(cond, vals) {
                eval(f, vals)
            } else {
                BigInt::zero()
            }
        }
        FunctionData::LoopSum { i, l, r, f, .. } => {
            let l = eval(l, vals);
            let r = eval(r, vals);
            let mut sum = BigInt::zero();
            let mut j = l;
            let mut vals = vals.clone();
            while j <= r {
                vals.insert(i.clone(), j.clone());
                sum += eval(f, &vals);
                j += BigInt::one();
            }
            sum
        }
    }
}

pub fn eval_function<'e>(f: &FunctionDeclare<'e>, vals: &Vec<BigInt>) -> BigInt {
    assert_eq!(vals.len(), f.args.len());
    let map = HashMap::from_iter(
        f.args
            .iter()
            .zip(vals.iter())
            .map(|(x, y)| (x.clone(), y.clone())),
    );
    eval(&f.body, &map)
}
