//use crate::sets_intersection;

use std::collections::HashMap;

use num::{BigInt, BigRational, One, Signed, Zero};

use crate::constant::Constant;
use crate::function::{Function, FunctionData};

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
        FunctionData::IntIsNotNeg { p } => {
            p.to_constant().map(|x| Constant::Bool(!x.is_negative()))
        }
        FunctionData::IntIsDivisor { l, r } => {
            l.to_constant().map(|x| Constant::Bool((x % r).is_zero()))
        }

        _ => None,
    }
}

pub fn eval<'e>(f: &Function<'e>, vals: &HashMap<Variable<'e>, Constant>) -> Constant {
    match f.data() {
        FunctionData::PolynomialAsInt { p } => {
            let x: BigRational = p
                .iter()
                .map(|(m, c)| -> BigRational {
                    m.iter()
                        .map(|(v, r)| {
                            if let Constant::Integer(x) = vals.get(v).unwrap() {
                                BigRational::from(x.pow(*r as u32))
                            } else {
                                unreachable!();
                            }
                        })
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
                    vals.insert(i.clone(), Constant::Integer(j.clone()));
                    sum = (sum + eval(f, &vals)).unwrap();
                    j += BigInt::one();
                }
                sum
            } else {
                todo!()
            }
        }
        FunctionData::IntIsNotNeg { p } => {
            let v = p.eval(&HashMap::from_iter(vals.iter().map(|(v, c)| {
                if let Constant::Integer(x) = c {
                    (v.clone(), BigRational::from(x.clone()))
                } else {
                    unreachable!()
                }
            })));
            if let Some(v) = v {
                Constant::Bool(!v.is_negative())
            } else {
                unreachable!();
            }
        }
        FunctionData::IntIsDivisor { l, r } => {
            let v = l.eval(&HashMap::from_iter(vals.iter().map(|(v, c)| {
                if let Constant::Integer(x) = c {
                    (v.clone(), BigRational::from(x.clone()))
                } else {
                    unreachable!()
                }
            })));
            if let Some(v) = v {
                Constant::Bool((v % r).is_zero())
            } else {
                unreachable!();
            }
        }
        _ => {
            println!("error: {}", f.to_source());
            todo!()
        }
    }
}

pub fn eval_function<'e>(f: &Function<'e>, vals: &Vec<Constant>) -> Constant {
    if let FunctionData::Declare {
        name: _,
        args,
        body,
    } = f.data()
    {
        assert_eq!(vals.len(), args.len());
        let map = HashMap::from_iter(
            args.iter()
                .zip(vals.iter())
                .map(|(x, y)| (x.clone(), y.clone())),
        );
        eval(body, &map)
    } else {
        panic!();
    }
}
