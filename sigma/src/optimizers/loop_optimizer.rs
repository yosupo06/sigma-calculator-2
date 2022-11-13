use num::{integer::gcd, BigInt, BigRational, One, Signed, ToPrimitive, Zero};

use crate::{
    function::{Function, FunctionData},
    math::mod_inverse,
    polynomial::{linear_polynomial::LinearPolynomial, polynomial::Polynomial},
    variable::Variable,
};

use super::Optimizer;

#[derive(Debug, Clone)]
struct LinearIsNotNeg<'e> {
    // evaluated p isn't integer => undefined
    p: LinearPolynomial<Variable<'e>>,
}
impl<'e> LinearIsNotNeg<'e> {
    fn is_not_neg(p: LinearPolynomial<Variable<'e>>) -> Self {
        if p == Default::default() {
            return LinearIsNotNeg {
                p: Default::default(),
            };
        }

        let g = p.numer_gcd();
        LinearIsNotNeg {
            p: p / BigRational::from(g),
        }
    }
}
impl<'e> From<LinearIsNotNeg<'e>> for Function<'e> {
    fn from(item: LinearIsNotNeg<'e>) -> Self {
        Function::new_int_is_not_neg(item.p)
    }
}

#[derive(Debug, Clone)]
struct LinearIsDivisor<'e> {
    // evaluated p isn't integer => undefined
    p: LinearPolynomial<Variable<'e>>,
    c: BigInt,
}
impl<'e> LinearIsDivisor<'e> {
    fn is_divisor(p: LinearPolynomial<Variable<'e>>, c: BigInt) -> Self {
        assert_ne!(c, BigInt::zero());

        if p == Default::default() {
            return LinearIsDivisor {
                p: Default::default(),
                c: BigInt::one(),
            };
        }

        let g = p.numer_gcd();
        LinearIsDivisor {
            p: p / BigRational::from(g.clone()),
            c: c.clone() / gcd(c, g),
        }
    }
}
impl<'e> From<LinearIsDivisor<'e>> for Function<'e> {
    fn from(item: LinearIsDivisor<'e>) -> Self {
        Function::new_int_is_divisor(item.p, item.c)
    }
}

#[derive(Debug, Clone)]
enum LinearCondition<'e> {
    IsNotNeg(LinearIsNotNeg<'e>),
    IsDivisor(LinearIsDivisor<'e>),
}
impl<'e> From<LinearCondition<'e>> for Function<'e> {
    fn from(item: LinearCondition<'e>) -> Self {
        match item {
            LinearCondition::IsNotNeg(c) => c.into(),
            LinearCondition::IsDivisor(c) => c.into(),
        }
    }
}

fn to_linear_polynomial<'e>(f: &Function<'e>) -> Option<LinearPolynomial<Variable<'e>>> {
    match f.data() {
        FunctionData::PolynomialAsInt { p } => p.to_linear_polynomial(),
        _ => None,
    }
}

fn to_linear_condition<'e>(f: &Function<'e>) -> Option<LinearCondition<'e>> {
    if let FunctionData::IntIsNotNeg { p } = f.data() {
        return Some(LinearCondition::IsNotNeg(LinearIsNotNeg { p: p.clone() }));
    }
    if let FunctionData::IntIsDivisor { l, r } = f.data() {
        return Some(LinearCondition::IsDivisor(LinearIsDivisor::is_divisor(
            l.clone(),
            r.clone(),
        )));
    }
    None
}

fn sum_linear_is_not_neg_pol<'e>(
    x: &Variable<'e>,
    conds: &[LinearIsNotNeg<'e>],
    pol: &Polynomial<Variable<'e>>,
) -> Option<Function<'e>> {
    let pol = pol.discrete_integral(x);

    let mut unrelated_conds = vec![];
    // p.1 <= i * p.0
    let mut l_conds = vec![];
    // i * p.0 <= p.1
    let mut r_conds = vec![];

    // x.1 / x.0 <= y.1 / y.0
    let assume_le = |x: &(BigRational, LinearPolynomial<Variable<'e>>),
                     y: &(BigRational, LinearPolynomial<Variable<'e>>)|
     -> LinearIsNotNeg {
        LinearIsNotNeg::is_not_neg(y.1.clone() * x.0.clone() - x.1.clone() * y.0.clone())
    };
    // x.1 / x.0 < y.1 / y.0
    let assume_lt = |x: &(BigRational, LinearPolynomial<Variable<'e>>),
                     y: &(BigRational, LinearPolynomial<Variable<'e>>)|
     -> LinearIsNotNeg {
        LinearIsNotNeg::is_not_neg(
            y.1.clone() * x.0.clone() - x.1.clone() * y.0.clone() - BigRational::one().into(),
        )
    };

    conds.iter().for_each(|LinearIsNotNeg { p }| {
        let coef = p.coefficient(&Some(x.clone()));
        if coef.is_zero() {
            unrelated_conds.push(LinearIsNotNeg { p: p.clone() });
        } else {
            let mut p = p.clone();
            p.set_coefficient(Some(x.clone()), BigRational::zero());
            if coef.is_negative() {
                r_conds.push((-coef, p));
            } else if coef.is_positive() {
                l_conds.push((coef, -p));
            } else {
                unreachable!("coef must be one of 0/+/-");
            }
        }
    });

    let n = l_conds.len();
    let m = r_conds.len();

    let mut sums = vec![];
    for i in 0..n {
        let coef_x = l_conds[i].0.clone();
        assert_ne!(coef_x, BigRational::zero());
        assert_eq!(coef_x.denom(), &BigInt::one());
        let coef_x_usize = coef_x.to_usize()?;
        // [(l_conds[i].1 - rem) % coef_x == 0] -p((l_conds[i].1 - rem) / coef_i)
        let mut sum = (1..coef_x_usize + 1)
            .map(|rem| {
                let trg = l_conds[i].1.clone()
                    - LinearPolynomial::from(BigRational::from(BigInt::from(rem)));

                Function::new_if(
                    LinearIsDivisor::is_divisor(trg.clone(), coef_x.to_integer()).into(),
                    Function::new_polynomial_as_int(
                        -pol.composite(x, &(Polynomial::from(trg) / coef_x.clone())),
                    ),
                )
            })
            .reduce(|x, y| (x + y).unwrap())
            .unwrap();

        // use l_conds[i]
        let mut conds = vec![];
        for i2 in 0..i {
            conds.push(assume_lt(&l_conds[i2], &l_conds[i]));
        }
        for i2 in i + 1..n {
            conds.push(assume_le(&l_conds[i2], &l_conds[i]));
        }
        for j in 0..m {
            conds.push(assume_le(&l_conds[i], &r_conds[j]));
        }
        for ele in conds {
            sum = Function::new_if(ele.into(), sum);
        }
        sums.push(sum);
    }
    for j in 0..m {
        // use r_conds[j]
        let mut conds = vec![];
        for j2 in 0..j {
            conds.push(assume_lt(&r_conds[j], &r_conds[j2]));
        }
        for j2 in j + 1..m {
            conds.push(assume_le(&r_conds[j], &r_conds[j2]));
        }
        for i in 0..n {
            conds.push(assume_le(&l_conds[i], &r_conds[j]));
        }

        let coef_x = r_conds[j].0.clone();
        assert_ne!(coef_x, BigRational::zero());
        assert_eq!(coef_x.denom(), &BigInt::one());
        let coef_x_usize = coef_x.to_usize()?;
        // [(r_conds[j].1 - rem) % coef_x == 0] p2((r_conds[j].1 - rem) / coef_i)
        let mut sum = (0..coef_x_usize)
            .map(|rem| {
                let trg = r_conds[j].1.clone()
                    - LinearPolynomial::from(BigRational::from(BigInt::from(rem)));

                Function::new_if(
                    LinearIsDivisor::is_divisor(trg.clone().into(), coef_x.to_integer().clone())
                        .into(),
                    Function::new_polynomial_as_int(pol.composite(
                        x,
                        &(Polynomial::from(trg) / BigRational::from(coef_x.clone())),
                    )),
                )
            })
            .reduce(|x, y| (x + y).unwrap())
            .unwrap();
        for ele in conds {
            sum = Function::new_if(ele.into(), sum);
        }
        sums.push(sum);
    }

    let mut sum = sums.into_iter().reduce(|x, y| (x + y).unwrap()).unwrap();

    for p in unrelated_conds.clone() {
        //TODO
        sum = Function::new_if(p.into(), sum);
    }

    Some(sum)
}

fn sum_polynomial_by_x<'e>(
    x: &Variable<'e>,
    conds: &Vec<LinearCondition<'e>>,
    pol: &Polynomial<Variable<'e>>,
) -> Option<Function<'e>> {
    let mut pol = pol.clone();
    let mut unrelated_conds: Vec<LinearCondition> = vec![];
    let mut is_not_neg_conds = vec![];
    let mut is_divisor_conds = vec![];
    for cond in conds {
        match cond {
            LinearCondition::IsNotNeg(cond) => {
                if cond.p.coefficient(&Some(x.clone())).is_zero() {
                    unrelated_conds.push(LinearCondition::IsNotNeg(cond.clone()))
                } else {
                    is_not_neg_conds.push(cond.clone())
                }
            }
            LinearCondition::IsDivisor(cond) => {
                if cond.p.coefficient(&Some(x.clone())).is_zero() {
                    unrelated_conds.push(LinearCondition::IsDivisor(cond.clone()))
                } else {
                    is_divisor_conds.push(cond.clone())
                }
            }
        }
    }

    let n = is_divisor_conds.len();

    for i in 0..n {
        let mut a = is_divisor_conds[i].p.clone();
        let coef_x = a.coefficient(&Some(x.clone()));
        a.set_coefficient(Some(x.clone()), BigRational::zero());

        let mut c = is_divisor_conds[i].c.clone();
        if coef_x.denom() != &BigInt::one() {
            // TODO?
            a = a * BigRational::from(coef_x.denom().clone());
            c *= coef_x.denom();
        }
        assert!(coef_x.is_integer());
        let coef_x = coef_x.to_integer();
        // c | coef_x * x + a
        let g = gcd(c.clone(), coef_x.clone());
        unrelated_conds.push(LinearCondition::IsDivisor(LinearIsDivisor::is_divisor(
            a.clone(),
            g.clone(),
        )));
        // x -> c/g x - inv(coef_x/g, c/g) * a / g
        let x2 =
            LinearPolynomial::from([(Some(x.clone()), BigRational::new(c.clone(), g.clone()))])
                - a * BigRational::new(
                    mod_inverse(coef_x.clone() / g.clone(), c.clone() / g.clone()),
                    g,
                );

        is_not_neg_conds = is_not_neg_conds
            .into_iter()
            .map(|cond| {
                // TODO
                LinearIsNotNeg::is_not_neg(
                    LinearPolynomial::from_iter(
                        cond.p
                            .iter()
                            .map(|(v, c)| (v.clone(), BigRational::from(c.clone()))),
                    )
                    .composite(&Some(x.clone()), &x2),
                )
            })
            .collect();

        for i2 in i + 1..n {
            is_divisor_conds[i2].p = is_divisor_conds[i2]
                .p
                .clone()
                .composite(&Some(x.clone()), &x2)
        }

        pol = pol.composite(&x.clone().into(), &x2.into());
    }

    let mut result = sum_linear_is_not_neg_pol(x, &is_not_neg_conds, &pol)?;
    for ele in unrelated_conds.clone() {
        result = Function::new_if(ele.into(), result);
    }
    Some(result)
}

fn decompose_sum_if_polynomial<'e>(
    f: &Function<'e>,
) -> Option<Vec<(Vec<LinearCondition<'e>>, Polynomial<Variable<'e>>)>> {
    match f.data() {
        FunctionData::PolynomialAsInt { p } => Some(vec![(vec![], p.clone())]),
        FunctionData::Add { l, r, .. } => {
            if let (Some(mut l), Some(mut r)) = (
                decompose_sum_if_polynomial(l),
                decompose_sum_if_polynomial(r),
            ) {
                l.append(&mut r);
                Some(l)
            } else {
                None
            }
        }
        FunctionData::If { cond, f, .. } => {
            if let (Some(cond), Some(mut f)) =
                (to_linear_condition(cond), decompose_sum_if_polynomial(f))
            {
                f.iter_mut().for_each(|(x, _)| x.insert(0, cond.clone())); // TODO: fix vec.insert(0)
                Some(f)
            } else {
                None
            }
        }
        _ => None,
    }
}

pub struct LoopIfSumOptimizer {}
impl<'e> Optimizer<'e> for LoopIfSumOptimizer {
    fn optimize(&self, f: &Function<'e>) -> Option<Function<'e>> {
        let FunctionData::LoopSum { i, l, r, f, .. } = f.data() else {
            return None
        };
        let Some(sections) = decompose_sum_if_polynomial(f) else {
            return None
        };
        let Some(lcond) = to_linear_polynomial(l) else {
            return None
        };
        let Some(rcond) = to_linear_polynomial(r) else {
            return None
        };

        let lcond = LinearPolynomial::from([(Some(i.clone()), BigRational::one())]) - lcond;
        let rcond = rcond - LinearPolynomial::from([(Some(i.clone()), BigRational::one())]);

        let lcond = LinearCondition::IsNotNeg(LinearIsNotNeg::is_not_neg(lcond));
        let rcond = LinearCondition::IsNotNeg(LinearIsNotNeg::is_not_neg(rcond));

        let sections: Vec<Option<Function<'e>>> = sections
            .into_iter()
            .map(|(mut conds, p)| {
                // TODO: fix vec.insert(0)
                conds.insert(0, lcond.clone());
                conds.insert(0, rcond.clone());
                sum_polynomial_by_x(i, &conds, &p)
            })
            .collect();

        if sections.iter().any(|x| x.is_none()) {
            return None;
        }
        sections
            .into_iter()
            .reduce(|x, y| Some((x.unwrap() + y.unwrap()).unwrap()))
            .unwrap()
    }
}
