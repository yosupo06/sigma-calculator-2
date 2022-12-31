use num::{integer::gcd, zero, BigInt, BigRational, One, ToPrimitive, Zero};

use crate::{
    function::{Condition, Function, FunctionData, IsDivisor, IsNotNeg},
    math::mod_inverse,
    polynomials::{
        linear_polynomial::LinearPolynomial, monomial::Monomial, polynomial::Polynomial,
    },
    variable::Variable,
};

use super::OptimizeRule;

pub fn loop_if_sum_optimize_rule<'e>() -> impl OptimizeRule<'e> {
    |f: &Function<'e>| {
        let FunctionData::LoopSum { i, l, r, f, .. } = f.data() else {
            return None
        };
        let Some(sections) = decompose_sum_if_polynomial(f) else {
            return None
        };
        let Some(lcond) = l.to_linear_polynomial() else {
            return None
        };
        let Some(rcond) = r.to_linear_polynomial() else {
            return None
        };

        let lcond = LinearPolynomial::from([(Some(i.clone()), BigInt::one())]) - lcond;
        let rcond = rcond - LinearPolynomial::from([(Some(i.clone()), BigInt::one())]);

        let lcond = Condition::IsNotNeg(IsNotNeg::new(lcond));
        let rcond = Condition::IsNotNeg(IsNotNeg::new(rcond));

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

fn sum_linear_is_not_neg_pol<'e>(
    x: &Variable<'e>,
    conds: &[IsNotNeg<'e>],
    pol: &Polynomial<Variable<'e>, BigRational>,
) -> Option<Function<'e>> {
    let pol = pol.discrete_integral(x);

    let mut unrelated_conds = vec![];

    type XFocusedCond<'e> = (BigInt, LinearPolynomial<Variable<'e>, BigInt>);

    // p.1 <= x * p.0
    let mut l_conds = vec![];
    // x * p.0 <= p.1
    let mut r_conds = vec![];

    // x.1 / x.0 <= y.1 / y.0
    let assume_le = |x: &XFocusedCond<'e>, y: &XFocusedCond<'e>| -> IsNotNeg {
        IsNotNeg::new(y.1.clone() * x.0.clone() - x.1.clone() * y.0.clone())
    };
    // x.1 / x.0 < y.1 / y.0
    let assume_lt = |x: &XFocusedCond<'e>, y: &XFocusedCond<'e>| -> IsNotNeg {
        IsNotNeg::new(y.1.clone() * x.0.clone() - x.1.clone() * y.0.clone() - BigInt::one().into())
    };

    conds.iter().for_each(|IsNotNeg { p }| {
        let coef = p.coefficient(&Some(x.clone()));
        match coef.sign() {
            num::bigint::Sign::NoSign => unrelated_conds.push(IsNotNeg { p: p.clone() }),
            num::bigint::Sign::Plus => {
                let mut p = p.clone();
                p.set_coefficient(Some(x.clone()), zero());
                l_conds.push((coef, -p));
            }
            num::bigint::Sign::Minus => {
                let mut p = p.clone();
                p.set_coefficient(Some(x.clone()), zero());
                r_conds.push((-coef, p));
            }
        }
    });

    let n = l_conds.len();
    let m = r_conds.len();

    let mut sums = vec![];
    for i in 0..n {
        let coef_x = l_conds[i].0.clone();
        assert_ne!(coef_x, BigInt::zero());
        let coef_x_usize = coef_x.to_usize()?;
        // [(l_conds[i].1 - rem) % coef_x == 0] -p((l_conds[i].1 - rem) / coef_i)
        let mut sum = (1..coef_x_usize + 1)
            .map(|rem| {
                let trg = l_conds[i].1.clone() - LinearPolynomial::from(BigInt::from(rem));
                let trg_as_pol = Polynomial::from(LinearPolynomial::from_iter(
                    trg.clone()
                        .into_iter()
                        .map(|x| (x.0, BigRational::from(x.1))),
                ));
                Function::new_if(
                    Condition::IsDivisor(IsDivisor {
                        p: trg.clone(),
                        c: coef_x.clone(),
                    }),
                    Function::new_polynomial_as_int(
                        -pol.composite(x, &(trg_as_pol / BigRational::from(coef_x.clone()))),
                    ),
                )
            })
            .reduce(Function::new_add)
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
            sum = Function::new_if(Condition::IsNotNeg(ele), sum);
        }
        sums.push(sum);
    }
    for j in 0..m {
        // use r_conds[j]

        let coef_x = r_conds[j].0.clone();
        assert_ne!(coef_x, BigInt::zero());
        let coef_x_usize = coef_x.to_usize()?;
        // [(r_conds[j].1 - rem) % coef_x == 0] p2((r_conds[j].1 - rem) / coef_i)
        let mut sum = (0..coef_x_usize)
            .map(|rem| {
                let trg = r_conds[j].1.clone() - LinearPolynomial::from(BigInt::from(rem));
                let trg_as_pol = Polynomial::from(LinearPolynomial::from_iter(
                    trg.clone()
                        .into_iter()
                        .map(|x| (x.0, BigRational::from(x.1))),
                ));

                Function::new_if(
                    Condition::IsDivisor(IsDivisor {
                        p: trg.clone(),
                        c: coef_x.clone(),
                    }),
                    Function::new_polynomial_as_int(
                        pol.composite(x, &(trg_as_pol / BigRational::from(coef_x.clone()))),
                    ),
                )
            })
            .reduce(Function::new_add)
            .unwrap();

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
        for ele in conds {
            sum = Function::new_if(Condition::IsNotNeg(ele), sum);
        }
        sums.push(sum);
    }

    let sum = sums.into_iter().reduce(Function::new_add).unwrap();
    Some(unrelated_conds.into_iter().fold(sum, |f, cond| {
        Function::new_if(Condition::IsNotNeg(cond), f)
    }))
}

fn sum_polynomial_by_x<'e>(
    x: &Variable<'e>,
    conds: &Vec<Condition<'e>>,
    pol: &Polynomial<Variable<'e>, BigRational>,
) -> Option<Function<'e>> {
    let mut pol = pol.clone();
    let mut unrelated_conds: Vec<Condition> = vec![];
    let mut is_not_neg_conds = vec![];
    let mut is_divisor_conds = vec![];
    for cond in conds {
        match cond {
            Condition::IsNotNeg(cond) => {
                if cond.p.coefficient(&Some(x.clone())).is_zero() {
                    unrelated_conds.push(Condition::IsNotNeg(cond.clone()))
                } else {
                    is_not_neg_conds.push(cond.clone())
                }
            }
            Condition::IsDivisor(cond) => {
                if cond.p.coefficient(&Some(x.clone())).is_zero() {
                    unrelated_conds.push(Condition::IsDivisor(cond.clone()))
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
        a.set_coefficient(Some(x.clone()), BigInt::zero());

        let c = is_divisor_conds[i].c.clone();
        // c | coef_x * x + a
        let g = gcd(c.clone(), coef_x.clone());
        unrelated_conds.push(Condition::IsDivisor(IsDivisor::new(a.clone(), g.clone())));
        // x -> (cx - inv(coef_x/g, c/g) * a) / g
        // x -> x2_numer / g
        let x2_numer = LinearPolynomial::from([(Some(x.clone()), c.clone())])
            - a * mod_inverse(coef_x.clone() / g.clone(), c.clone() / g.clone());

        is_not_neg_conds = is_not_neg_conds
            .into_iter()
            .map(|cond| cond.composite(x, &x2_numer, &g))
            .collect();

        for i2 in i + 1..n {
            is_divisor_conds[i2] = is_divisor_conds[i2].composite(x, &x2_numer, &g)
        }

        pol = pol.composite(
            x,
            &(Polynomial::from_iter(
                x2_numer
                    .into_iter()
                    .map(|x| (Monomial::from(x.0), BigRational::from(x.1))),
            ) / BigRational::from(g.clone())),
        );
    }

    let result = sum_linear_is_not_neg_pol(x, &is_not_neg_conds, &pol)?;

    Some(
        unrelated_conds
            .into_iter()
            .fold(result, |f, cond| Function::new_if(cond, f)),
    )
}

fn decompose_sum_if_polynomial<'e>(
    f: &Function<'e>,
) -> Option<Vec<(Vec<Condition<'e>>, Polynomial<Variable<'e>, BigRational>)>> {
    match f.data() {
        FunctionData::Polynomial { p } => Some(vec![(vec![], p.clone())]),
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
            if let Some(mut f) = decompose_sum_if_polynomial(f) {
                f.iter_mut().for_each(|(x, _)| x.insert(0, cond.clone())); // TODO: fix vec.insert(0)
                Some(f)
            } else {
                None
            }
        }
        _ => None,
    }
}
