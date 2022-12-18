use num::{integer::gcd, BigInt, BigRational, One, Signed, ToPrimitive, Zero};

use crate::{
    function::{Condition, Function, FunctionData, IsDivisor, IsNotNeg},
    math::mod_inverse,
    polynomials::{linear_polynomial::LinearPolynomial, polynomial::Polynomial},
    variable::Variable,
};

use super::OptimizeRule;

fn to_linear_polynomial<'e>(f: &Function<'e>) -> Option<LinearPolynomial<Variable<'e>, BigInt>> {
    match f.data() {
        FunctionData::Polynomial { p } => {
            if let Some(p) = p.to_linear_polynomial() {
                Some(LinearPolynomial::from_iter(
                    p.into_iter().map(|x| (x.0, x.1.to_integer())),
                ))
            } else {
                None
            }
        }
        _ => None,
    }
}

fn sum_linear_is_not_neg_pol<'e>(
    x: &Variable<'e>,
    conds: &[IsNotNeg<'e>],
    pol: &Polynomial<Variable<'e>, BigRational>,
) -> Option<Function<'e>> {
    let pol = pol.discrete_integral(x);

    let mut unrelated_conds = vec![];
    // p.1 <= x * p.0
    let mut l_conds = vec![];
    // x * p.0 <= p.1
    let mut r_conds = vec![];

    type XFocusedCond<'e> = (BigInt, LinearPolynomial<Variable<'e>, BigInt>);
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
        if coef.is_zero() {
            unrelated_conds.push(IsNotNeg { p: p.clone() });
        } else {
            let mut p = p.clone();
            p.set_coefficient(Some(x.clone()), BigInt::zero());
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
                    //LinearIsDivisor::is_divisor(trg.clone(), coef_x.clone()).into(),
                    Condition::IsDivisor(IsDivisor {
                        p: trg.clone(),
                        c: coef_x.clone(),
                    }),
                    Function::new_polynomial_as_int(
                        -pol.composite(x, &(trg_as_pol / BigRational::from(coef_x.clone()))),
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
            sum = Function::new_if(
                Condition::IsNotNeg(IsNotNeg { p: ele.p }),
                //ele.into(),
                sum,
            );
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
                    //                    LinearIsDivisor::is_divisor(trg.clone().into(), coef_x.clone()).into(),
                    Condition::IsDivisor(IsDivisor {
                        p: trg.clone(),
                        c: coef_x.clone(),
                    }),
                    Function::new_polynomial_as_int(
                        pol.composite(x, &(trg_as_pol / BigRational::from(coef_x.clone()))),
                    ),
                )
            })
            .reduce(|x, y| (x + y).unwrap())
            .unwrap();
        for ele in conds {
            sum = Function::new_if(
                Condition::IsNotNeg(IsNotNeg { p: ele.p }),
                //ele.into(),
                sum,
            );
        }
        sums.push(sum);
    }

    let mut sum = sums.into_iter().reduce(|x, y| (x + y).unwrap()).unwrap();

    for p in unrelated_conds.clone() {
        //TODO
        sum = Function::new_if(
            Condition::IsNotNeg(IsNotNeg { p: p.p }),
            //            p.into(),
            sum,
        );
    }

    Some(sum)
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
        let _cg = c.clone() / g.clone();
        // x -> cg' x - inv(coef_x/g, c/g) * a / g
        let x2_numer = LinearPolynomial::from([(Some(x.clone()), c.clone())])
            - a * BigInt::from(mod_inverse(
                coef_x.clone() / g.clone(),
                c.clone() / g.clone(),
            ));

        let x2_as_pol = Polynomial::from(LinearPolynomial::from_iter(
            x2_numer
                .clone()
                .into_iter()
                .map(|x| (x.0, BigRational::from(x.1))),
        )) / BigRational::from(g.clone());
        /*
                    let x2 =
        LinearPolynomial::from([(Some(x.clone()), BigRational::from(cg))])
            - a * BigRational::new(
                mod_inverse(coef_x.clone() / g.clone(), c.clone() / g.clone()),
                g,
            );*/

        is_not_neg_conds = is_not_neg_conds
            .into_iter()
            .map(|cond| {
                // TODO
                cond.composite(x, &x2_numer, &g)
                /*                LinearIsNotNeg::is_not_neg(
                    LinearPolynomial::from_iter(
                        cond.p
                            .iter()
                            .map(|(v, c)| (v.clone(), BigRational::from(c.clone()))),
                    )
                    .composite(&Some(x.clone()), &x2),
                )*/
            })
            .collect();

        for i2 in i + 1..n {
            is_divisor_conds[i2] = is_divisor_conds[i2].composite(x, &x2_numer, &g)
            /*            is_divisor_conds[i2].p = is_divisor_conds[i2]
            .p
            .clone()
            .composite(&Some(x.clone()), &x2)*/
        }

        pol = pol.composite(&x.clone().into(), &x2_as_pol.into());
    }

    let mut result = sum_linear_is_not_neg_pol(x, &is_not_neg_conds, &pol)?;
    for ele in unrelated_conds.clone() {
        result = Function::new_if(ele.into(), result);
    }
    Some(result)
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

pub fn loop_if_sum_optimize_rule<'e>() -> impl OptimizeRule<'e> {
    |f: &Function<'e>| {
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
