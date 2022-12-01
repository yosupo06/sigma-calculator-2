use num::{
    integer::{gcd, lcm},
    BigInt, BigRational, Integer, One, Signed, Zero,
};

use crate::{polynomial::linear_polynomial::LinearPolynomial, variable::Variable};

pub fn mod_inverse(a: BigInt, b: BigInt) -> BigInt {
    assert_eq!(BigInt::one(), gcd(a.clone(), b.clone()));
    let c = a.extended_gcd_lcm(&b).0.x;
    ((c % b.clone()) + b.clone()) % b
}

// C(n, k)
fn comb(n: usize, k: usize) -> BigInt {
    (n - k + 1..n + 1)
        .map(BigInt::from)
        .fold(BigInt::one(), |x, y| x * y)
        / (1..k + 1)
            .map(BigInt::from)
            .fold(BigInt::one(), |x, y| x * y)
}

fn bernoulli(n: usize) -> BigRational {
    if n == 0 {
        return BigRational::one();
    }
    -(0..n)
        .map(|k| BigRational::from(comb(n + 1, k)) * bernoulli(k))
        .sum::<BigRational>()
        / BigRational::from(BigInt::from(n + 1))
}

pub fn faulhaber(n: usize) -> Vec<BigRational> {
    [BigRational::zero()]
        .into_iter()
        .chain((1..n + 2).map(|i| {
            let x = BigRational::from(comb(n + 1, i)) * bernoulli(n + 1 - i)
                / BigRational::from(BigInt::from(n + 1));
            if i == n {
                -x
            } else {
                x
            }
        }))
        .collect()
}

impl<'e> LinearPolynomial<Variable<'e>, BigRational> {
    pub fn numer_gcd(&self) -> BigInt {
        // some x s.t. (evaluated p) is integer => x | (evaluated p)
        if self.is_zero() {
            return BigInt::one();
        }

        let denom_lcm = self
            .iter()
            .map(|(_, c)| c.denom().clone())
            .reduce(lcm)
            .unwrap();

        self.iter()
            .map(|(_, c)| c.numer() * denom_lcm.clone() / c.denom())
            .reduce(gcd)
            .unwrap()
            .abs()
    }
}

#[cfg(test)]
mod tests {
    use num::{integer::gcd, BigInt, BigRational, One, Zero};

    use crate::math::{bernoulli, faulhaber};

    use super::{comb, mod_inverse};

    #[test]
    fn mod_inverse_test() {
        for a in (1_i32)..30 {
            for b in (1_i32)..30 {
                if gcd(a, b) != 1 {
                    continue;
                }
                let c = mod_inverse(a.into(), b.into());
                assert_eq!(BigInt::one() % b, (a * c) % b);
            }
        }
    }

    #[test]
    fn comb_test() {
        assert_eq!(BigInt::from(210), comb(10, 4));
    }

    #[test]
    fn bernoulli_test() {
        assert_eq!(BigRational::one(), bernoulli(0));
        assert_eq!(
            BigRational::new(BigInt::from(-1_i32), BigInt::from(2)),
            bernoulli(1)
        );
        assert_eq!(
            BigRational::new(BigInt::from(1), BigInt::from(6)),
            bernoulli(2)
        );
        assert_eq!(BigRational::zero(), bernoulli(3));
    }

    #[test]
    fn faulhaber_test() {
        assert_eq!(
            faulhaber(0),
            vec![
                BigRational::zero(),
                BigRational::from(BigInt::from(1 as i32)),
            ]
        );
        assert_eq!(
            faulhaber(1),
            vec![
                BigRational::zero(),
                BigRational::new(BigInt::from(1 as i32), BigInt::from(2 as i32)),
                BigRational::new(BigInt::from(1 as i32), BigInt::from(2 as i32)),
            ]
        );
        assert_eq!(
            faulhaber(2),
            vec![
                BigRational::zero(),
                BigRational::new(BigInt::from(1 as i32), BigInt::from(6 as i32)),
                BigRational::new(BigInt::from(3 as i32), BigInt::from(6 as i32)),
                BigRational::new(BigInt::from(2 as i32), BigInt::from(6 as i32)),
            ]
        );
        assert_eq!(
            faulhaber(3),
            vec![
                BigRational::zero(),
                BigRational::zero(),
                BigRational::new(BigInt::from(1 as i32), BigInt::from(4 as i32)),
                BigRational::new(BigInt::from(2 as i32), BigInt::from(4 as i32)),
                BigRational::new(BigInt::from(1 as i32), BigInt::from(4 as i32)),
            ]
        );
    }
}
