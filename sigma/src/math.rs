use num::{
    integer::{gcd, lcm},
    BigInt, BigRational, One, Signed, Zero,
};

use crate::{polynomial::linear_polynomial::LinearPolynomial, variable::Variable};

pub fn mod_inverse(a: BigInt, b: BigInt) -> BigInt {
    // todo: ext_gcd
    assert_eq!(BigInt::one(), gcd(a.clone(), b.clone()));
    if b == BigInt::one() {
        return BigInt::zero();
    }

    let mut c = BigInt::one();
    loop {
        if c.clone() * a.clone() % b.clone() == BigInt::one() {
            return c;
        }
        c += BigInt::one();
    }
}

pub fn faulhaber(n: usize) -> Vec<BigRational> {
    if n == 0 {
        return vec![
            BigRational::zero(),
            BigRational::from(BigInt::from(1 as i32)),
        ];
    }
    if n == 1 {
        return vec![
            BigRational::zero(),
            BigRational::new(BigInt::from(1 as i32), BigInt::from(2 as i32)),
            BigRational::new(BigInt::from(1 as i32), BigInt::from(2 as i32)),
        ];
    }
    if n == 2 {
        return vec![
            BigRational::zero(),
            BigRational::new(BigInt::from(1 as i32), BigInt::from(6 as i32)),
            BigRational::new(BigInt::from(3 as i32), BigInt::from(6 as i32)),
            BigRational::new(BigInt::from(2 as i32), BigInt::from(6 as i32)),
        ];
    }
    if n == 3 {
        return vec![
            BigRational::zero(),
            BigRational::zero(),
            BigRational::new(BigInt::from(1 as i32), BigInt::from(4 as i32)),
            BigRational::new(BigInt::from(2 as i32), BigInt::from(4 as i32)),
            BigRational::new(BigInt::from(1 as i32), BigInt::from(4 as i32)),
        ];
    }
    todo!("todo")
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
    use num::{integer::gcd, BigInt, One};

    use super::mod_inverse;

    #[test]
    fn test() {
        for a in (1 as i32)..30 {
            for b in (1 as i32)..30 {
                if gcd(a, b) != 1 {
                    continue;
                }
                let c = mod_inverse(a.into(), b.into());
                assert_eq!(BigInt::one() % b, (a * c) % b);
            }
        }
    }
}
