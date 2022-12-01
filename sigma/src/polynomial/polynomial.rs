use core::fmt;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use std::hash::Hash;
use std::ops::{Add, AddAssign, Div, Mul, Neg, Sub};

use num::rational::BigRational;
use num::traits::Zero;
use num::traits::{One, Pow};
use num::zero;

use crate::math::faulhaber;

use super::linear_polynomial::LinearPolynomial;
use super::monomial::Monomial;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Polynomial<T>
where
    T: Ord + Clone + Hash,
{
    pub v: HashMap<Monomial<T>, BigRational>,
}
impl<T: Ord + Clone + Hash> Polynomial<T> {
    pub fn to_constant(&self) -> Option<BigRational> {
        if self.iter().find(|(p, _)| p.degree() >= 1).is_none() {
            Some(self.coefficient(&Monomial::default()))
        } else {
            None
        }
    }
    pub fn to_linear_polynomial(&self) -> Option<LinearPolynomial<T, BigRational>> {
        if self.iter().find(|(p, _)| p.degree() >= 2).is_some() {
            return None;
        }

        let mut result = LinearPolynomial::default();
        for (m, r) in self.iter() {
            let v = if m.degree() == 0 {
                None
            } else {
                Some(m.iter().next().unwrap().0.clone())
            };
            result.set_coefficient(v, r.clone());
        }
        Some(result)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Monomial<T>, &BigRational)> + '_ {
        self.v.iter()
    }
    pub fn into_iter(self) -> impl IntoIterator<Item = (Monomial<T>, BigRational)> {
        self.v.into_iter()
    }
}
impl<T> Default for Polynomial<T>
where
    T: Ord + Clone + Hash,
{
    fn default() -> Self {
        zero()
    }
}
impl<T> Zero for Polynomial<T>
where
    T: Ord + Clone + Hash,
{
    fn zero() -> Self {
        Polynomial { v: HashMap::new() }
    }
    fn is_zero(&self) -> bool {
        self.v.is_empty()
    }
    fn set_zero(&mut self) {
        self.v.clear()
    }
}

impl<T: Ord + Clone + Hash> From<BigRational> for Polynomial<T> {
    fn from(item: BigRational) -> Self {
        if item.is_zero() {
            zero()
        } else {
            Polynomial {
                v: HashMap::from([(Monomial::default(), item)]),
            }
        }
    }
}
impl<T: Ord + Clone + Hash> FromIterator<(Monomial<T>, BigRational)> for Polynomial<T> {
    fn from_iter<I: IntoIterator<Item = (Monomial<T>, BigRational)>>(iter: I) -> Polynomial<T> {
        let mut result: Polynomial<T> = zero();
        iter.into_iter().for_each(|(p, c)| {
            result.add_coefficient(p, c);
        });
        result
    }
}
impl<T: Ord + Clone + Hash, const N: usize> From<[(Monomial<T>, BigRational); N]>
    for Polynomial<T>
{
    fn from(arr: [(Monomial<T>, BigRational); N]) -> Self {
        Self::from_iter(arr)
    }
}

impl<T: Ord + Clone + Hash> From<Monomial<T>> for Polynomial<T> {
    fn from(item: Monomial<T>) -> Self {
        Polynomial {
            v: HashMap::from([(item, BigRational::one())]),
        }
    }
}
impl<T: Ord + Clone + Hash> From<LinearPolynomial<T, BigRational>> for Polynomial<T> {
    fn from(item: LinearPolynomial<T, BigRational>) -> Self {
        Self::from_iter(item.into_iter().map(|(v, c)| {
            if let Some(v) = v {
                (Monomial::from(v), c)
            } else {
                (Monomial::default(), c)
            }
        }))
    }
}

impl<T: Ord + Clone + Hash> Polynomial<T> {
    pub fn coefficient(&self, p: &Monomial<T>) -> BigRational {
        self.v.get(p).unwrap_or(&BigRational::zero()).clone()
    }
    pub fn set_coefficient(&mut self, p: Monomial<T>, c: BigRational) {
        if c.is_zero() {
            self.v.remove(&p);
        } else {
            self.v.insert(p, c);
        };
    }
    fn add_coefficient(&mut self, p: Monomial<T>, c: BigRational) {
        let c = c + self.coefficient(&p);
        self.set_coefficient(p, c);
    }
}
impl<T: Ord + Clone + Hash> Add for Polynomial<T> {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        let mut result = self;
        other.v.into_iter().for_each(|(p, c)| {
            result.add_coefficient(p, c);
        });
        result
    }
}
impl<T: Ord + Clone + Hash> AddAssign for Polynomial<T> {
    fn add_assign(&mut self, other: Self) {
        other.v.into_iter().for_each(|(p, c)| {
            self.add_coefficient(p, c);
        });
    }
}
impl<T: Ord + Clone + Hash> Sub for Polynomial<T> {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        let mut result = self;
        other.v.into_iter().for_each(|(p, c)| {
            result.add_coefficient(p, -c);
        });
        result
    }
}
impl<T: Ord + Clone + Hash> Mul for Polynomial<T> {
    type Output = Self;
    fn mul(self, other: Self) -> Self {
        let mut result: Polynomial<T> = zero();
        for (p0, c0) in &self.v {
            for (p1, c1) in &other.v {
                result.add_coefficient(p0.clone() * p1.clone(), c0 * c1);
            }
        }
        result
    }
}
impl<T: Ord + Clone + Hash> Div<BigRational> for Polynomial<T> {
    type Output = Self;
    fn div(self, other: BigRational) -> Self {
        Polynomial {
            v: HashMap::from_iter(self.v.into_iter().map(|(p0, c0)| (p0, c0 / other.clone()))),
        }
    }
}
impl<T: Ord + Clone + Hash> Neg for Polynomial<T> {
    type Output = Self;
    fn neg(self) -> Self {
        let mut result: Polynomial<T> = zero();
        self.v.into_iter().for_each(|(p, c)| {
            result.add_coefficient(p, -c);
        });
        result
    }
}
impl<T: Ord + Clone + Hash> Pow<usize> for Polynomial<T> {
    type Output = Self;

    fn pow(self, n: usize) -> Self {
        // TODO: binary pow
        let mut result = Self::from(BigRational::one());
        for _ in 0..n {
            result = result * self.clone()
        }
        result
    }
}

impl<T: Ord + Clone + Hash> Polynomial<T> {
    // x -> f
    pub fn composite(&self, x: &T, f: &Polynomial<T>) -> Self {
        let mut result: Polynomial<T> = zero();

        self.v.iter().for_each(|(p, c)| {
            let exp = p.exponent(x);

            let mut p = p.clone();
            p.set_exponent(x.clone(), 0);

            result += Polynomial::from(p) * f.clone().pow(exp) * Polynomial::from(c.clone());
        });
        result
    }

    // P(i) = p(1) + p(2) + ... + p(i)
    pub fn discrete_integral(&self, x: &T) -> Self {
        let mut result: Polynomial<T> = zero();
        self.v.iter().for_each(|(p, c)| {
            let exp = p.exponent(x);
            let faulhaber_pol = faulhaber(exp);

            faulhaber_pol.iter().enumerate().for_each(|(deg, c2)| {
                let mut p = p.clone();
                p.set_exponent(x.clone(), deg);
                result.add_coefficient(p, c * c2);
            });
        });
        result
    }
}

impl<T: Ord + Clone + Hash + Display> Display for Polynomial<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.v.is_empty() {
            return f.write_str("0");
        }
        f.write_str(
            &self
                .v
                .iter()
                .map(|(p, c)| {
                    if p.is_one() {
                        format!("{}", c)
                    } else if c == &BigRational::one() {
                        format!("{}", p)
                    } else {
                        format!("({}) {}", c, p)
                    }
                })
                .collect::<Vec<_>>()
                .join(" + "),
        )
    }
}
