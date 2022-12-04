use core::fmt;
use std::collections::hash_map::IntoIter;
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

#[derive(Debug, Clone)]
pub struct Polynomial<K, V> {
    pub v: HashMap<Monomial<K>, V>,
}

impl<K, V> Default for Polynomial<K, V> {
    fn default() -> Self {
        Polynomial {
            v: HashMap::default(),
        }
    }
}
impl<K, V> PartialEq for Polynomial<K, V>
where
    K: Hash + Eq,
    V: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.v == other.v
    }
}
impl<K, V> Eq for Polynomial<K, V>
where
    K: Hash + Eq,
    V: Eq,
{
}

impl<K, V> Polynomial<K, V> {
    pub fn iter(&self) -> impl Iterator<Item = (&Monomial<K>, &V)> + '_ {
        self.v.iter()
    }
}
impl<K, V> IntoIterator for Polynomial<K, V> {
    type Item = (Monomial<K>, V);
    type IntoIter = IntoIter<Monomial<K>, V>;
    fn into_iter(self) -> Self::IntoIter {
        self.v.into_iter()
    }
}

impl<K, V> Polynomial<K, V>
where
    K: Hash + Eq,
    V: Zero,
{
    pub fn set_coefficient(&mut self, p: Monomial<K>, c: V) {
        if c.is_zero() {
            self.v.remove(&p);
        } else {
            self.v.insert(p, c);
        };
    }
}
impl<K, V> Polynomial<K, V>
where
    K: Hash + Eq,
    V: Clone + Zero,
{
    pub fn coefficient(&self, p: &Monomial<K>) -> V {
        self.v.get(p).unwrap_or(&V::zero()).clone()
    }
    fn add_coefficient(&mut self, p: Monomial<K>, c: V) {
        let c = c + self.coefficient(&p);
        self.set_coefficient(p, c);
    }
}

impl<K, V> Polynomial<K, V>
where
    K: Hash + Eq,
    V: Clone + Zero,
{
    pub fn to_constant(&self) -> Option<V> {
        if !self.iter().any(|(p, _)| p.degree() >= 1) {
            Some(self.coefficient(&Monomial::default()))
        } else {
            None
        }
    }
}
impl<K, V> Polynomial<K, V>
where
    K: Hash + Eq + Clone,
    V: Clone + Zero,
{
    pub fn to_linear_polynomial(&self) -> Option<LinearPolynomial<K, V>> {
        if self.iter().any(|(p, _)| p.degree() >= 2) {
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
}

impl<K, V> From<V> for Polynomial<K, V>
where
    K: Eq + Hash,
    V: Zero,
{
    fn from(item: V) -> Self {
        if item.is_zero() {
            Self::default()
        } else {
            Self {
                v: HashMap::from([(Monomial::default(), item)]),
            }
        }
    }
}
impl<K, V> FromIterator<(Monomial<K>, V)> for Polynomial<K, V>
where
    K: Eq + Hash,
    V: Clone + Zero,
{
    fn from_iter<I: IntoIterator<Item = (Monomial<K>, V)>>(iter: I) -> Self {
        let mut result = Self::default();
        iter.into_iter().for_each(|(p, c)| {
            result.add_coefficient(p, c);
        });
        result
    }
}

impl<K, V, const N: usize> From<[(Monomial<K>, V); N]> for Polynomial<K, V>
where
    K: Eq + Hash,
    V: Clone + Zero,
{
    fn from(arr: [(Monomial<K>, V); N]) -> Self {
        Self::from_iter(arr)
    }
}
impl<K, V> From<Monomial<K>> for Polynomial<K, V>
where
    K: Eq + Hash,
    V: One,
{
    fn from(item: Monomial<K>) -> Self {
        Polynomial {
            v: HashMap::from([(item, V::one())]),
        }
    }
}
impl<K, V> From<LinearPolynomial<K, V>> for Polynomial<K, V>
where
    K: Ord + Hash,
    V: Zero + Clone,
{
    fn from(item: LinearPolynomial<K, V>) -> Self {
        Self::from_iter(item.into_iter().map(|(v, c)| {
            if let Some(v) = v {
                (Monomial::from(v), c)
            } else {
                (Monomial::default(), c)
            }
        }))
    }
}

impl<K, V> Zero for Polynomial<K, V>
where
    K: Eq + Hash,
    V: Clone + Zero,
{
    fn zero() -> Self {
        Self::default()
    }
    fn is_zero(&self) -> bool {
        self.v.is_empty()
    }
    fn set_zero(&mut self) {
        self.v.clear()
    }
}

impl<K, V> Add for Polynomial<K, V>
where
    K: Eq + Hash,
    V: Clone + Zero,
{
    type Output = Self;
    fn add(self, other: Self) -> Self {
        let mut result = self;
        other.v.into_iter().for_each(|(p, c)| {
            result.add_coefficient(p, c);
        });
        result
    }
}
impl<K, V> AddAssign for Polynomial<K, V>
where
    K: Eq + Hash,
    V: Clone + Zero,
{
    fn add_assign(&mut self, other: Self) {
        other.v.into_iter().for_each(|(p, c)| {
            self.add_coefficient(p, c);
        });
    }
}

impl<K, V> Sub for Polynomial<K, V>
where
    K: Eq + Hash,
    V: Clone + Zero + Neg<Output = V>,
{
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        let mut result = self;
        other.v.into_iter().for_each(|(p, c)| {
            result.add_coefficient(p, -c);
        });
        result
    }
}
impl<K, V> Mul for Polynomial<K, V>
where
    K: Ord + Hash + Clone,
    V: Clone + Zero + Mul<Output = V>,
{
    type Output = Self;
    fn mul(self, other: Self) -> Self {
        let mut result = Self::zero();
        for (p0, c0) in &self.v {
            for (p1, c1) in &other.v {
                result.add_coefficient(p0.clone() * p1.clone(), c0.clone() * c1.clone());
            }
        }
        result
    }
}
impl<K, V> Div<V> for Polynomial<K, V>
where
    K: Eq + Hash,
    V: Clone + Div<Output = V>,
{
    type Output = Self;
    fn div(self, other: V) -> Self {
        Self {
            v: HashMap::from_iter(self.v.into_iter().map(|(p0, c0)| (p0, c0 / other.clone()))),
        }
    }
}
impl<K, V> Neg for Polynomial<K, V>
where
    K: Eq + Hash,
    V: Clone + Zero + Neg<Output = V>,
{
    type Output = Self;
    fn neg(self) -> Self {
        let mut result = Self::zero();
        self.v.into_iter().for_each(|(p, c)| {
            result.set_coefficient(p, -c);
        });
        result
    }
}
impl<K, V> Pow<usize> for Polynomial<K, V>
where
    K: Clone + Ord + Hash,
    V: Clone + Zero + One,
{
    type Output = Self;

    fn pow(self, n: usize) -> Self {
        // TODO: binary pow
        let mut result = Self::from(V::one());
        for _ in 0..n {
            result = result * self.clone()
        }
        result
    }
}

impl<K> Polynomial<K, BigRational>
where
    K: Clone + Ord + Hash,
{
    // x -> f
    pub fn composite(&self, x: &K, f: &Polynomial<K, BigRational>) -> Self {
        let mut result: Polynomial<K, BigRational> = zero();

        self.v.iter().for_each(|(p, c)| {
            let exp = p.exponent(x);

            let mut p = p.clone();
            p.set_exponent(x.clone(), 0);

            result += Polynomial::from(p) * f.clone().pow(exp) * Polynomial::from(c.clone());
        });
        result
    }

    // P(i) = p(1) + p(2) + ... + p(i)
    pub fn discrete_integral(&self, x: &K) -> Self {
        let mut result: Polynomial<K, BigRational> = zero();
        self.v.iter().for_each(|(p, c)| {
            let exp = p.exponent(x);
            let faulhaber_pol = faulhaber(exp);

            faulhaber_pol.iter().enumerate().for_each(|(deg, c2)| {
                let mut p = p.clone();
                p.set_exponent(x.clone(), deg);
                result.add_coefficient(p, c.clone() * c2)
            });
        });
        result
    }
}

impl<K, V> Display for Polynomial<K, V>
where
    K: Ord + Clone + Hash + Display,
    V: Eq + One + Display,
{
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
                    } else if c.is_one() {
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
