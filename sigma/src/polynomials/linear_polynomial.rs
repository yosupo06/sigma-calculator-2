use std::{
    collections::{hash_map::IntoIter, HashMap},
    hash::Hash,
    iter::Sum,
    ops::{Add, Div, Mul, Neg, Sub},
};

use num::Zero;

#[derive(Debug)]
pub struct LinearPolynomial<K, V> {
    v: HashMap<Option<K>, V>,
}
impl<K, V> Default for LinearPolynomial<K, V> {
    fn default() -> Self {
        Self {
            v: HashMap::default(),
        }
    }
}

impl<K, V> Clone for LinearPolynomial<K, V>
where
    K: Clone,
    V: Clone,
{
    fn clone(&self) -> Self {
        LinearPolynomial { v: self.v.clone() }
    }
}
impl<K, V> PartialEq for LinearPolynomial<K, V>
where
    K: Hash + Eq,
    V: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.v == other.v
    }
}
impl<K, V> Eq for LinearPolynomial<K, V>
where
    K: Hash + Eq,
    V: Eq,
{
}

impl<K, V> LinearPolynomial<K, V> {
    pub fn iter(&self) -> impl Iterator<Item = (&Option<K>, &V)> + '_ {
        self.v.iter()
    }
}
impl<K, V> IntoIterator for LinearPolynomial<K, V> {
    type Item = (Option<K>, V);
    type IntoIter = IntoIter<Option<K>, V>;
    fn into_iter(self) -> Self::IntoIter {
        self.v.into_iter()
    }
}
impl<K, V> From<V> for LinearPolynomial<K, V>
where
    K: Eq + Hash,
{
    fn from(c: V) -> Self {
        LinearPolynomial {
            v: HashMap::from([(None, c)]),
        }
    }
}
impl<K, V> FromIterator<(Option<K>, V)> for LinearPolynomial<K, V>
where
    K: Eq + Hash,
{
    fn from_iter<I: IntoIterator<Item = (Option<K>, V)>>(iter: I) -> Self {
        LinearPolynomial {
            v: HashMap::from_iter(iter),
        }
    }
}
impl<K, V, const N: usize> From<[(Option<K>, V); N]> for LinearPolynomial<K, V>
where
    K: Eq + Hash,
{
    fn from(arr: [(Option<K>, V); N]) -> Self {
        Self::from_iter(arr)
    }
}
impl<K, V> LinearPolynomial<K, V>
where
    K: Eq + Hash,
    V: Clone + Zero,
{
    pub fn coefficient(&self, p: &Option<K>) -> V {
        self.v.get(p).cloned().unwrap_or_else(V::zero)
    }
    pub fn constant(&self) -> V {
        self.coefficient(&None)
    }
    pub fn to_constant(&self) -> Option<V> {
        if !self.iter().any(|(p, _)| p.is_some()) {
            Some(self.constant())
        } else {
            None
        }
    }
}
impl<K, V> LinearPolynomial<K, V>
where
    K: Hash + Eq,
    V: Zero,
{
    pub fn set_coefficient(&mut self, p: Option<K>, c: V) {
        if c.is_zero() {
            self.v.remove(&p);
        } else {
            self.v.insert(p, c);
        };
    }
    pub fn set_constant(&mut self, c: V) {
        self.set_coefficient(None, c)
    }
}
impl<K, V> LinearPolynomial<K, V>
where
    K: Clone + Eq + Hash,
    V: Clone + Mul<Output = V> + Sum,
{
    pub fn eval(&self, map: &HashMap<K, V>) -> Option<V> {
        self.iter()
            .map(|(m, c)| {
                if m.clone().is_none() {
                    Some(c.clone())
                } else {
                    map.get(m.as_ref().unwrap()).map(|x| x.clone() * c.clone())
                }
            })
            .sum()
    }
}

impl<K, V> Zero for LinearPolynomial<K, V>
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
impl<K, V> Add for LinearPolynomial<K, V>
where
    K: Eq + Hash,
    V: Clone + Zero,
{
    type Output = Self;
    fn add(self, other: Self) -> Self {
        let mut result = self;
        other.into_iter().for_each(|(p, c)| {
            let val = result.coefficient(&p) + c;
            result.set_coefficient(p, val);
        });
        result
    }
}
impl<K, V> Sub for LinearPolynomial<K, V>
where
    K: Clone + Eq + Hash,
    V: Clone + Zero + Sub<Output = V>,
{
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        let mut result = self;
        other.iter().for_each(|(p, c)| {
            result.set_coefficient(p.clone(), result.coefficient(p) - c.clone());
        });
        result
    }
}
impl<K, V> Neg for LinearPolynomial<K, V>
where
    K: Clone + Eq + Hash,
    V: Clone + Zero + Neg<Output = V>,
{
    type Output = Self;
    fn neg(self) -> Self {
        let mut result = LinearPolynomial::default();
        self.iter().for_each(|(p, c)| {
            result.set_coefficient(p.clone(), -c.clone());
        });
        result
    }
}
impl<K, V> Mul<V> for LinearPolynomial<K, V>
where
    K: Clone + Eq + Hash,
    V: Clone + Zero + Mul<Output = V>,
{
    type Output = Self;
    fn mul(self, other: V) -> Self {
        let mut result = LinearPolynomial::default();
        self.iter().for_each(|(p, c)| {
            result.set_coefficient(p.clone(), c.clone() * other.clone());
        });
        result
    }
}
impl<K, V> Div<V> for LinearPolynomial<K, V>
where
    K: Clone + Eq + Hash,
    V: Clone + Zero + Div<Output = V>,
{
    type Output = Self;
    fn div(self, other: V) -> Self {
        let mut result = LinearPolynomial::default();
        self.iter().for_each(|(p, c)| {
            result.set_coefficient(p.clone(), c.clone() / other.clone());
        });
        result
    }
}

impl<K, V> LinearPolynomial<K, V>
where
    K: Clone + Eq + Hash,
    V: Clone + Zero + Mul<Output = V>,
{
    // x -> f
    pub fn composite(self, x: &K, f: &Self) -> Self {
        let coef = self.coefficient(&Some(x.clone()));

        let mut result = self;
        result.set_coefficient(Some(x.clone()), V::zero());
        f.v.iter().for_each(|(y, c)| {
            result.set_coefficient(y.clone(), result.coefficient(y) + coef.clone() * c.clone());
        });
        result
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use num::{BigInt, BigRational, FromPrimitive};

    use crate::polynomials::linear_polynomial::LinearPolynomial;

    #[test]
    fn from_iter() {
        let a = LinearPolynomial::from([
            (Some(1 as i32), BigRational::from(BigInt::from(2 as i32))),
            (Some(2), BigRational::from(BigInt::from(3 as i32))),
        ]);
        assert_eq!(BigRational::from_u32(2).unwrap(), a.coefficient(&Some(1)));
        assert_eq!(BigRational::from_u32(0).unwrap(), a.coefficient(&Some(3)));
    }

    #[test]
    fn composite() {
        // 2x_1 + 3x_2
        let a = LinearPolynomial::from([
            (Some(1 as i32), BigRational::from(BigInt::from(2 as i32))),
            (Some(2), BigRational::from(BigInt::from(3 as i32))),
        ]);
        // 4x_2 + 5
        let b = LinearPolynomial::from([
            (Some(2 as i32), BigRational::from(BigInt::from(4 as i32))),
            (None, BigRational::from(BigInt::from(5 as i32))),
        ]);

        // 2x_1 + 3(4x_2 + 5) = 2x_1 + 12x_2 + 15
        assert_eq!(
            HashMap::from([
                (Some(1), BigRational::from(BigInt::from(2 as i32))),
                (Some(2), BigRational::from(BigInt::from(12 as i32))),
                (None, BigRational::from(BigInt::from(15 as i32)))
            ]),
            HashMap::from_iter(a.composite(&2, &b).into_iter())
        );
    }

    #[test]
    fn eval() {
        // 1 + 2x_1 + 3x_2
        let a = LinearPolynomial::from([
            (None, BigRational::from_u32(1).unwrap()),
            (Some(1 as i32), BigRational::from_u32(2).unwrap()),
            (Some(2), BigRational::from_u32(3).unwrap()),
        ]);

        // (x_1 = 10, x_2 = 100) => 321
        assert_eq!(
            Some(BigRational::from_u32(321).unwrap()),
            a.eval(&HashMap::from([
                (1 as i32, BigRational::from_u32(10).unwrap()),
                (2 as i32, BigRational::from_u32(100).unwrap())
            ]))
        );
    }
}
