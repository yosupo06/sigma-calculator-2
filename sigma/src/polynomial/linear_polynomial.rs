use std::{
    collections::HashMap,
    hash::Hash,
    ops::{Add, Div, Mul, Neg, Sub},
};

use num::{BigRational, Zero};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinearPolynomial<K>
where
    K: Clone + Hash + Eq,
{
    v: HashMap<Option<K>, BigRational>,
}

impl<K> LinearPolynomial<K>
where
    K: Clone + Eq + Hash,
{
    pub fn new() -> Self {
        LinearPolynomial { v: HashMap::new() }
    }
    pub fn iter(&self) -> impl Iterator<Item = (&Option<K>, &BigRational)> + '_ {
        self.v.iter()
    }
    pub fn into_iter(self) -> impl Iterator<Item = (Option<K>, BigRational)> {
        self.v.into_iter()
    }
    pub fn coefficient(&self, p: &Option<K>) -> BigRational {
        self.v
            .get(p)
            .map(|c| c.clone())
            .unwrap_or(BigRational::zero())
    }
    pub fn constant(&self) -> BigRational {
        self.coefficient(&None)
    }
    pub fn set_coefficient(&mut self, p: Option<K>, c: BigRational) {
        if c.is_zero() {
            self.v.remove(&p);
        } else {
            self.v.insert(p, c);
        };
    }
    pub fn set_constant(&mut self, c: BigRational) {
        self.set_coefficient(None, c)
    }

    pub fn to_constant(&self) -> Option<BigRational> {
        if self.iter().find(|(p, _)| p.is_some()).is_none() {
            Some(self.constant())
        } else {
            None
        }
    }
}
impl<T> Zero for LinearPolynomial<T>
where
    T: Ord + Clone + Hash,
{
    fn zero() -> Self {
        LinearPolynomial { v: HashMap::new() }
    }
    fn is_zero(&self) -> bool {
        self.v.is_empty()
    }
    fn set_zero(&mut self) {
        self.v.clear()
    }
}

impl<K> LinearPolynomial<K>
where
    K: Clone + Eq + Hash,
{
    pub fn eval(&self, map: &HashMap<K, BigRational>) -> Option<BigRational> {
        self.iter()
            .map(|(m, c)| {
                if m.clone().is_none() {
                    Some(c.clone())
                } else {
                    map.get(&m.as_ref().unwrap()).map(|x| x.clone() * c.clone())
                }
            })
            .sum()
    }
}
impl<K> Default for LinearPolynomial<K>
where
    K: Clone + Eq + Hash,
{
    fn default() -> Self {
        LinearPolynomial { v: HashMap::new() }
    }
}
impl<K> From<BigRational> for LinearPolynomial<K>
where
    K: Clone + Eq + Hash,
{
    fn from(c: BigRational) -> Self {
        LinearPolynomial {
            v: HashMap::from([(None, c)]),
        }
    }
}
impl<K> FromIterator<(Option<K>, BigRational)> for LinearPolynomial<K>
where
    K: Clone + Eq + Hash,
{
    fn from_iter<I: IntoIterator<Item = (Option<K>, BigRational)>>(iter: I) -> Self {
        LinearPolynomial {
            v: HashMap::from_iter(iter),
        }
    }
}
impl<K, const N: usize> From<[(Option<K>, BigRational); N]> for LinearPolynomial<K>
where
    K: Clone + Eq + Hash,
{
    fn from(arr: [(Option<K>, BigRational); N]) -> Self {
        Self::from_iter(arr)
    }
}

impl<K> Add for LinearPolynomial<K>
where
    K: Clone + Eq + Hash,
{
    type Output = Self;
    fn add(self, other: Self) -> Self {
        let mut result = self;
        other.iter().for_each(|(p, c)| {
            result.set_coefficient(p.clone(), result.coefficient(p) + c.clone());
        });
        result
    }
}
impl<K> Sub for LinearPolynomial<K>
where
    K: Clone + Eq + Hash,
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
impl<K> Neg for LinearPolynomial<K>
where
    K: Clone + Eq + Hash,
{
    type Output = Self;
    fn neg(self) -> Self {
        let mut result = LinearPolynomial::new();
        self.iter().for_each(|(p, c)| {
            result.set_coefficient(p.clone(), -c.clone());
        });
        result
    }
}
impl<K> Mul<BigRational> for LinearPolynomial<K>
where
    K: Clone + Eq + Hash,
{
    type Output = Self;
    fn mul(self, other: BigRational) -> Self {
        let mut result = LinearPolynomial::new();
        self.iter().for_each(|(p, c)| {
            result.set_coefficient(p.clone(), c.clone() * other.clone());
        });
        result
    }
}
impl<K> Div<BigRational> for LinearPolynomial<K>
where
    K: Clone + Eq + Hash,
{
    type Output = Self;
    fn div(self, other: BigRational) -> Self {
        let mut result = LinearPolynomial::new();
        self.iter().for_each(|(p, c)| {
            result.set_coefficient(p.clone(), c.clone() / other.clone());
        });
        result
    }
}

impl<K> LinearPolynomial<K>
where
    K: Clone + Eq + Hash,
{
    // x -> f
    pub fn composite(self, x: &Option<K>, f: &Self) -> Self {
        let coef = self.coefficient(x);

        let mut result = self;
        result.set_coefficient(x.clone(), BigRational::zero());
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

    use crate::polynomial::linear_polynomial::LinearPolynomial;

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
            HashMap::from_iter(a.composite(&Some(2), &b).into_iter())
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
