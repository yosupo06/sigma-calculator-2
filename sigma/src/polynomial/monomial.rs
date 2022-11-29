use core::fmt;
use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::ops::Mul;

use num::one;
use num::One;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Monomial<T>
where
    T: Ord + Clone + Hash,
{
    pub v: BTreeMap<T, usize>,
}
impl<T> Default for Monomial<T>
where
    T: Ord + Clone + Hash,
{
    fn default() -> Self {
        one()
    }
}
impl<T> One for Monomial<T>
where
    T: Ord + Clone + Hash,
{
    fn one() -> Self {
        Self { v: BTreeMap::new() }
    }
    fn is_one(&self) -> bool {
        self.v.is_empty()
    }
    fn set_one(&mut self) {
        self.v.clear()
    }
}
impl<T: Ord + Clone + Hash> From<T> for Monomial<T> {
    fn from(item: T) -> Self {
        Monomial {
            v: BTreeMap::from([(item, 1)]),
        }
    }
}
impl<T: Ord + Clone + Hash> From<Option<T>> for Monomial<T> {
    fn from(item: Option<T>) -> Self {
        if let Some(item) = item {
            Monomial {
                v: BTreeMap::from([(item, 1)]),
            }
        } else {
            Default::default()
        }
    }
}
impl<T: Ord + Clone + Hash> Monomial<T> {
    pub fn new() -> Self {
        Monomial { v: BTreeMap::new() }
    }
    pub fn exponent(&self, x: &T) -> usize {
        *self.v.get(x).unwrap_or(&0)
    }
    pub fn set_exponent(&mut self, x: T, k: usize) {
        if k != 0 {
            self.v.insert(x, k);
        } else {
            self.v.remove(&x);
        }
    }
    pub fn degree(&self) -> usize {
        self.v.iter().map(|x| *x.1).sum()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&T, &usize)> + '_ {
        self.v.iter()
    }
    pub fn into_iter(self) -> impl IntoIterator<Item = (T, usize)> {
        self.v.into_iter()
    }
}
impl<T: Ord + Clone + Hash> Mul for Monomial<T> {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        let mut result = self.clone();
        other
            .v
            .iter()
            .for_each(|(x, k)| result.set_exponent(x.clone(), result.exponent(x) + k));
        result
    }
}
impl<T: Ord + Clone + Hash + Display> Display for Monomial<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let to_str = |v: &T, e: &usize| match (v, e) {
            (v, &1) => format!("{}", v),
            _ => format!("{}^{}", v, e),
        };
        f.write_str(
            &self
                .v
                .iter()
                .map(|(v, e)| to_str(v, e)) //|(v, e)|
                .collect::<Vec<_>>()
                .join(" "),
        )
    }
}
