use core::fmt;
use std::collections::btree_map::IntoIter;
use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::ops::Mul;

use num::One;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Monomial<K> {
    v: BTreeMap<K, usize>,
}
impl<K> Default for Monomial<K> {
    fn default() -> Self {
        Self {
            v: BTreeMap::default(),
        }
    }
}
impl<K> From<K> for Monomial<K>
where
    K: Ord,
{
    fn from(item: K) -> Self {
        Monomial {
            v: BTreeMap::from([(item, 1)]),
        }
    }
}
impl<K> From<Option<K>> for Monomial<K>
where
    K: Ord,
{
    fn from(item: Option<K>) -> Self {
        if let Some(item) = item {
            Monomial {
                v: BTreeMap::from([(item, 1)]),
            }
        } else {
            Default::default()
        }
    }
}

impl<K> Monomial<K> {
    pub fn iter(&self) -> impl Iterator<Item = (&K, &usize)> + '_ {
        self.v.iter()
    }
    pub fn degree(&self) -> usize {
        self.v.iter().map(|x| *x.1).sum()
    }
}
impl<K> IntoIterator for Monomial<K> {
    type Item = (K, usize);
    type IntoIter = IntoIter<K, usize>;
    fn into_iter(self) -> Self::IntoIter {
        self.v.into_iter()
    }
}
impl<K> Monomial<K>
where
    K: Ord,
{
    pub fn exponent(&self, x: &K) -> usize {
        *self.v.get(x).unwrap_or(&0)
    }
    pub fn set_exponent(&mut self, x: K, k: usize) {
        if k != 0 {
            self.v.insert(x, k);
        } else {
            self.v.remove(&x);
        }
    }
}

impl<K> One for Monomial<K>
where
    K: Ord,
{
    fn one() -> Self {
        Self::default()
    }
    fn is_one(&self) -> bool {
        self.v.is_empty()
    }
    fn set_one(&mut self) {
        self.v.clear()
    }
}
impl<K> Mul for Monomial<K>
where
    K: Ord,
{
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        let mut result = self;
        other.v.into_iter().for_each(|(x, k)| {
            let v = result.exponent(&x) + k;
            result.set_exponent(x, v)
        });
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
