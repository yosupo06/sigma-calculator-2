use core::fmt;
use std::{
    cell::Cell,
    cmp::Ordering,
    fmt::{Debug, Display, Formatter},
    hash::{Hash, Hasher},
};

use std::ptr;

#[derive(Clone)]
pub struct Variable<'e> {
    gen: &'e VariableManager,
    pub id: usize,
}
impl<'e> Variable<'e> {
    pub fn name(&self) -> String {
        let names = self.gen.names.take();
        let n = names[self.id].clone();
        self.gen.names.set(names);
        return n;
    }
}
impl<'e> Display for Variable<'e> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}_{}", self.name(), self.id)
    }
}
impl<'e> Debug for Variable<'e> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}_{}", self.name(), self.id)
    }
}

impl<'e> PartialEq for Variable<'e> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.gen, other.gen) && self.id == other.id
    }
}
impl<'e> Eq for Variable<'e> {}
impl<'e> Hash for Variable<'e> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        ptr::hash(self.gen, state);
        self.id.hash(state);
    }
}
impl<'e> PartialOrd for Variable<'e> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl<'e> Ord for Variable<'e> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

pub struct VariableManager {
    next_id: Cell<usize>,
    names: Cell<Vec<String>>,
}
impl VariableManager {
    /*    pub fn new() -> VariableManager {
        VariableManager {
            next_id: Cell::new(0),
            names: Cell::new(Vec::new()),
        }
    }*/
    pub fn new_var(&self, name: String) -> Variable {
        let id = self.next_id.get();
        self.next_id.set(id + 1);

        let mut names = self.names.take();
        names.push(name);
        self.names.set(names);

        Variable { gen: self, id }
    }
}
impl Default for VariableManager {
    fn default() -> Self {
        VariableManager {
            next_id: Cell::new(0),
            names: Cell::new(Vec::new()),
        }
    }
}
