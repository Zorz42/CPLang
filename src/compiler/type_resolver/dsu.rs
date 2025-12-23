use crate::compiler::type_resolver::dsu::NodeType::HasParent;
use std::mem::swap;
use std::ops::Add;

#[derive(Clone)]
enum NodeType {
    HasParent(usize),
    Root(usize),
}

#[derive(Clone)]
pub struct Dsu<T: Add<Output=T> + Default> {
    parent: Vec<NodeType>,
    value: Vec<T>,
}

impl<T: Add<Output=T> + Default> Dsu<T> {
    pub const fn new() -> Self {
        Self {
            parent: Vec::new(),
            value: Vec::new(),
        }
    }

    pub const fn len(&self) -> usize {
        self.parent.len()
    }

    pub fn add(&mut self) {
        self.parent.push(NodeType::Root(1));
        self.value.push(T::default());
    }

    pub fn get_repr(&mut self, a: usize) -> usize {
        match self.parent[a] {
            HasParent(par) => {
                let res = self.get_repr(par);
                self.parent[a] = HasParent(res);
                res
            }
            NodeType::Root(_) => a,
        }
    }

    pub fn get(&mut self, a: usize) -> &mut T {
        let a = self.get_repr(a);
        &mut self.value[a]
    }

    pub fn merge(&mut self, a: usize, b: usize) -> bool {
        let mut a = self.get_repr(a);
        let mut b = self.get_repr(b);
        if a == b {
            return false;
        }
        let mut a_size = match self.parent[a] {
            HasParent(_) => unreachable!(),
            NodeType::Root(x) => x,
        };

        let mut b_size = match self.parent[b] {
            HasParent(_) => unreachable!(),
            NodeType::Root(x) => x,
        };

        if a_size < b_size {
            swap(&mut a, &mut b);
            swap(&mut a_size, &mut b_size);
        }

        match &mut self.parent[a] {
            HasParent(_) => unreachable!(),
            NodeType::Root(x) => *x += b_size,
        }

        self.parent[b] = HasParent(a);

        let mut a_val = T::default();
        let mut b_val = T::default();
        swap(&mut self.value[a], &mut a_val);
        swap(&mut self.value[b], &mut b_val);
        self.value[a] = a_val + b_val;

        true
    }
}

impl<T: Add<Output=T> + Default> Default for Dsu<T> {
    fn default() -> Self {
        Self::new()
    }
}
