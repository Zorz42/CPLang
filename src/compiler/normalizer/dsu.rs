use crate::compiler::normalizer::dsu::NodeType::HasParent;
use std::mem::swap;

enum NodeType {
    HasParent(usize),
    Root(usize),
}

pub struct Dsu {
    parent: Vec<NodeType>,
}

impl Dsu {
    pub fn new() -> Self {
        Self {
            parent: Vec::new(),
        }
    }

    pub fn add(&mut self) {
        self.parent.push(NodeType::Root(1));
    }

    pub fn get(&mut self, a: usize) -> usize {
        match self.parent[a] {
            HasParent(par) => {
                let res = self.get(par);
                self.parent[a] = HasParent(res);
                res
            }
            NodeType::Root(_) => a,
        }
    }

    pub fn merge(&mut self, a: usize, b: usize) -> bool {
        let mut a = self.get(a);
        let mut b = self.get(b);
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

        true
    }
}