use std::mem::swap;
use std::ops::Add;

#[derive(Clone)]
pub struct Dsu<T: Add<Output=T> + Default> {
    // parent = -x: is root with size x
    // parent = x: has parent x
    parent: Vec<i32>,
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
        self.parent.push(-1);
        self.value.push(T::default());
    }

    pub fn get_repr(&mut self, mut a: usize) -> usize {
        while self.parent[a] >= 0 {
            a = self.parent[a] as usize;
        }
        a
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
        let mut a_size = -self.parent[a];
        let mut b_size = -self.parent[b];

        if a_size < b_size {
            swap(&mut a, &mut b);
            swap(&mut a_size, &mut b_size);
        }

        self.parent[a] -= b_size;
        self.parent[b] = a as i32;

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
