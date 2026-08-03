use crate::compiler::type_resolver::rollback::{Rollback, RollbackVec};
use call_counter_derive::count_calls;
use rollback_derive::Rollback;
use std::mem::swap;
use std::ops::AddAssign;

#[derive(Rollback)]
pub struct Dsu<T: AddAssign + Default + Rollback> {
    // parent = -x: is root with size x
    // parent = x: has parent x
    parent: RollbackVec<i32>,
    value: RollbackVec<T>,
}

impl<T: AddAssign + Default + Rollback> Dsu<T> {
    pub const fn new() -> Self {
        Self {
            parent: RollbackVec::new(),
            value: RollbackVec::new(),
        }
    }

    pub const fn len(&self) -> usize {
        self.parent.len()
    }

    pub fn add(&mut self) {
        self.parent.push(-1);
        self.value.push(T::default());
    }

    #[count_calls]
    pub fn get_repr(&mut self, mut a: usize) -> usize {
        while let x = unsafe { *self.parent.get_unchecked(a) }
            && x >= 0
        {
            a = x as usize;
        }
        a
    }

    pub fn get(&mut self, a: usize) -> &mut T {
        let a = self.get_repr(a);
        self.value.get_mut(a)
    }

    // returns true if a was merged into b and false if b was merged into a
    // returns None if nothing was merged
    pub fn merge(&mut self, a: usize, b: usize) -> Option<bool> {
        let mut a = self.get_repr(a);
        let mut b = self.get_repr(b);
        if a == b {
            return None;
        }
        let mut a_size = -self.parent.get(a);
        let mut b_size = -self.parent.get(b);
        let mut res = false;

        if a_size < b_size {
            res = true;
            swap(&mut a, &mut b);
            swap(&mut a_size, &mut b_size);
        }

        *self.parent.get_mut(a) -= b_size;
        *self.parent.get_mut(b) = a as i32;

        let mut b_val = T::default();
        swap(self.value.get_mut(b), &mut b_val);
        *self.value.get_mut(a) += b_val;

        Some(res)
    }
}

impl<T: AddAssign + Default + Rollback> Default for Dsu<T> {
    fn default() -> Self {
        Self::new()
    }
}
