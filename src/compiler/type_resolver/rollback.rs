// Rollback only works for a single save and then single revert
// you cannot save twice and then revert twice

use std::collections::{HashMap, VecDeque};

pub trait Rollback {
    type SaveState;
    fn save_state(&mut self) -> Self::SaveState;
    fn restore_state(&mut self, state: Self::SaveState);
}

#[derive(Clone, Default)]
pub struct RollbackVec<T: Rollback> {
    vec: Vec<T>,
    // when doing the rollback, keep track of with
    // indexes got changed and their previous state
    prev_state: Vec<Option<T::SaveState>>,
    changed_list: Vec<u32>,
    rollback_mode: bool,
}

impl<T: Rollback> RollbackVec<T> {
    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn iter(&self) -> &Vec<T> {
        &self.vec
    }

    pub fn push(&mut self, x: T) {
        self.vec.push(x);
        if !self.rollback_mode {
            self.prev_state.push(None);
        }
    }

    pub fn reserve(&mut self, n: usize) {
        self.vec.reserve(n);
        self.prev_state.reserve(n);
    }
}

impl<T: Rollback> Rollback for RollbackVec<T> {
    type SaveState = ();

    fn save_state(&mut self) -> Self::SaveState {
        todo!()
    }

    fn restore_state(&mut self, state: Self::SaveState) {
        todo!()
    }
}

impl<T: Clone> Rollback for Vec<T> {
    type SaveState = Vec<T>;

    fn save_state(&mut self) -> Self::SaveState {
        self.clone()
    }

    fn restore_state(&mut self, state: Self::SaveState) {
        *self = state
    }
}

impl<K: Clone, V: Clone> Rollback for HashMap<K, V> {
    type SaveState = HashMap<K, V>;

    fn save_state(&mut self) -> Self::SaveState {
        self.clone()
    }

    fn restore_state(&mut self, state: Self::SaveState) {
        *self = state
    }
}

impl<T: Clone> Rollback for VecDeque<T> {
    type SaveState = VecDeque<T>;

    fn save_state(&mut self) -> Self::SaveState {
        self.clone()
    }

    fn restore_state(&mut self, state: Self::SaveState) {
        *self = state
    }
}

impl<T1: Rollback, T2: Rollback> Rollback for (T1, T2) {
    type SaveState = (T1::SaveState, T2::SaveState);

    fn save_state(&mut self) -> Self::SaveState {
        (self.0.save_state(), self.1.save_state())
    }

    fn restore_state(&mut self, state: Self::SaveState) {
        let (state1, state2) = state;
        self.0.restore_state(state1);
        self.1.restore_state(state2);
    }
}

impl<T: Clone> Rollback for Option<T> {
    type SaveState = Option<T>;

    fn save_state(&mut self) -> Self::SaveState {
        self.clone()
    }

    fn restore_state(&mut self, state: Self::SaveState) {
        *self = state;
    }
}

macro_rules! impl_rollback_copy {
    ($t:ty) => {
        impl Rollback for $t
        where
            $t: Copy,
        {
            type SaveState = Self;

            fn save_state(&mut self) -> Self::SaveState {
                *self
            }

            fn restore_state(&mut self, state: Self::SaveState) {
                *self = state;
            }
        }
    };
}

impl_rollback_copy!(i8);
impl_rollback_copy!(i16);
impl_rollback_copy!(i32);
impl_rollback_copy!(i64);
impl_rollback_copy!(u8);
impl_rollback_copy!(u16);
impl_rollback_copy!(u32);
impl_rollback_copy!(u64);
impl_rollback_copy!(char);
impl_rollback_copy!(usize);
impl_rollback_copy!(bool);
