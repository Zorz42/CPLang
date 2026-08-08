use crate::compiler::type_resolver::Rollback;
use std::ops::Index;

#[derive(Default, Clone, Rollback)]
pub struct SmallMap<K, V> {
    keys: Vec<K>,
    values: Vec<V>,
}

impl<K: PartialEq + Clone, V: Clone> SmallMap<K, V> {
    pub const fn is_empty(&self) -> bool {
        self.keys.is_empty()
    }

    pub const fn values(&self) -> &Vec<V> {
        &self.values
    }

    pub fn insert(&mut self, key: K, val: V) {
        self.keys.push(key);
        self.values.push(val);
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.keys.iter().position(|x| x == key).map(|x| &self.values[x])
    }

    pub fn to_vec(&self) -> Vec<(K, V)> {
        self.keys.iter().zip(self.values.iter()).map(|(x, y)| (x.clone(), y.clone())).collect()
    }

    pub fn clear(&mut self) {
        self.keys.clear();
        self.values.clear();
    }

    pub fn remove(&mut self, key: &K) {
        let Some(idx) = self.keys.iter().position(|x| x == key) else { return };
        self.keys.remove(idx);
        self.values.remove(idx);
    }
}

impl<K: PartialEq + Clone, V: Clone> Index<&K> for SmallMap<K, V> {
    type Output = V;

    fn index(&self, index: &K) -> &Self::Output {
        self.get(index).unwrap()
    }
}

#[derive(Default)]
pub struct BoundedSet {
    present: Vec<bool>,
    vec: Vec<u32>,
}

impl Rollback for BoundedSet {
    type SaveState = ();

    fn save_state(&mut self) -> Self::SaveState {}

    fn restore_state(&mut self, _state: Self::SaveState) {}
}

impl BoundedSet {
    pub fn insert(&mut self, x: usize) {
        while self.present.len() <= x {
            self.present.push(false);
        }
        if !self.present[x] {
            self.present[x] = true;
            self.vec.push(x as u32);
        }
    }

    pub fn contains(&self, x: usize) -> bool {
        self.present.get(x).is_some_and(|x| *x)
    }

    pub fn clear(&mut self) {
        for i in &self.vec {
            self.present[*i as usize] = false;
        }
        self.vec.clear();
    }

    pub const fn as_vec(&self) -> &Vec<u32> {
        &self.vec
    }
}
