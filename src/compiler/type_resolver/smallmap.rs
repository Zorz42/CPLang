use std::ops::Index;

#[derive(Default, Clone)]
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

#[derive(Default, Clone)]
pub struct SmallSet<T> {
    values: Vec<T>,
}

impl<T: PartialEq> SmallSet<T> {
    pub fn insert(&mut self, val: T) {
        self.values.push(val);
    }

    pub fn contains(&self, val: &T) -> bool {
        self.values.iter().any(|x| x == val)
    }

    pub fn into_vec(self) -> Vec<T> {
        self.values
    }
}



