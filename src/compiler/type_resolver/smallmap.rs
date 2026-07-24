use std::collections::HashMap;
use std::hash::Hash;
use std::mem::swap;
use std::ops::Index;

#[derive(Clone)]
pub enum SmallMap<K, V> {
    Normal(HashMap<K, V>),
    Small {
        keys: Vec<K>,
        values: Vec<V>,
    },
}

impl<K, V> Default for SmallMap<K, V> {
    fn default() -> Self {
        Self::Small {
            keys: Vec::new(),
            values: Vec::new(),
        }
    }
}

impl<K: Eq + Clone + Hash, V: Clone> SmallMap<K, V> {
    pub fn is_empty(&self) -> bool {
        match self {
            Self::Normal(map) => map.is_empty(),
            Self::Small { keys, .. } => keys.is_empty(),
        }
    }

    pub fn values(&self) -> Vec<V> {
        match self {
            Self::Normal(map) => map.values().map(V::clone).collect::<Vec<_>>().clone(),
            Self::Small { values, .. } => values.clone(),
        }
    }
  
    pub fn insert(&mut self, key: K, val: V) {
        match self {
            Self::Normal(map) => {
                map.insert(key, val);
            }
            Self::Small { keys, values } => {
                keys.push(key);
                values.push(val);
            }
        }
        const SWTICH_THRESHOLD: usize = 50;
        if let Self::Small { keys, values } = self && keys.len() >= SWTICH_THRESHOLD {
            let mut new_map = HashMap::new();
            new_map.reserve(keys.len());
            for (key, val) in keys.iter().zip(values) {
                new_map.insert(key.clone(), val.clone());
            }
            swap(&mut SmallMap::Normal(new_map), self);
        }
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        match self {
            Self::Normal(map) => map.get(key),
            Self::Small { keys, values } =>
                keys.iter().position(|x| x == key).map(|x| &values[x])
        }
    }

    pub fn to_vec(&self) -> Vec<(K, V)> {
        match self {
            Self::Normal(map) =>
                map.keys().zip(map.values()).map(|(x, y)| (x.clone(), y.clone())).collect(),
            Self::Small { keys, values } =>
                keys.iter().zip(values.iter()).map(|(x, y)| (x.clone(), y.clone())).collect(),
        }
    }

    pub fn clear(&mut self) {
        match self {
            Self::Normal(map) => map.clear(),
            Self::Small { keys, values } => {
                keys.clear();
                values.clear();
            }
        }
    }

    pub fn remove(&mut self, key: &K) {
        match self {
            Self::Normal(map) => {
                map.remove(key);
            }
            Self::Small { keys, values } => {
                let Some(idx) = keys.iter().position(|x| x == key) else { return };
                keys.remove(idx);
                values.remove(idx);
            }
        }
    }
}

impl<K: Eq + Hash + Clone, V: Clone> Index<&K> for SmallMap<K, V> {
    type Output = V;

    fn index(&self, index: &K) -> &Self::Output {
        match self {
            Self::Normal(map) => &map[index],
            Self::Small { .. } => self.get(index).unwrap(),
        }
    }
}
