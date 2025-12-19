use crate::compiler::normalizer::ir::IRTypeLabel;
use crate::compiler::type_resolver::dsu::Dsu;
use crate::compiler::type_resolver::{Conn, TypeResolver};
use std::collections::HashMap;
use std::ops::Add;

fn compactify<T: Add<Output=T> + Default>(
    set1: &Vec<IRTypeLabel>,
    set2: &Vec<IRTypeLabel>,
    dsu: &mut Dsu<T>,
) -> Option<(HashMap<IRTypeLabel, usize>, HashMap<IRTypeLabel, usize>, Vec<usize>, Vec<usize>)> {
    let mut mp1 = HashMap::new();
    let mut mp2 = HashMap::new();
    let mut comps1 = Vec::new();
    let mut comps2 = Vec::new();
    let mut curr_comp = 0;

    for (i1, i2) in set1.iter().zip(set2) {
        let i1 = dsu.get_repr(*i1);
        let i2 = dsu.get_repr(*i2);
        if !mp1.contains_key(&i1) && !mp2.contains_key(&i2) {
            mp1.insert(i1, curr_comp);
            mp2.insert(i2, curr_comp);
            comps1.push(curr_comp);
            comps2.push(curr_comp);
            curr_comp += 1;
        } else {
            if !mp1.contains_key(&i1) || !mp2.contains_key(&i2) {
                return None;
            }
            if mp1[&i1] != mp2[&i2] {
                return None;
            }
        }
    }
    Some((mp1, mp2, comps1, comps2))
}

impl TypeResolver {
    // lets define P(s) as the set of all theoretically possible arrays of types for some array of type labels s
    // if this function returns true P(set1) == P(set2) definitely holds, else P(set1) == P(set2) might hold or not.
    pub fn compare_sets(&mut self, set1: Vec<IRTypeLabel>, set2: Vec<IRTypeLabel>) -> bool {
        if set1.len() != set2.len() {
            return false;
        }

        // identifies elements with their representatives (and gives indices to components) and checks that they all match structurally
        let Some((mp1, mp2, comps1, comps2)) = compactify(&set1, &set2, &mut self.dsu) else {
            return false;
        };
        // mp maps from some element to its component index
        // comps maps from component index to its representative
        let num_comps = comps1.len();
        for i in 0..num_comps {
            let data1 = self.dsu.get(comps1[i]).clone();
            let data2 = self.dsu.get(comps2[i]).clone();

            if data1.ref_depth != data2.ref_depth {
                return false;
            }

            if data1.neighbours.len() != data2.neighbours.len() {
                return false;
            }

            for (conn1, conn2) in data1.neighbours.into_iter().zip(data2.neighbours) {
                match (conn1, conn2) {
                    (Conn::Operator(..), Conn::Operator(..)) => unreachable!(),
                    (
                        Conn::Struct(label1, struct_label1, fields1),
                        Conn::Struct(label2, struct_label2, fields2),
                    ) => {
                        todo!()
                    }
                    (Conn::IsField(..), Conn::IsField(..)) => unreachable!(),
                    (_, _) => return false,
                }
            }
        }

        true
    }
}

