use crate::compiler::normalizer::ir::{IRStructLabel, IRTypeLabel};
use crate::compiler::type_resolver::dsu::Dsu;
use crate::compiler::type_resolver::TypeResolver;
use std::collections::HashMap;
use std::ops::Add;

fn compactify<T: Add<Output=T> + Default>(
    set1: &[IRTypeLabel],
    set2: &[IRTypeLabel],
    dsu: &mut Dsu<T>,
) -> Option<(Vec<usize>, Vec<usize>)> {
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
            comps1.push(i1);
            comps2.push(i2);
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
    Some((comps1, comps2))
}

impl TypeResolver {
    fn get_type_struct(&mut self, typ: IRTypeLabel) -> Option<(IRStructLabel, Vec<IRTypeLabel>)> {
        if let Some(struct_label) = self.type_dsu.get(typ).known_struct {
            let fields = self.structs_ord[struct_label].clone();

            let mut field_types = Vec::new();

            for field in fields {
                let typ = self.type_dsu.get(typ).child_fields[&field];
                field_types.push(typ);
            }

            Some((struct_label, field_types))
        } else {
            None
        }
    }

    // lets define P(s) as the set of all theoretically possible arrays of types for some array of type labels s
    // if this function returns true P(set1) == P(set2) definitely holds, else P(set1) == P(set2) might hold or not.
    pub fn compare_sets(&mut self, set1: Vec<IRTypeLabel>, set2: Vec<IRTypeLabel>) -> bool {
        if set1.len() != set2.len() {
            return false;
        }

        let Some((comps1, comps2)) = compactify(&set1, &set2, &mut self.ref_dsu) else {
            return false;
        };
        let num_comps = comps1.len();
        for i in 0..num_comps {
            if self.ref_is_fixed(comps1[i]) != self.ref_is_fixed(comps2[i]) {
                return false;
            }
        }

        let Some((comps1, comps2)) = compactify(&set1, &set2, &mut self.type_dsu) else {
            return false;
        };
        let num_comps = comps1.len();
        for i in 0..num_comps {
            if self.type_dsu.get(comps1[i]).typ.clone() != self.type_dsu.get(comps2[i]).typ {
                return false;
            }
        }

        // identifies elements with their representatives (and gives indices to components) and checks that they all match structurally
        let Some((comps1, comps2)) = compactify(&set1, &set2, &mut self.dsu) else {
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

            let struct1 = self.get_type_struct(comps1[i]);
            let struct2 = self.get_type_struct(comps2[i]);

            if struct1.is_some() != struct2.is_some() {
                return false;
            }

            if let Some(struct1) = struct1 && let Some(struct2) = struct2 && !self.compare_sets(struct1.1, struct2.1) {
                return false;
            }
        }

        true
    }
}

