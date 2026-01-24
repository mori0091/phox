use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;

pub trait VarId:
    Copy + Clone + Eq + Ord + Hash + Debug
{
    fn from_usize(u: usize) -> Self;
    fn to_usize(self) -> usize;
}

// ===== Context: common part of var-gen + union-find + binding =====
#[derive(Clone)]
pub struct Context<Id: VarId, T> {
    pub parent: Vec<Id>,         // union-find parent pointers
    pub binding: Vec<Option<T>>, // representative binding (Some if bound to a type)
    pub non_touchable: HashSet<Id>,
}

impl <Id: VarId, T: Clone> Context<Id, T> {
    pub fn new() -> Self {
        Self {
            parent: Vec::new(),
            binding: Vec::new(),
            non_touchable: HashSet::new(),
        }
    }

    pub fn set_non_touchable(&mut self, non_touchable: &HashSet<Id>) {
        self.non_touchable.extend(non_touchable);
    }

    pub fn clear_non_touchable(&mut self) {
        self.non_touchable.clear();
    }

    pub fn fresh_var_id(&mut self) -> Id {
        let id = Id::from_usize(self.parent.len());
        self.parent.push(id);
        self.binding.push(None);
        id
    }

    // find with path compression
    pub fn find(&mut self, id: Id) -> Id {
        let idx = id.to_usize();
        let p = self.parent[idx];
        if p != id {
            let root = self.find(p);
            self.parent[idx] = root;
        }
        self.parent[idx]
    }

    pub fn get_bound(&mut self, v: &Id) -> Option<T> {
        let r = self.find(*v);
        self.binding[r.to_usize()].clone()
    }
}
