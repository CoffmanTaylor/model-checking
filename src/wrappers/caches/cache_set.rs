use std::{
    collections::HashSet,
    fmt::Debug,
    hash::Hash,
    iter::FilterMap,
    ops::{Deref, DerefMut},
    sync::{Arc, Mutex},
};

use crate::SearchState;

#[derive(Clone)]
pub struct CacheSet<State> {
    already_searched: Arc<Mutex<HashSet<State>>>,
    state: State,
}

impl<State> Debug for CacheSet<State>
where
    State: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.state)
    }
}

impl<State> CacheSet<State> {
    pub fn new(s: State) -> CacheSet<State> {
        CacheSet {
            already_searched: Arc::new(Mutex::new(HashSet::new())),
            state: s,
        }
    }
}

impl<State> Deref for CacheSet<State> {
    type Target = State;

    fn deref(&self) -> &Self::Target {
        &self.state
    }
}

impl<State> DerefMut for CacheSet<State>
where
    State: Clone,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.state
    }
}

impl<State, SubIter> SearchState for CacheSet<State>
where
    State: Eq + Hash + 'static + SearchState<Iter = SubIter> + Clone,
    SubIter: Iterator<Item = State>,
{
    type Iter = FilterMap<SubIter, Box<dyn Fn(State) -> Option<CacheSet<State>>>>;

    fn get_transitions(self) -> Self::Iter {
        let already_searched = self.already_searched;
        self.state.get_transitions().filter_map(Box::new(move |s| {
            if already_searched.lock().unwrap().insert(s.clone()) {
                Some(CacheSet {
                    already_searched: Arc::clone(&already_searched),
                    state: s,
                })
            } else {
                None
            }
        }))
    }

    fn clear_shared_state(&mut self) {
        self.already_searched = Arc::new(Mutex::new(HashSet::new()));
    }

    fn merge_shared_state(&mut self, other: &mut Self) {
        other.already_searched = Arc::clone(&self.already_searched);
    }
}
