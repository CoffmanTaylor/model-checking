use std::{
    collections::HashSet,
    fmt::Debug,
    hash::Hash,
    iter::FilterMap,
    ops::Deref,
    sync::{Arc, Mutex},
};

use crate::SearchState;

#[derive(Clone)]
pub struct CacheSet<State> {
    already_searched: Arc<Mutex<HashSet<Arc<State>>>>,
    state: Arc<State>,
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
            state: Arc::new(s),
        }
    }
}

impl<State> Deref for CacheSet<State> {
    type Target = State;

    fn deref(&self) -> &Self::Target {
        &*self.state
    }
}

impl<State, SubIter> SearchState for CacheSet<State>
where
    State: Eq + Hash + 'static + SearchState<Iter = SubIter>,
    SubIter: Iterator<Item = State>,
{
    type Iter = FilterMap<SubIter, Box<dyn Fn(State) -> Option<CacheSet<State>>>>;

    fn get_transitions(self: Arc<Self>) -> Self::Iter {
        Arc::clone(&self.state)
            .get_transitions()
            .filter_map(Box::new(move |s| {
                let s_arc = Arc::new(s);
                if self
                    .already_searched
                    .lock()
                    .unwrap()
                    .insert(Arc::clone(&s_arc))
                {
                    Some(CacheSet {
                        already_searched: Arc::clone(&self.already_searched),
                        state: s_arc,
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
