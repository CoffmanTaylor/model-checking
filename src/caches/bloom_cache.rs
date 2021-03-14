use std::{
    fmt::Debug,
    hash::Hash,
    iter::FilterMap,
    ops::Deref,
    sync::{Arc, Mutex},
};

use bloom_filter::BloomFilter;

use crate::SearchState;

#[derive(Clone)]
pub struct BloomCache<State> {
    already_searched: Arc<Mutex<BloomFilter<State>>>,
    state: Arc<State>,
}

impl<State> Debug for BloomCache<State>
where
    State: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.state)
    }
}

impl<State> BloomCache<State> {
    pub fn new(s: State) -> BloomCache<State> {
        BloomCache {
            already_searched: Arc::new(Mutex::new(BloomFilter::new())),
            state: Arc::new(s),
        }
    }
}

impl<State> Deref for BloomCache<State> {
    type Target = State;

    fn deref(&self) -> &Self::Target {
        &*self.state
    }
}

impl<State, SubIter> SearchState for BloomCache<State>
where
    State: Eq + Hash + 'static + SearchState<Iter = SubIter>,
    SubIter: Iterator<Item = State>,
{
    type Iter = FilterMap<SubIter, Box<dyn Fn(State) -> Option<BloomCache<State>>>>;

    fn get_transitions(self: Arc<Self>) -> Self::Iter {
        Arc::clone(&self.state)
            .get_transitions()
            .filter_map(Box::new(move |s| {
                if self.already_searched.lock().unwrap().insert(&s) {
                    Some(BloomCache {
                        already_searched: Arc::clone(&self.already_searched),
                        state: Arc::new(s),
                    })
                } else {
                    None
                }
            }))
    }
}