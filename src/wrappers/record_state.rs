use std::{
    fmt::Debug,
    iter::Map,
    ops::Deref,
    sync::{Arc, Mutex},
};

use hi_set::bag::HIBag;

use crate::SearchState;

#[derive(Clone)]
pub struct RecordState<'a, State> {
    states: Arc<Mutex<&'a mut HIBag<State>>>,
    state: State,
}

impl<'a, State> Debug for RecordState<'a, State>
where
    State: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.state)
    }
}

impl<'a, State> RecordState<'a, State> {
    pub fn new(s: State, set: &'a mut HIBag<State>) -> RecordState<'a, State> {
        RecordState {
            states: Arc::new(Mutex::new(set)),
            state: s,
        }
    }
}

impl<'a, State> Deref for RecordState<'a, State> {
    type Target = State;

    fn deref(&self) -> &Self::Target {
        &self.state
    }
}

impl<'a, State, SubIter> SearchState for RecordState<'a, State>
where
    State: Eq + 'static + SearchState<Iter = SubIter> + Clone + Ord,
    SubIter: Iterator<Item = State>,
{
    type Iter = Map<State::Iter, Box<dyn Fn(State) -> RecordState<'a, State> + 'a>>;

    fn get_transitions(self) -> Self::Iter {
        let states = Arc::clone(&self.states);
        self.state.get_transitions().map(Box::new(move |s| {
            states.lock().unwrap().insert(s.clone());
            RecordState {
                states: Arc::clone(&states),
                state: s,
            }
        }))
    }
}
