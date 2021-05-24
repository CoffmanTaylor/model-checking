use std::{
    fmt::Debug,
    iter::Map,
    ops::{Add, AddAssign, Deref},
    sync::{Arc, Mutex},
};

use crate::SearchState;

#[derive(Debug, Clone)]
pub struct DebugState<State> {
    inner_state: Option<State>,
    from: usize,
    this: usize,
    last: Arc<Mutex<usize>>,
}

impl<State> DebugState<State> {
    pub fn new(inner_state: State) -> Self
    where
        State: Debug,
    {
        DebugState {
            inner_state: Some(inner_state),
            from: 0,
            this: 0,
            last: Arc::new(Mutex::new(0)),
        }
    }
}

impl<State> Deref for DebugState<State> {
    type Target = State;

    fn deref(&self) -> &Self::Target {
        match &self.inner_state {
            Some(s) => s,
            None => unreachable!(), // The only time we have a none is when we call get_transitions which takes self.
        }
    }
}

impl<State> SearchState for DebugState<State>
where
    State: SearchState + Debug + 'static,
{
    type Iter = Map<State::Iter, Box<dyn Fn(State) -> DebugState<State>>>;  // TODO: Replace with impl ~~~ once it is stabilized.

    fn get_transitions(mut self) -> Self::Iter {
        self.inner_state
            .take()
            .unwrap()
            .get_transitions()
            .map(Box::new(move |s| {
                let mut last = self.last.lock().unwrap();
                let this = last.add(1);
                last.add_assign(1);

                println!("Searching {} -> {}: {:?}", self.this, this, &s);
                DebugState {
                    inner_state: Some(s),
                    from: self.this,
                    this,
                    last: Arc::clone(&self.last),
                }
            }))
    }

    fn clear_shared_state(&mut self) {
        self.last = Arc::new(Mutex::new(0));
        self.this = 0;
        self.from = 0;
    }

    fn merge_shared_state(&mut self, other: &mut Self) {
        let mut last = self.last.lock().unwrap();
        let other_this = last.add(1);
        last.add_assign(1);

        other.from = 0;
        other.this = other_this;
        other.last = Arc::clone(&self.last);
    }
}
