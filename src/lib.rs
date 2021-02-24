use std::{
    collections::{HashSet, VecDeque},
    hash::Hash,
};

pub trait SearchState: Sized {
    fn get_transitions(&self) -> HashSet<Self>;
}

pub struct BreathFirstTester<S> {
    to_search: VecDeque<S>,
    already_searched: HashSet<S>,
    invariants: Vec<fn(&S) -> bool>,
    end_condition: fn(&S) -> bool,
}

impl<S> BreathFirstTester<S> {
    pub fn new(
        start: S,
        invariants: Vec<fn(&S) -> bool>,
        end_condition: fn(&S) -> bool,
    ) -> BreathFirstTester<S>
    where
        S: Eq + Hash,
    {
        let mut to_search = VecDeque::new();
        to_search.push_back(start);
        BreathFirstTester {
            to_search,
            already_searched: HashSet::new(),
            invariants,
            end_condition,
        }
    }

    pub fn search(&mut self) -> Result<S, S>
    where
        S: Eq + Hash + SearchState,
    {
        while let Some(state) = self.to_search.pop_front() {
            // Check the invariants.
            if !self.invariants.iter().all(|inv| inv(&state)) {
                return Err(state);
            }

            // Check the end condition.
            if (self.end_condition)(&state) {
                return Ok(state);
            }

            // Add all of the states after each transition to the search list if they are not already known.
            for next_state in state.get_transitions() {
                if !self.to_search.contains(&next_state)
                    && !self.already_searched.contains(&next_state)
                    && next_state != state
                {
                    self.to_search.push_back(next_state);
                }
            }

            self.already_searched.insert(state);
        }

        unimplemented!("Exhaustive searches are not implemented yet.")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn first_state_breaks() {
        #[derive(Hash, Eq, PartialEq, Debug)]
        struct State();

        impl SearchState for State {
            fn get_transitions(&self) -> HashSet<Self> {
                HashSet::new()
            }
        }

        let mut tester = BreathFirstTester::new(State(), vec![|_s| false], |_s| false);

        assert_eq!(Err(State()), tester.search());
    }

    #[test]
    fn first_state_ends() {
        #[derive(Hash, Eq, PartialEq, Debug)]
        struct State();

        impl SearchState for State {
            fn get_transitions(&self) -> HashSet<Self> {
                HashSet::new()
            }
        }

        let mut tester = BreathFirstTester::new(State(), Vec::new(), |_s| true);

        assert_eq!(Ok(State()), tester.search());
    }

    #[test]
    fn chain_to_end() {
        #[derive(Hash, Eq, PartialEq, Debug)]
        struct State(usize);

        impl SearchState for State {
            fn get_transitions(&self) -> HashSet<Self> {
                let mut out = HashSet::new();
                out.insert(State(self.0 + 1));
                out
            }
        }

        let mut tester = BreathFirstTester::new(State(0), Vec::new(), |s| s == &State(10));

        assert_eq!(Ok(State(10)), tester.search());
    }

    #[test]
    fn chain_to_brake() {
        #[derive(Hash, Eq, PartialEq, Debug)]
        struct State(usize);

        impl SearchState for State {
            fn get_transitions(&self) -> HashSet<Self> {
                let mut out = HashSet::new();
                out.insert(State(self.0 + 1));
                out
            }
        }

        let mut tester = BreathFirstTester::new(State(0), vec![|s| s.0 != 10], |_s| false);

        assert_eq!(Err(State(10)), tester.search());
    }

    #[test]
    fn tree_to_end() {
        #[derive(Hash, Eq, PartialEq, Debug)]
        struct State(usize, usize);

        impl SearchState for State {
            fn get_transitions(&self) -> HashSet<Self> {
                let mut out = HashSet::new();
                out.insert(State(self.0 + 1, self.1));
                out.insert(State(self.0, self.1 + 1));
                out
            }
        }

        let mut tester = BreathFirstTester::new(State(0, 0), Vec::new(), |s| s == &State(10, 5));

        assert_eq!(Ok(State(10, 5)), tester.search());
    }

    #[test]
    fn tree_to_break() {
        #[derive(Hash, Eq, PartialEq, Debug)]
        struct State(usize, usize);

        impl SearchState for State {
            fn get_transitions(&self) -> HashSet<Self> {
                let mut out = HashSet::new();
                out.insert(State(self.0 + 1, self.1));
                out.insert(State(self.0, self.1 + 1));
                out
            }
        }

        let mut tester = BreathFirstTester::new(State(0, 0), vec![|s| s != &State(10, 5)], |s| {
            s.0 > 20 || s.1 > 20
        });

        assert_eq!(Err(State(10, 5)), tester.search());
    }

    #[test]
    fn multiple_invariants() {
        #[derive(Hash, Eq, PartialEq, Debug)]
        struct State();

        impl SearchState for State {
            fn get_transitions(&self) -> HashSet<Self> {
                HashSet::new()
            }
        }

        let mut tester = BreathFirstTester::new(State(), vec![|_s| true, |_s| false], |_s| false);

        assert_eq!(Err(State()), tester.search());
    }
}
