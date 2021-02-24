use std::{
    collections::{HashSet, VecDeque},
    hash::Hash,
};

pub trait SearchState: Sized {
    fn get_transitions(&self) -> HashSet<Self>;
}

pub struct SearchConfig<'a, S> {
    start_states: VecDeque<S>,
    invariants: Vec<(fn(&S) -> bool, &'a str)>,
}

impl<'a, S> SearchConfig<'a, S> {
    pub fn new(start_state: S) -> SearchConfig<'a, S>
    where
        S: Eq + Hash,
    {
        let mut start_states = VecDeque::new();
        start_states.push_back(start_state);
        SearchConfig {
            start_states,
            invariants: Vec::new(),
        }
    }

    pub fn add_invariant(mut self, name: &'a str, inv: fn(&S) -> bool) -> Self {
        self.invariants.push((inv, name));
        self
    }

    pub fn add_invariants<I>(mut self, invs: I) -> Self
    where
        I: IntoIterator<Item = (fn(&S) -> bool, &'a str)>,
    {
        self.invariants.extend(invs);
        self
    }

    pub fn search_bfs(&self, end_condition: fn(&S) -> bool) -> Result<S, (S, &'a str)>
    where
        S: Eq + Hash + SearchState + Clone,
    {
        let mut to_search = self.start_states.clone();
        let mut already_searched = HashSet::new();

        while let Some(state) = to_search.pop_front() {
            // Check the invariants.
            for (inv, name) in self.invariants.iter() {
                if !inv(&state) {
                    return Err((state, name));
                }
            }

            // Check the end condition.
            if end_condition(&state) {
                return Ok(state);
            }

            // Add all of the states after each transition to the search list if they are not already known.
            for next_state in state.get_transitions() {
                if !to_search.contains(&next_state)
                    && !already_searched.contains(&next_state)
                    && next_state != state
                {
                    to_search.push_back(next_state);
                }
            }

            already_searched.insert(state);
        }

        unimplemented!("Exhaustive searches are not implemented yet.")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn first_state_breaks() {
        #[derive(Hash, Eq, PartialEq, Debug, Clone)]
        struct State();

        impl SearchState for State {
            fn get_transitions(&self) -> HashSet<Self> {
                HashSet::new()
            }
        }

        let tester = SearchConfig::new(State()).add_invariant("test", |_| false);

        assert_eq!(Err((State(), "test")), tester.search_bfs(|_| false));
    }

    #[test]
    fn first_state_ends() {
        #[derive(Hash, Eq, PartialEq, Debug, Clone)]
        struct State();

        impl SearchState for State {
            fn get_transitions(&self) -> HashSet<Self> {
                HashSet::new()
            }
        }

        let tester = SearchConfig::new(State());

        assert_eq!(Ok(State()), tester.search_bfs(|_| true));
    }

    #[test]
    fn chain_to_end() {
        #[derive(Hash, Eq, PartialEq, Debug, Clone)]
        struct State(usize);

        impl SearchState for State {
            fn get_transitions(&self) -> HashSet<Self> {
                let mut out = HashSet::new();
                out.insert(State(self.0 + 1));
                out
            }
        }

        let tester = SearchConfig::new(State(0));

        assert_eq!(Ok(State(10)), tester.search_bfs(|s| s == &State(10)));
    }

    #[test]
    fn chain_to_brake() {
        #[derive(Hash, Eq, PartialEq, Debug, Clone)]
        struct State(usize);

        impl SearchState for State {
            fn get_transitions(&self) -> HashSet<Self> {
                let mut out = HashSet::new();
                out.insert(State(self.0 + 1));
                out
            }
        }

        let tester = SearchConfig::new(State(0)).add_invariant("test", |s| s.0 != 10);

        assert_eq!(Err((State(10), "test")), tester.search_bfs(|_| false));
    }

    #[test]
    fn tree_to_end() {
        #[derive(Hash, Eq, PartialEq, Debug, Clone)]
        struct State(usize, usize);

        impl SearchState for State {
            fn get_transitions(&self) -> HashSet<Self> {
                let mut out = HashSet::new();
                out.insert(State(self.0 + 1, self.1));
                out.insert(State(self.0, self.1 + 1));
                out
            }
        }

        let tester = SearchConfig::new(State(0, 0));

        assert_eq!(Ok(State(10, 5)), tester.search_bfs(|s| s == &State(10, 5)));
    }

    #[test]
    fn tree_to_break() {
        #[derive(Hash, Eq, PartialEq, Debug, Clone)]
        struct State(usize, usize);

        impl SearchState for State {
            fn get_transitions(&self) -> HashSet<Self> {
                let mut out = HashSet::new();
                out.insert(State(self.0 + 1, self.1));
                out.insert(State(self.0, self.1 + 1));
                out
            }
        }

        let tester = SearchConfig::new(State(0, 0)).add_invariant("test", |s| s != &State(10, 5));

        assert_eq!(Err((State(10, 5), "test")), tester.search_bfs(|_| false));
    }

    #[test]
    fn multiple_invariants() {
        #[derive(Hash, Eq, PartialEq, Debug, Clone)]
        struct State();

        impl SearchState for State {
            fn get_transitions(&self) -> HashSet<Self> {
                HashSet::new()
            }
        }

        let tester = SearchConfig::new(State())
            .add_invariant("test 1", |_| true)
            .add_invariant("test 2", |_| false);

        assert_eq!(Err((State(), "test 2")), tester.search_bfs(|_| false));
    }
}
