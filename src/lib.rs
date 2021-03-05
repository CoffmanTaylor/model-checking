use std::{
    collections::{HashSet, VecDeque},
    hash::Hash,
    rc::Rc,
};

/// This trait means that the struct this is implemented on can be used to define a specific state
/// of the System. `get_transitions` must be deterministic and idempotent. It is a logical error if
/// two equal `SearchState`s return different results from `get_transitions`.
pub trait SearchState {
    fn get_transitions(self: Rc<Self>) -> Box<dyn Iterator<Item = Self>>;
}

/// Defines the System that you want to search. The System contains at least one start state.
#[derive(Clone)]
pub struct SearchConfig<'a, S> {
    start_states: VecDeque<S>,
    invariants: Vec<(fn(&S) -> bool, &'a str)>,
    prune_conditions: Vec<fn(&S) -> bool>,
}

impl<'a, S> SearchConfig<'a, S> {
    /// Constructs a new `SearchConfig` using the provided state as the only start state.
    pub fn new(start_state: S) -> SearchConfig<'a, S> {
        let mut start_states = VecDeque::new();
        start_states.push_back(start_state);
        SearchConfig {
            start_states,
            invariants: Vec::new(),
            prune_conditions: Vec::new(),
        }
    }

    /// Adds an invariant to the System. Every state discovered during the search, including
    /// the state states, will be checked against every invariant of the System.
    pub fn add_invariant(mut self, name: &'a str, inv: fn(&S) -> bool) -> Self {
        self.invariants.push((inv, name));
        self
    }

    /// Adds a collection of invariants to the System. Every state discovered during the search,
    /// including the state states, will be checked against every invariant of the System.
    pub fn add_invariants<I>(mut self, invs: I) -> Self
    where
        I: IntoIterator<Item = (fn(&S) -> bool, &'a str)>,
    {
        self.invariants.extend(invs);
        self
    }

    /// Adds a pruning condition to the System. When preforming a search, if a state matches
    /// any of the pruning conditions, none of that state's transitions will be searched. This
    /// is helpful for speeding up searches when you know there are states that once you reach,
    /// you know you wont find the end condition afterwards.
    pub fn add_prune_condition(mut self, prune_condition: fn(&S) -> bool) -> Self {
        self.prune_conditions.push(prune_condition);
        self
    }

    /// Preforms a Breath First Search on the System looking for the a state that matches the given
    /// end condition. If a matching state is found, it is returned in `Ok(State)`. If any of the
    /// System's invariants are violated, then the state that failed and the name of the invariant
    /// are returned as `Err((State, name))`.
    pub fn search_bfs(&self, end_condition: fn(&S) -> bool) -> Result<S, (S, &'a str)>
    where
        S: Eq + Hash + SearchState + Clone,
    {
        let mut to_search: VecDeque<(usize, Box<dyn Iterator<Item = S>>)> = VecDeque::new();
        to_search.push_back((0, Box::new(self.start_states.iter().cloned())));

        let mut already_searched = HashSet::new();
        let mut count: usize = 0;

        while let Some(mut states) = to_search.pop_front() {
            while let Some(state) = states.1.next() {
                if already_searched.contains(&state) {
                    continue;
                }

                count += 1;
                if count.is_power_of_two() {
                    println!("Searched {} states, At depth: {}", count, states.0);
                }

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

                // Check if the state should be pruned
                if self.prune_conditions.iter().any(|f| f(&state)) {
                    continue;
                }

                let state_rc = Rc::new(state);
                already_searched.insert(Rc::clone(&state_rc));

                to_search.push_back((states.0 + 1, Rc::clone(&state_rc).get_transitions()));
            }
        }

        unimplemented!("Exhaustive searches are not implemented yet.")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::iter;

    #[test]
    fn first_state_breaks() {
        #[derive(Hash, Eq, PartialEq, Debug, Clone)]
        struct State();

        impl SearchState for State {
            fn get_transitions(self: Rc<Self>) -> Box<dyn Iterator<Item = State>> {
                Box::new(iter::empty())
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
            fn get_transitions(self: Rc<Self>) -> Box<dyn Iterator<Item = State>> {
                Box::new(iter::empty())
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
            fn get_transitions(self: Rc<Self>) -> Box<dyn Iterator<Item = State>> {
                Box::new(iter::once(State(self.0 + 1)))
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
            fn get_transitions(self: Rc<Self>) -> Box<dyn Iterator<Item = State>> {
                Box::new(iter::once(State(self.0 + 1)))
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
            fn get_transitions(self: Rc<Self>) -> Box<dyn Iterator<Item = State>> {
                Box::new(
                    iter::once(State(self.0 + 1, self.1))
                        .chain(iter::once(State(self.0, self.1 + 1))),
                )
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
            fn get_transitions(self: Rc<Self>) -> Box<dyn Iterator<Item = State>> {
                Box::new(
                    iter::once(State(self.0 + 1, self.1))
                        .chain(iter::once(State(self.0, self.1 + 1))),
                )
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
            fn get_transitions(self: Rc<Self>) -> Box<dyn Iterator<Item = State>> {
                Box::new(iter::empty())
            }
        }

        let tester = SearchConfig::new(State())
            .add_invariant("test 1", |_| true)
            .add_invariant("test 2", |_| false);

        assert_eq!(Err((State(), "test 2")), tester.search_bfs(|_| false));
    }

    #[test]
    fn pruning() {
        #[derive(Hash, Eq, PartialEq, Debug, Clone)]
        struct State(usize, usize);

        impl SearchState for State {
            fn get_transitions(self: Rc<Self>) -> Box<dyn Iterator<Item = State>> {
                Box::new(
                    iter::once(State(self.0 + 1, self.1))
                        .chain(iter::once(State(self.0, self.1 + 1))),
                )
            }
        }

        let tester = SearchConfig::new(State(0, 0))
            .add_invariant("test", |s| s.1 != 4)
            .add_prune_condition(|s| s.1 >= 3);

        assert_eq!(Ok(State(10, 0)), tester.search_bfs(|s| s == &State(10, 0)));
    }
}
