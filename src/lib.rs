use std::{
    collections::{HashSet, VecDeque},
    hash::Hash,
    sync::Arc,
};

/// This trait means that the struct this is implemented on can be used to define a specific state
/// of the System. `get_transitions` must be deterministic and idempotent. It is a logical error if
/// two equal `SearchState`s return different results from `get_transitions`.
pub trait SearchState {
    fn get_transitions(self: Arc<Self>) -> Box<dyn Iterator<Item = Self>>;
}

/// The results of preforming a search on a system.
#[derive(Debug, PartialEq, Eq)]
pub enum SearchResults<'a, State> {
    /// The contained State matches the end condition and is reachable from one of the starting states.
    Found(State),
    /// The contained State broke the named invariant.
    BrokenInvariant(State, &'a str),
    /// All reachable States, without going past max depth, do not match the end condition.
    SpaceExhausted,
}

use SearchResults::{BrokenInvariant, Found, SpaceExhausted};

/// Defines the System that you want to search. The System contains at least one start state.
#[derive(Clone)]
pub struct SearchConfig<'a, S> {
    start_states: VecDeque<S>,
    invariants: Vec<(fn(&S) -> bool, &'a str)>,
    prune_conditions: Vec<fn(&S) -> bool>,
    max_depth: Option<usize>,
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
            max_depth: None,
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

    /// Sets the maximum depth any search will go. If the end condition cannot be found within
    /// the states reachable inside the maximum depth, a `SearchResult::SpaceExhausted` will be
    /// returned.
    ///
    /// Default to no maximum depth.
    pub fn set_max_depth(mut self, max_depth: usize) -> Self {
        self.max_depth = Some(max_depth);
        self
    }

    /// Preforms a Breath First Search on the System looking for the a state that matches the given
    /// end condition. If a matching state is found, it is returned in `Found(State)`. If any of the
    /// System's invariants are violated, then the state that failed and the name of the invariant
    /// are returned as `BrokenInvariant(State, name)`.
    pub fn search_bfs(&self, end_condition: fn(&S) -> bool) -> SearchResults<'a, S>
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
                        return BrokenInvariant(state, name);
                    }
                }

                // Check the end condition.
                if end_condition(&state) {
                    return Found(state);
                }

                // Check if the state should be pruned
                if self.prune_conditions.iter().any(|f| f(&state)) {
                    continue;
                }

                // Add this state to the set of searched states.
                let state_rc = Arc::new(state);
                already_searched.insert(Arc::clone(&state_rc));

                // Check if we are at the maximin depth.
                if self.max_depth == Some(states.0) {
                    continue;
                }

                // Add the possible transitions to the search queue.
                to_search.push_back((states.0 + 1, Arc::clone(&state_rc).get_transitions()));
            }
        }

        SpaceExhausted
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
            fn get_transitions(self: Arc<Self>) -> Box<dyn Iterator<Item = State>> {
                Box::new(iter::empty())
            }
        }

        let tester = SearchConfig::new(State()).add_invariant("test", |_| false);

        assert_eq!(
            BrokenInvariant(State(), "test"),
            tester.search_bfs(|_| false)
        );
    }

    #[test]
    fn first_state_ends() {
        #[derive(Hash, Eq, PartialEq, Debug, Clone)]
        struct State();

        impl SearchState for State {
            fn get_transitions(self: Arc<Self>) -> Box<dyn Iterator<Item = State>> {
                Box::new(iter::empty())
            }
        }

        let tester = SearchConfig::new(State());

        assert_eq!(Found(State()), tester.search_bfs(|_| true));
    }

    #[test]
    fn chain_to_end() {
        #[derive(Hash, Eq, PartialEq, Debug, Clone)]
        struct State(usize);

        impl SearchState for State {
            fn get_transitions(self: Arc<Self>) -> Box<dyn Iterator<Item = State>> {
                Box::new(iter::once(State(self.0 + 1)))
            }
        }

        let tester = SearchConfig::new(State(0));

        assert_eq!(Found(State(10)), tester.search_bfs(|s| s == &State(10)));
    }

    #[test]
    fn chain_to_brake() {
        #[derive(Hash, Eq, PartialEq, Debug, Clone)]
        struct State(usize);

        impl SearchState for State {
            fn get_transitions(self: Arc<Self>) -> Box<dyn Iterator<Item = State>> {
                Box::new(iter::once(State(self.0 + 1)))
            }
        }

        let tester = SearchConfig::new(State(0)).add_invariant("test", |s| s.0 != 10);

        assert_eq!(
            BrokenInvariant(State(10), "test"),
            tester.search_bfs(|_| false)
        );
    }

    #[test]
    fn tree_to_end() {
        #[derive(Hash, Eq, PartialEq, Debug, Clone)]
        struct State(usize, usize);

        impl SearchState for State {
            fn get_transitions(self: Arc<Self>) -> Box<dyn Iterator<Item = State>> {
                Box::new(
                    iter::once(State(self.0 + 1, self.1))
                        .chain(iter::once(State(self.0, self.1 + 1))),
                )
            }
        }

        let tester = SearchConfig::new(State(0, 0));

        assert_eq!(
            Found(State(10, 5)),
            tester.search_bfs(|s| s == &State(10, 5))
        );
    }

    #[test]
    fn tree_to_break() {
        #[derive(Hash, Eq, PartialEq, Debug, Clone)]
        struct State(usize, usize);

        impl SearchState for State {
            fn get_transitions(self: Arc<Self>) -> Box<dyn Iterator<Item = State>> {
                Box::new(
                    iter::once(State(self.0 + 1, self.1))
                        .chain(iter::once(State(self.0, self.1 + 1))),
                )
            }
        }

        let tester = SearchConfig::new(State(0, 0)).add_invariant("test", |s| s != &State(10, 5));

        assert_eq!(
            BrokenInvariant(State(10, 5), "test"),
            tester.search_bfs(|_| false)
        );
    }

    #[test]
    fn multiple_invariants() {
        #[derive(Hash, Eq, PartialEq, Debug, Clone)]
        struct State();

        impl SearchState for State {
            fn get_transitions(self: Arc<Self>) -> Box<dyn Iterator<Item = State>> {
                Box::new(iter::empty())
            }
        }

        let tester = SearchConfig::new(State())
            .add_invariant("test 1", |_| true)
            .add_invariant("test 2", |_| false);

        assert_eq!(
            BrokenInvariant(State(), "test 2"),
            tester.search_bfs(|_| false)
        );
    }

    #[test]
    fn pruning() {
        #[derive(Hash, Eq, PartialEq, Debug, Clone)]
        struct State(usize, usize);

        impl SearchState for State {
            fn get_transitions(self: Arc<Self>) -> Box<dyn Iterator<Item = State>> {
                Box::new(
                    iter::once(State(self.0 + 1, self.1))
                        .chain(iter::once(State(self.0, self.1 + 1))),
                )
            }
        }

        let tester = SearchConfig::new(State(0, 0))
            .add_invariant("test", |s| s.1 != 4)
            .add_prune_condition(|s| s.1 >= 3);

        assert_eq!(
            Found(State(10, 0)),
            tester.search_bfs(|s| s == &State(10, 0))
        );
    }

    #[test]
    fn bounded_space() {
        #[derive(Hash, Eq, PartialEq, Debug, Clone)]
        struct State(usize);

        impl SearchState for State {
            fn get_transitions(self: Arc<Self>) -> Box<dyn Iterator<Item = State>> {
                if self.0 < 10 {
                    Box::new(iter::once(State(self.0 + 1)))
                } else {
                    Box::new(iter::empty())
                }
            }
        }

        let tester = SearchConfig::new(State(0));

        assert_eq!(SpaceExhausted, tester.search_bfs(|s| s.0 > 20));
    }

    #[test]
    fn max_depth_exhausted() {
        #[derive(Hash, Eq, PartialEq, Debug, Clone)]
        struct State(usize);

        impl SearchState for State {
            fn get_transitions(self: Arc<Self>) -> Box<dyn Iterator<Item = State>> {
                Box::new(iter::once(State(self.0 + 1)))
            }
        }

        let tester = SearchConfig::new(State(0)).set_max_depth(10);

        assert_eq!(SpaceExhausted, tester.search_bfs(|_| false));
    }

    #[test]
    fn max_depth_found() {
        #[derive(Hash, Eq, PartialEq, Debug, Clone)]
        struct State(usize);

        impl SearchState for State {
            fn get_transitions(self: Arc<Self>) -> Box<dyn Iterator<Item = State>> {
                Box::new(iter::once(State(self.0 + 1)))
            }
        }

        let tester = SearchConfig::new(State(0)).set_max_depth(10);

        assert_eq!(Found(State(10)), tester.search_bfs(|s| s == &State(10)));
    }
}
