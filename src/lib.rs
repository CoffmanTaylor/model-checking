use std::{
    collections::VecDeque,
    fmt::Debug,
    sync::Arc,
    time::{Duration, Instant},
};

use SearchResults::{BrokenInvariant, Found, SearchedOverMax, SpaceExhausted, TimedOut};

pub mod caches;

/// This trait means that the struct this is implemented on can be used to define a specific state
/// of the System. `get_transitions` must be deterministic and idempotent. It is a logical error if
/// two equal `SearchState`s return different results from `get_transitions`. The iteration order
/// must be consistent within the same execution.
pub trait SearchState {
    type Iter: Iterator<Item = Self>;
    fn get_transitions(self: Arc<Self>) -> Self::Iter;
}

/// The results of preforming a search on a system.
#[derive(Debug, PartialEq, Eq)]
pub enum SearchResults<State, InvRes> {
    /// The contained State matches the end condition and is reachable from one of the starting states.
    Found(State),
    /// The contained State broke one of the invariants. The invariant provided InvRes as explication for
    /// why the given state is invalid.
    BrokenInvariant(State, InvRes),
    /// All reachable States, without going past max depth, do not match the end condition.
    SpaceExhausted,
    /// The number of states searched exceeds the maximum provided.
    SearchedOverMax,
    /// The search failed to find a State matching the end condition before exceeding the time limit.
    TimedOut,
}

/// Defines the System that you want to search. The System must contain at least one start state.
#[derive(Clone)]
pub struct SearchConfig<S, InvRes> {
    start_states: VecDeque<Arc<S>>,
    invariants: Vec<fn(&S) -> Result<(), InvRes>>,
    prune_conditions: Vec<fn(&S) -> bool>,
    max_depth: Option<usize>,
    max_states: Option<usize>,
    max_time: Option<Duration>,
}

impl<S> SearchConfig<S, ()> {
    /// Constructs a new `SearchConfig` with no return type of invariants.
    pub fn new_without_inv(start_state: S) -> SearchConfig<S, ()> {
        SearchConfig::new(start_state)
    }
}

impl<S, InvRes> SearchConfig<S, InvRes> {
    /// Constructs a new `SearchConfig` using the provided state as the only start state.
    pub fn new(start_state: S) -> SearchConfig<S, InvRes> {
        let mut start_states = VecDeque::new();
        start_states.push_back(Arc::new(start_state));
        SearchConfig {
            start_states,
            invariants: Vec::new(),
            prune_conditions: Vec::new(),
            max_depth: None,
            max_states: None,
            max_time: None,
        }
    }

    /// Add the given State as a possible start state. When searching, all possible start
    /// States are considered.
    pub fn add_start_state(mut self, start_state: S) -> Self {
        self.start_states.push_back(Arc::new(start_state));
        self
    }

    /// Add all of the given States as possible start states. When searching, all possible
    /// start States are considered.
    pub fn add_start_states<I>(mut self, start_states: I) -> Self
    where
        I: IntoIterator<Item = S>,
    {
        self.start_states
            .extend(start_states.into_iter().map(Arc::new));
        self
    }

    /// Adds an invariant to the System. Every state discovered during the search, excluding
    /// the start States, will be checked against every invariant of the System.
    pub fn add_invariant(mut self, inv: fn(&S) -> Result<(), InvRes>) -> Self {
        self.invariants.push(inv);
        self
    }

    /// Adds a collection of invariants to the System. Every state discovered during the search,
    /// excluding the start States, will be checked against every invariant of the System.
    pub fn add_invariants<I>(mut self, invs: I) -> Self
    where
        I: IntoIterator<Item = fn(&S) -> Result<(), InvRes>>,
    {
        self.invariants.extend(invs);
        self
    }

    /// Adds a pruning condition to the System. When preforming a search, if a state matches
    /// any of the pruning conditions, none of that state's transitions will be searched. This
    /// is helpful for speeding up searches when you know there are states that once you reach,
    /// you know you wont find the end condition afterwards.
    ///
    /// Note: The start States are not checked against the prune conditions.
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

    /// Sets the maximum number of States to search. If the end condition cannot be found before
    /// reaching the maximum, a `SearchResult::SearchedOfMax` will be returned.
    ///
    /// Default to no maximum.
    pub fn set_max_number_of_states(mut self, max_states: usize) -> Self {
        self.max_states = Some(max_states);
        self
    }

    /// Sets the maximum time for the search. If the end condition cannot be found before exceeding
    /// the given duration, a `SearchResult::TimedOut` will be returned.
    ///
    /// Default to no maximum.
    pub fn set_timeout(mut self, timeout: Duration) -> Self {
        self.max_time = Some(timeout);
        self
    }

    /// Preforms a Breath First Search on the System looking for a State that matches the given
    /// end condition. If a matching State is found, it is returned in `Found(State)`. If any of the
    /// System's invariants are violated, then the State that failed and result of the invariant
    /// are returned as `BrokenInvariant(State, ResInv)`.
    ///
    /// Note: Assumes that the starting states are: not prunable, not an end state, and do not
    /// brake invariants.
    pub fn search_bfs(&self, end_condition: fn(&S) -> bool) -> SearchResults<S, InvRes>
    where
        S: SearchState,
    {
        let mut to_search = VecDeque::new();
        to_search.extend(
            self.start_states
                .iter()
                .map(|s| (0, Arc::clone(s).get_transitions())),
        );

        let mut count: usize = 0;

        let start_time = Instant::now();
        let mut last_print = start_time.clone();

        println!("Starting search...");

        while let Some(mut states) = to_search.pop_front() {
            while let Some(state) = states.1.next() {
                count += 1;
                if last_print.elapsed() > Duration::from_secs(5) {
                    last_print = Instant::now();
                    println!("Searched {} states, Current depth: {}", count, states.0);
                }
                if self.max_states.is_some() && Some(count) > self.max_states {
                    return SearchedOverMax;
                }

                if let Some(timeout) = self.max_time {
                    if start_time.elapsed() > timeout {
                        return TimedOut;
                    }
                }

                // Check the invariants.
                for inv in self.invariants.iter() {
                    match inv(&state) {
                        Ok(_) => continue,
                        Err(res) => return BrokenInvariant(state, res),
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

                // Check if we are at the maximin depth.
                if self.max_depth == Some(states.0) {
                    continue;
                }

                // Add this state to the set of searched states.
                let state_rc = Arc::new(state);

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

    mod chain {
        use super::*;

        #[derive(Hash, Eq, PartialEq, Debug, Clone)]
        pub struct State(pub usize);

        impl SearchState for State {
            type Iter = iter::Once<State>;
            fn get_transitions(self: Arc<Self>) -> Self::Iter {
                iter::once(State(self.0 + 1))
            }
        }
    }

    mod tree {
        use super::*;

        #[derive(Hash, Eq, PartialEq, Debug, Clone)]
        pub struct State(pub usize, pub usize);

        impl SearchState for State {
            type Iter = iter::Chain<iter::Once<State>, iter::Once<State>>;
            fn get_transitions(self: Arc<Self>) -> Self::Iter {
                iter::once(State(self.0 + 1, self.1)).chain(iter::once(State(self.0, self.1 + 1)))
            }
        }
    }

    mod chain_to_end {
        use super::*;

        macro_rules! define_tests {
            ($( $func:ident ),+) => {
                $(
                    #[test]
                    fn $func() {
                        use chain::State;

                        let tester =
                            SearchConfig::new_without_inv(State(0)).set_timeout(Duration::from_secs(5));

                        assert_eq!(Found(State(100)), tester.$func(|s| s == &State(100)));
                    }
                )+
            };
        }

        define_tests!(search_bfs);
    }

    mod chain_to_brake {
        use super::*;

        macro_rules! define_tests {
            ($( $func:ident ),+) => {
                $(
                    #[test]
                    fn $func() {
                        use chain::State;

                        let tester = SearchConfig::new(State(0))
                            .set_timeout(Duration::from_secs(5))
                            .add_invariant(|s| if s.0 != 10 { Ok(()) } else { Err("test") });

                        assert_eq!(
                            BrokenInvariant(State(10), "test"),
                            tester.$func(|_| false)
                        );
                    }
                )+
            };
        }

        define_tests!(search_bfs);
    }

    mod tree_to_end {
        use super::*;

        macro_rules! define_tests {
            ($( $func:ident ),+) => {
                $(
                    #[test]
                    fn $func() {
                        use tree::State;

                        let tester = SearchConfig::new_without_inv(State(0, 0)).set_timeout(Duration::from_secs(5));

                        assert_eq!(
                            Found(State(10, 5)),
                            tester.$func(|s| s == &State(10, 5))
                        );
                    }
                )+

            };
        }

        define_tests!(search_bfs);
    }

    mod tree_to_break {
        use super::*;

        macro_rules! define_tests {
            ($( $func:ident ),+) => {
                $(
                    #[test]
                    fn $func() {
                        use tree::State;

                        let tester = SearchConfig::new(State(0, 0))
                            .set_timeout(Duration::from_secs(5))
                            .add_invariant(|s| {
                                if s != &State(10, 5) {
                                    Ok(())
                                } else {
                                    Err("test")
                                }
                            });

                        assert_eq!(
                            BrokenInvariant(State(10, 5), "test"),
                            tester.$func(|_| false)
                        );
                    }
                )+
            };
        }

        define_tests!(search_bfs);
    }

    mod multiple_invariants {
        use super::*;

        macro_rules! define_tests {
            ($( $func:ident ),+) => {
                $(
                    #[test]
                    fn $func() {
                        use chain::State;

                        let tester = SearchConfig::new(State(0))
                            .set_timeout(Duration::from_secs(5))
                            .add_invariant(|_| if true { Ok(()) } else { Err("test 1") })
                            .add_invariant(|s| if s.0 < 1 { Ok(()) } else { Err("test 2") });

                        assert_eq!(
                            BrokenInvariant(State(1), "test 2"),
                            tester.$func(|_| false)
                        );
                    }
                )+
            };
        }

        define_tests!(search_bfs);
    }

    mod pruning {
        use super::*;

        macro_rules! define_tests {
            ($( $func:ident ),+) => {
                $(
                    #[test]
                    fn $func() {
                        use tree::State;

                        let tester = SearchConfig::new(State(0, 0))
                            .set_timeout(Duration::from_secs(5))
                            .add_invariant(|s| if s.1 != 4 { Ok(()) } else { Err("test") })
                            .add_prune_condition(|s| s.1 >= 3);

                        assert_eq!(
                            Found(State(10, 0)),
                            tester.$func(|s| s == &State(10, 0))
                        );
                    }
                )+
            };
        }

        define_tests!(search_bfs);
    }

    mod bounded_space {
        use super::*;

        macro_rules! define_tests {
            ($( $func:ident ),+) => {
                $(
                    #[test]
                    fn $func() {
                        #[derive(Hash, Eq, PartialEq, Debug, Clone)]
                        struct State(usize);

                        impl SearchState for State {
                            type Iter = Box<dyn Iterator<Item = State>>;
                            fn get_transitions(self: Arc<Self>) -> Box<dyn Iterator<Item = State>> {
                                if self.0 < 10 {
                                    Box::new(iter::once(State(self.0 + 1)))
                                } else {
                                    Box::new(iter::empty())
                                }
                            }
                        }

                        let tester = SearchConfig::new_without_inv(State(0)).set_timeout(Duration::from_secs(5));

                        assert_eq!(SpaceExhausted, tester.$func(|s| s.0 > 20));
                    }
                )+
            };
        }

        define_tests!(search_bfs);
    }

    mod max_depth_exhausted {
        use super::*;

        macro_rules! define_tests {
            ($( $func:ident ),+) => {
                $(
                    #[test]
                    fn $func() {
                        use chain::State;

                        let tester = SearchConfig::new_without_inv(State(0))
                            .set_timeout(Duration::from_secs(5))
                            .set_max_depth(10);

                        assert_eq!(SpaceExhausted, tester.$func(|_| false));
                    }
                )+
            };
        }

        define_tests!(search_bfs);
    }

    mod max_depth_found {
        use super::*;

        macro_rules! define_tests {
            ($( $func:ident ),+) => {
                $(
                    #[test]
                    fn $func() {
                        use chain::State;

                        let tester = SearchConfig::new_without_inv(State(0))
                            .set_timeout(Duration::from_secs(5))
                            .set_max_depth(10);

                        assert_eq!(Found(State(10)), tester.$func(|s| s == &State(10)));
                    }
                )+
            };
        }

        define_tests!(search_bfs);
    }
}
