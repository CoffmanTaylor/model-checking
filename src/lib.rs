use std::{
    collections::VecDeque,
    fmt::Debug,
    sync::Arc,
    time::{Duration, Instant},
};

use SearchResults::{BrokenInvariant, Found, SearchedOverMax, SpaceExhausted, TimedOut};

pub mod wrappers;

/// This trait means that the struct this is implemented on can be used to define a specific state
/// of the System.
pub trait SearchState {
    type Iter: Iterator<Item = Self>;

    /// Get all possible states that can be reached in one 'step' from this state. The order and content
    /// of the returned iterator MUST be deterministic and consistent within the same compilation. Must
    /// be idempotent.
    fn get_transitions(self: Arc<Self>) -> Self::Iter;

    /// If this type contains any shared data, this function will merge the shared data between the two
    /// given states.
    ///
    /// Note: Any shared state MUST not effect equality, hash value, ordering, and results
    /// of get_transitions. It is a logical error if it does.
    fn merge_shared_state(&mut self, _other: &mut Self) {}

    /// If this type contains any shared data, this function will restore the value of the shared data
    /// to the starting value. It will also un-merge any merged states.
    ///
    /// Note: Any shared state MUST not effect equality, hash value, ordering, and results
    /// of get_transitions. It is a logical error if it does.
    fn clear_shared_state(&mut self) {}
}

/// Statistics of the completed search.
#[derive(Debug, PartialEq, Eq)]
pub struct SearchStatistics {
    pub number_of_states: usize,
    pub max_depth: usize,
    pub total_time: Duration,
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
    start_states: VecDeque<S>,
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
        start_states.push_back(start_state);
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
        self.start_states.push_back(start_state);
        self
    }

    /// Add all of the given States as possible start states. When searching, all possible
    /// start States are considered.
    pub fn add_start_states<I>(mut self, start_states: I) -> Self
    where
        I: IntoIterator<Item = S>,
    {
        self.start_states.extend(start_states.into_iter());
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
    ///
    /// Note: Invariants and the end condition are checked before the state is considered for pruning.
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
    ///
    /// Note: It is *extremely* encouraged to compile with optimizations. I have witnessed 10x speed up
    /// of the search with vs. without optimizations.
    pub fn search_bfs(
        &self,
        end_condition: fn(&S) -> bool,
    ) -> (SearchResults<S, InvRes>, SearchStatistics)
    where
        S: SearchState + Clone,
    {
        println!("Starting search...");

        // Setup the to_search queue with all of the possible transitions from the starting states.
        let mut to_search: VecDeque<_> = {
            let mut tmp = Vec::new();

            // Setup the first state
            let mut start_state = self.start_states[0].clone();
            start_state.clear_shared_state();
            tmp.push(start_state);

            // merge the shared states of all of the other start states.
            for (i, state) in self.start_states.iter().enumerate().skip(1) {
                let mut state = state.clone();
                state.clear_shared_state();
                tmp[i - 1].merge_shared_state(&mut state);
                tmp.push(state);
            }

            // Create the vec of transitions.
            tmp.into_iter()
                .map(|s| (1, Arc::new(s).get_transitions()))
                .collect()
        };

        // Variables used for printing statistics and determining if we have gone passed a search constraint.
        let mut count = 0;
        let mut last_count = 0;
        let mut max_depth = 0;
        let start_time = Instant::now();
        let mut last_print = start_time.clone();

        while let Some((depth, mut states)) = to_search.pop_front() {
            max_depth = usize::max(depth, max_depth);

            while let Some(state) = states.next() {
                count += 1;

                // Check if we should print active statistics.
                if last_print.elapsed() > Duration::from_secs(5) {
                    last_print = Instant::now();
                    println!(
                        "Current depth: {}, Searched {}k states, At a rate of {:.2}k states per second.",
                        depth,
                        count / 1000,
                        (count - last_count) as f32 / 5000.0
                    );
                    last_count = count;
                }

                // Check if we have searched more states than the user requested.
                if self.max_states.is_some() && Some(count) > self.max_states {
                    println!("Search Complete");
                    return (
                        SearchedOverMax,
                        SearchStatistics {
                            max_depth,
                            number_of_states: count,
                            total_time: start_time.elapsed(),
                        },
                    );
                }

                // Check if we have searched for longer time than the user requested.
                if let Some(timeout) = self.max_time {
                    if start_time.elapsed() > timeout {
                        println!("Search Complete");
                        return (
                            TimedOut,
                            SearchStatistics {
                                max_depth,
                                number_of_states: count,
                                total_time: start_time.elapsed(),
                            },
                        );
                    }
                }

                // Check the invariants.
                for inv in self.invariants.iter() {
                    match inv(&state) {
                        Ok(_) => continue,
                        Err(res) => {
                            println!("Search Complete");
                            return (
                                BrokenInvariant(state, res),
                                SearchStatistics {
                                    max_depth,
                                    number_of_states: count,
                                    total_time: start_time.elapsed(),
                                },
                            );
                        }
                    }
                }

                // Check the end condition.
                if end_condition(&state) {
                    println!("Search Complete");
                    return (
                        Found(state),
                        SearchStatistics {
                            max_depth,
                            number_of_states: count,
                            total_time: start_time.elapsed(),
                        },
                    );
                }

                // Check if the state should be pruned
                if self.prune_conditions.iter().any(|f| f(&state)) {
                    continue;
                }

                // Check if we are at the maximin depth.
                if self.max_depth == Some(depth) {
                    continue;
                }

                // Add the possible transitions to the search queue.
                to_search.push_back((depth + 1, Arc::new(state).get_transitions()));
            }
        }

        // If we reach here, that means we have either emptied the to_search queue or have searched over
        // the requested number of states the user requested.
        println!("Search Complete");

        (
            SpaceExhausted,
            SearchStatistics {
                max_depth,
                number_of_states: count,
                total_time: start_time.elapsed(),
            },
        )
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

                        let (res, _) = tester.$func(|s| s == &State(100));

                        assert_eq!(Found(State(100)), res);
                    }
                )+
            };
        }

        define_tests!(search_bfs);
    }

    mod check_inv_before_pruning {
        use super::*;

        macro_rules! define_tests {
            ($( $func:ident ),+) => {
                $(
                    #[test]
                    fn $func() {
                        use chain::State;

                        let tester =
                            SearchConfig::new(State(0))
                                .set_timeout(Duration::from_secs(5))
                                .add_invariant(|s| if s == &State(10) { Err(()) } else { Ok(()) })
                                .add_prune_condition(|s| s == &State(10));

                        assert_eq!(BrokenInvariant(State(10), ()), tester.$func(|s| s == &State(100)).0);
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
                            tester.$func(|_| false).0
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
                            tester.$func(|s| s == &State(10, 5)).0
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
                            tester.$func(|_| false).0
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
                            tester.$func(|_| false).0
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
                            tester.$func(|s| s == &State(10, 0)).0
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

                        assert_eq!(SpaceExhausted, tester.$func(|s| s.0 > 20).0);
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

                        let (res, stats) = tester.$func(|_| false);

                        assert_eq!(SpaceExhausted, res);
                        assert_eq!(10, stats.number_of_states);
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

                        assert_eq!(Found(State(10)), tester.$func(|s| s == &State(10)).0);
                    }
                )+
            };
        }

        define_tests!(search_bfs);
    }

    mod can_reuse_searcher {
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

                        assert_eq!(Found(State(10)), tester.clone().$func(|s| s == &State(10)).0);
                        assert_eq!(Found(State(10)), tester.clone().$func(|s| s == &State(10)).0);
                    }
                )+
            };
        }

        define_tests!(search_bfs);
    }

    mod shared_state {
        use std::{iter::Once, ops::AddAssign, sync::Mutex};

        use super::*;

        #[derive(Clone)]
        struct State {
            shared: Arc<Mutex<usize>>,
            private: usize,
        }

        impl State {
            fn new() -> State {
                State {
                    shared: Arc::new(Mutex::new(1)),
                    private: 1,
                }
            }
        }

        impl SearchState for State {
            type Iter = Once<State>;

            fn get_transitions(self: Arc<Self>) -> Self::Iter {
                self.shared.lock().unwrap().add_assign(1);
                iter::once(State {
                    shared: Arc::clone(&self.shared),
                    private: self.private + 1,
                })
            }

            fn clear_shared_state(&mut self) {
                self.shared = Arc::new(Mutex::new(1));
            }

            fn merge_shared_state(&mut self, other: &mut Self) {
                // Add the count from other into self.
                self.shared
                    .lock()
                    .unwrap()
                    .add_assign(other.shared.lock().unwrap().clone());

                // link self's count to other.
                other.shared = Arc::clone(&self.shared);
            }
        }

        #[test]
        fn single_shared_state() {
            let (res, _) =
                SearchConfig::new_without_inv(State::new()).search_bfs(|s| s.private == 10);

            if let Found(state) = res {
                assert_eq!(10, state.shared.lock().unwrap().clone());
            } else {
                panic!("Test failed");
            }
        }

        #[test]
        fn dual_shared_state() {
            let (res, _) = SearchConfig::new_without_inv(State::new())
                .add_start_state(State::new())
                .search_bfs(|s| s.private == 10);

            if let Found(state) = res {
                assert_eq!(20, state.shared.lock().unwrap().clone());
            } else {
                panic!("Test failed");
            }
        }

        #[test]
        fn can_reuse_search_with_shared_state() {
            let searcher = SearchConfig::new_without_inv(State::new());

            let (res, _) = searcher.search_bfs(|s| s.private == 10);

            if let Found(state) = res {
                assert_eq!(10, state.shared.lock().unwrap().clone(), "Failed first use");
            } else {
                panic!("Test failed: first use");
            }

            let (res, _) = searcher.search_bfs(|s| s.private == 5);

            if let Found(state) = res {
                assert_eq!(5, state.shared.lock().unwrap().clone(), "Failed second use");
            } else {
                panic!("Test failed: second use");
            }
        }
    }
}
