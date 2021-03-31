use std::{
    collections::VecDeque,
    fmt::Debug,
    iter,
    sync::Arc,
    thread,
    time::{Duration, Instant},
};

use chaining_iter::IterChain;
use SearchResults::{BrokenInvariant, Found, SearchedOverMax, SpaceExhausted, TimedOut};

pub mod multi_phase_searcher;
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

impl<State, InvRes> SearchResults<State, InvRes> {
    pub fn is_found(&self) -> bool {
        match self {
            Found(_) => true,
            _ => false,
        }
    }
}

/// Defines the System that you want to search. The System must contain at least one start state.
#[derive(Clone)]
pub struct SearchConfig<S, InvRes> {
    start_states: VecDeque<S>,
    invariants: Vec<Arc<Box<dyn Fn(&S) -> Result<(), InvRes> + Send + Sync>>>,
    prune_conditions: Vec<Arc<Box<dyn Fn(&S) -> bool + Send + Sync>>>,
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

impl<S> SearchConfig<S, String> {
    /// Adds a simple boolean invariant with the given name. If the invariant ever returns false,
    /// the name will be returned in the [SearchResults::BrokenInvariant].
    pub fn add_named_invariant<F>(&mut self, name: String, inv: F) -> &mut Self
    where
        F: Fn(&S) -> bool + 'static + Send + Sync,
    {
        self.add_invariant(move |s| if inv(s) { Ok(()) } else { Err(name.clone()) })
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
    pub fn add_start_state(&mut self, start_state: S) -> &mut Self {
        self.start_states.push_back(start_state);
        self
    }

    /// Add all of the given States as possible start states. When searching, all possible
    /// start States are considered.
    pub fn add_start_states<I>(&mut self, start_states: I) -> &mut Self
    where
        I: IntoIterator<Item = S>,
    {
        self.start_states.extend(start_states.into_iter());
        self
    }

    /// Adds an invariant to the System. Every state discovered during the search, excluding
    /// the start States, will be checked against every invariant of the System.
    pub fn add_invariant<F>(&mut self, inv: F) -> &mut Self
    where
        F: Fn(&S) -> Result<(), InvRes> + 'static + Send + Sync,
    {
        self.invariants.push(Arc::new(Box::new(inv)));
        self
    }

    /// Adds a collection of invariants to the System. Every state discovered during the search,
    /// excluding the start States, will be checked against every invariant of the System.
    pub fn add_invariants<I, F>(&mut self, invs: I) -> &mut Self
    where
        F: Fn(&S) -> Result<(), InvRes> + 'static + Send + Sync,
        I: IntoIterator<Item = F>,
    {
        for inv in invs {
            self.invariants.push(Arc::new(Box::new(inv)));
        }
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
    pub fn add_prune_condition<F>(&mut self, prune_condition: F) -> &mut Self
    where
        F: Fn(&S) -> bool + 'static + Send + Sync,
    {
        self.prune_conditions
            .push(Arc::new(Box::new(prune_condition)));
        self
    }

    /// Sets the maximum depth any search will go. If the end condition cannot be found within
    /// the states reachable inside the maximum depth, a `SearchResult::SpaceExhausted` will be
    /// returned.
    ///
    /// Default to no maximum depth.
    pub fn set_max_depth(&mut self, max_depth: usize) -> &mut Self {
        self.max_depth = Some(max_depth);
        self
    }

    /// Sets the maximum number of States to search. If the end condition cannot be found before
    /// reaching the maximum, a `SearchResult::SearchedOfMax` will be returned.
    ///
    /// Default to no maximum.
    pub fn set_max_number_of_states(&mut self, max_states: usize) -> &mut Self {
        self.max_states = Some(max_states);
        self
    }

    /// Sets the maximum time for the search. If the end condition cannot be found before exceeding
    /// the given duration, a `SearchResult::TimedOut` will be returned.
    ///
    /// Default to no maximum.
    pub fn set_timeout(&mut self, timeout: Duration) -> &mut Self {
        self.max_time = Some(timeout);
        self
    }

    /// Removes all of the start states and replaces them with the given new start state.
    pub fn replace_start(&mut self, new_start: S) -> &mut Self {
        self.start_states = VecDeque::new();
        self.start_states.push_back(new_start);

        self
    }

    /// Will preform a bfs search with no end condition. Will only return if an invariant is broken
    /// or a constraint is reached, ie max depth or time limit.
    pub fn exhaustive_search(&self) -> (SearchResults<S, InvRes>, SearchStatistics)
    where
        S: SearchState + Clone,
    {
        self.search_bfs(|_| false)
    }

    /// Preforms a Breath First Search on the System looking for a State that matches the given
    /// end condition. If a matching State is found, it is returned in `Found(State)`. If any of the
    /// System's invariants are violated, then the State that failed and result of the invariant
    /// are returned as `BrokenInvariant(State, ResInv)`. Will clear any shared data contained in the
    /// states.
    ///
    /// Note: Assumes that the starting states are: not prunable, not an end state, and do not
    /// brake invariants.
    ///
    /// Note: It is *extremely* encouraged to compile with optimizations. I have witnessed 10x speed up
    /// of the search with vs. without optimizations.
    pub fn search_bfs<F>(&self, end_condition: F) -> (SearchResults<S, InvRes>, SearchStatistics)
    where
        S: SearchState + Clone,
        F: Fn(&S) -> bool,
    {
        let end_condition = Arc::new(Box::new(end_condition));

        println!("Starting search...");

        // Setup the to_search queue with all of the possible transitions from the starting states.
        let mut to_search = IterChain::new();
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
            .map(|s| iter::repeat(1).zip(Arc::new(s).get_transitions()))
            .for_each(|i| to_search.include(i));

        // Variables used for printing statistics and determining if we have gone passed a search constraint.
        let mut count = 0;
        let mut last_count = 0;
        let mut max_depth = 0;
        let start_time = Instant::now();
        let mut last_print = start_time.clone();

        while let Some((depth, state)) = to_search.next() {
            max_depth = usize::max(depth, max_depth);
            count += 1;

            // Check if we should print active statistics.
            if last_print.elapsed() > Duration::from_secs(5) {
                last_print = Instant::now();
                println!(
                    "Max depth: {}, Searched {}k states, At a rate of {:.2}k states per second.",
                    max_depth,
                    count / 1000,
                    (count - last_count) as f32 / 5000.0,
                );
                last_count = count;
            }

            let stats = SearchStatistics {
                max_depth,
                number_of_states: count,
                total_time: start_time.elapsed(),
            };

            // Check if we have searched more states than the user requested.
            if self.max_states.is_some() && Some(count) > self.max_states {
                println!("Search Complete");
                return (SearchedOverMax, stats);
            }

            // Check if we have searched for longer time than the user requested.
            if let Some(timeout) = self.max_time {
                if start_time.elapsed() > timeout {
                    println!("Search Complete");
                    return (TimedOut, stats);
                }
            }

            // Check the invariants.
            for inv in self.invariants.iter() {
                match inv(&state) {
                    Ok(_) => continue,
                    Err(res) => {
                        println!("Search Complete");
                        return (BrokenInvariant(state, res), stats);
                    }
                }
            }

            // Check the end condition.
            if end_condition(&state) {
                println!("Search Complete");
                return (Found(state), stats);
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
            let transitions = iter::repeat(depth + 1).zip(Arc::new(state).get_transitions());
            to_search.include(transitions);
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

    pub fn parallel_search<F>(
        &self,
        end_condition: F,
    ) -> (SearchResults<S, InvRes>, SearchStatistics)
    where
        S: SearchState + Clone + Send + Sync + 'static,
        S::Iter: Send + Sync,
        InvRes: Send + Sync + 'static,
        Self: Clone,
        F: Fn(&S) -> bool + Send + Sync + 'static,
    {
        let start_time = Instant::now();

        let to_search = crossbeam_channel::unbounded();
        // If the channel contains anything, it is time to stop.
        let control = crossbeam_channel::bounded::<u8>(1);

        for start_state in self.start_states.iter() {
            to_search
                .0
                .send(Arc::new(start_state.clone()).get_transitions())
                .unwrap();
        }

        let end_condition = Arc::new(end_condition);

        let threads: Vec<_> = (0..num_cpus::get())
            .map(|_| {
                let control = control.clone();
                let to_search = to_search.clone();
                let end_condition = Arc::clone(&end_condition);
                let config = self.clone();

                thread::spawn(move || {
                    let mut count = 0;
                    for states in to_search.1 {
                        // Check if we should stop.
                        if control.1.len() == 1 {
                            return (PartialResult::Interrupted, count);
                        }

                        'states: for state in states {
                            count += 1;

                            for inv in config.invariants.iter() {
                                if let Err(res) = inv(&state) {
                                    // Signal that the search is done.
                                    control.0.send(1).unwrap();
                                    return (PartialResult::BrokenInvariant(state, res), count);
                                }
                            }

                            if end_condition(&state) {
                                // Signal that the search is done.
                                control.0.send(1).unwrap();
                                return (PartialResult::Found(state), count);
                            }

                            for condition in config.prune_conditions.iter() {
                                if condition(&state) {
                                    continue 'states;
                                }
                            }

                            to_search.0.send(Arc::new(state).get_transitions()).unwrap();
                        }
                    }

                    (PartialResult::Exhausted, count)
                })
            })
            .collect();

        let res = threads.into_iter().map(|t| t.join()).fold(
            (PartialResult::Interrupted, 0),
            |(acc, total), r| match acc {
                PartialResult::Interrupted | PartialResult::Exhausted => {
                    let r = r.unwrap();
                    (r.0, total + r.1)
                }
                e => (e, total + r.unwrap().1),
            },
        );

        (
            match res.0 {
                PartialResult::Found(s) => SearchResults::Found(s),
                PartialResult::BrokenInvariant(s, r) => SearchResults::BrokenInvariant(s, r),
                PartialResult::Interrupted => SearchResults::TimedOut,
                PartialResult::Exhausted => SearchResults::SpaceExhausted,
            },
            SearchStatistics {
                number_of_states: res.1,
                max_depth: 0,
                total_time: start_time.elapsed(),
            },
        )
    }
}

enum PartialResult<State, InvRes> {
    Found(State),
    BrokenInvariant(State, InvRes),
    Interrupted,
    Exhausted,
}
