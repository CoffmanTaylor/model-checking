use crate::{
    SearchConfig,
    SearchResults::{self, Found},
    SearchState, SearchStatistics,
};

use derivative::Derivative;

pub struct PhaseConfig<State, InvRes> {
    phase_number: usize,
    config_or_error: PhaseConfigOrError<State, InvRes>,
}

enum PhaseConfigOrError<State, InvRes> {
    Config {
        base_searcher: SearchConfig<State, InvRes>,
        phase_searcher: SearchConfig<State, InvRes>,
        name: Option<&'static str>,
    },
    Error {
        results: SearchResults<State, InvRes>,
        stats: SearchStatistics,
    },
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct PhaseSearchResults<State, InvRes> {
    pub phase: usize,
    pub results: SearchResults<State, InvRes>,
    pub stats: SearchStatistics,
    #[derivative(Debug = "ignore")]
    base_searcher: Option<SearchConfig<State, InvRes>>,
}

pub fn begin_multi_phase_search<State, InvRes>(
    base_searcher: SearchConfig<State, InvRes>,
) -> PhaseConfig<State, InvRes>
where
    SearchConfig<State, InvRes>: Clone,
{
    PhaseConfig {
        phase_number: 0,
        config_or_error: PhaseConfigOrError::Config {
            base_searcher: base_searcher.clone(),
            phase_searcher: base_searcher,
            name: Some("Start phase"),
        },
    }
}

impl<State, InvRes> PhaseConfig<State, InvRes> {
    pub fn search<F>(self, end_condition: F) -> PhaseSearchResults<State, InvRes>
    where
        F: Fn(&State) -> bool,
        State: SearchState + Clone,
    {
        match self.config_or_error {
            PhaseConfigOrError::Config {
                base_searcher,
                phase_searcher,
                name,
            } => {
                if let Some(name) = name {
                    println!("Beginning phase {} : {}", self.phase_number + 1, name);
                } else {
                    println!("Beginning phase {}", self.phase_number + 1);
                }
                let (results, stats) = phase_searcher.search_bfs(end_condition);
                PhaseSearchResults {
                    results,
                    stats,
                    base_searcher: Some(base_searcher),
                    phase: self.phase_number,
                }
            }
            PhaseConfigOrError::Error { results, stats } => PhaseSearchResults {
                results,
                stats,
                phase: self.phase_number,
                base_searcher: None,
            },
        }
    }

    pub fn map_state<F>(mut self, map: F) -> Self
    where
        F: Fn(State) -> State,
    {
        match self.config_or_error {
            PhaseConfigOrError::Config {
                base_searcher: _,
                ref mut phase_searcher,
                name: _,
            } => {
                let state = phase_searcher.start_states.remove(0).unwrap();
                phase_searcher.start_states.push_front(map(state));
            }
            PhaseConfigOrError::Error {
                results: _,
                stats: _,
            } => {}
        }
        self
    }
}

impl<State> PhaseConfig<State, String> {
    pub fn add_named_phase_invariant<F>(mut self, name: String, inv: F) -> Self
    where
        F: Fn(&State) -> bool + 'static,
    {
        match self.config_or_error {
            PhaseConfigOrError::Config {
                base_searcher: _,
                ref mut phase_searcher,
                name: _,
            } => {
                phase_searcher.add_named_invariant(name, inv);
                self
            }
            PhaseConfigOrError::Error {
                results: _,
                stats: _,
            } => self,
        }
    }
}

impl<State, InvRes> PhaseSearchResults<State, InvRes> {
    fn add_phase_helper(self, name: Option<&'static str>) -> PhaseConfig<State, InvRes>
    where
        SearchConfig<State, InvRes>: Clone,
    {
        // Check if the previous phase resulted in a found state.
        if let Found(state) = self.results {
            let mut phase_searcher = self.base_searcher.clone().unwrap();
            phase_searcher.replace_start(state);

            PhaseConfig {
                phase_number: self.phase + 1,
                config_or_error: PhaseConfigOrError::Config {
                    base_searcher: self.base_searcher.unwrap(),
                    phase_searcher,
                    name: name,
                },
            }
        } else {
            // Cant continue because we failed to find a valid state.
            PhaseConfig {
                phase_number: self.phase,
                config_or_error: PhaseConfigOrError::Error {
                    results: self.results,
                    stats: self.stats,
                },
            }
        }
    }

    pub fn add_phase(self) -> PhaseConfig<State, InvRes>
    where
        SearchConfig<State, InvRes>: Clone,
    {
        self.add_phase_helper(None)
    }

    pub fn add_named_phase(self, name: &'static str) -> PhaseConfig<State, InvRes>
    where
        SearchConfig<State, InvRes>: Clone,
    {
        self.add_phase_helper(Some(name))
    }
}
