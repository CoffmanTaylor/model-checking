use std::{iter, sync::Arc, time::Duration};

use model_checking::{
    SearchConfig,
    SearchResults::{BrokenInvariant, Found, SpaceExhausted},
    SearchState,
};

mod libs;

use libs::{chain, tree};

#[test]
fn chain_to_end() {
    use chain::State;

    let mut tester = SearchConfig::new_without_inv(State(0));
    tester.set_timeout(Duration::from_secs(5));

    let (res, _) = tester.search_bfs(|s| s == &State(100));

    assert_eq!(Found(State(100)), res);
}

#[test]
fn check_inv_before_pruning() {
    use chain::State;

    let mut tester = SearchConfig::new(State(0));
    tester
        .set_timeout(Duration::from_secs(5))
        .add_invariant(|s| if s == &State(10) { Err(()) } else { Ok(()) })
        .add_prune_condition(|s| s == &State(10));

    assert_eq!(
        BrokenInvariant(State(10), ()),
        tester.search_bfs(|s| s == &State(100)).0
    );
}

#[test]
fn chain_to_brake() {
    use chain::State;

    let mut tester = SearchConfig::new(State(0));
    tester
        .set_timeout(Duration::from_secs(5))
        .add_invariant(|s| if s.0 != 10 { Ok(()) } else { Err("test") });

    assert_eq!(
        BrokenInvariant(State(10), "test"),
        tester.search_bfs(|_| false).0
    );
}

#[test]
fn tree_to_end() {
    use tree::State;

    let mut tester = SearchConfig::new_without_inv(State(0, 0));
    tester.set_timeout(Duration::from_secs(5));

    assert_eq!(
        Found(State(10, 5)),
        tester.search_bfs(|s| s == &State(10, 5)).0
    );
}

#[test]
fn tree_to_break() {
    use tree::State;

    let mut tester = SearchConfig::new(State(0, 0));
    tester
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
        tester.search_bfs(|_| false).0
    );
}

#[test]
fn multiple_invariants() {
    use chain::State;

    let mut tester = SearchConfig::new(State(0));
    tester
        .set_timeout(Duration::from_secs(5))
        .add_invariant(|_| if true { Ok(()) } else { Err("test 1") })
        .add_invariant(|s| if s.0 < 1 { Ok(()) } else { Err("test 2") });

    assert_eq!(
        BrokenInvariant(State(1), "test 2"),
        tester.search_bfs(|_| false).0
    );
}

#[test]
fn pruning() {
    use tree::State;

    let mut tester = SearchConfig::new(State(0, 0));
    tester
        .set_timeout(Duration::from_secs(5))
        .add_invariant(|s| if s.1 != 4 { Ok(()) } else { Err("test") })
        .add_prune_condition(|s| s.1 >= 3);

    assert_eq!(
        Found(State(10, 0)),
        tester.search_bfs(|s| s == &State(10, 0)).0
    );
}

#[test]
fn bounded_space() {
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

    let mut tester = SearchConfig::new_without_inv(State(0));
    tester.set_timeout(Duration::from_secs(5));

    assert_eq!(SpaceExhausted, tester.search_bfs(|s| s.0 > 20).0);
}

#[test]
fn max_depth_exhausted() {
    use chain::State;

    let mut tester = SearchConfig::new_without_inv(State(0));
    tester.set_timeout(Duration::from_secs(5)).set_max_depth(10);

    let (res, stats) = tester.search_bfs(|_| false);

    assert_eq!(SpaceExhausted, res);
    assert_eq!(10, stats.number_of_states);
}

#[test]
fn max_depth_found() {
    use chain::State;

    let mut tester = SearchConfig::new_without_inv(State(0));
    tester.set_timeout(Duration::from_secs(5)).set_max_depth(10);

    assert_eq!(Found(State(10)), tester.search_bfs(|s| s == &State(10)).0);
}

#[test]
fn can_reuse_searcher() {
    use chain::State;

    let mut tester = SearchConfig::new_without_inv(State(0));
    tester.set_timeout(Duration::from_secs(5)).set_max_depth(10);

    assert_eq!(
        Found(State(10)),
        tester.clone().search_bfs(|s| s == &State(10)).0
    );
    assert_eq!(
        Found(State(10)),
        tester.clone().search_bfs(|s| s == &State(10)).0
    );
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
        let (res, _) = SearchConfig::new_without_inv(State::new()).search_bfs(|s| s.private == 10);

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
