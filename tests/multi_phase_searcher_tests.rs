mod libs;

use std::time::Duration;

use libs::chain;
use model_checking::{
    multi_phase_searcher::begin_multi_phase_search,
    SearchConfig,
    SearchResults::{BrokenInvariant, Found},
};

#[test]
fn single_phase_only_base() {
    use chain::State;

    let mut searcher = SearchConfig::new_without_inv(State(0));
    searcher.set_timeout(Duration::from_secs(5));

    let res = begin_multi_phase_search(searcher).search(|s| s.0 == 10);

    assert_eq!(0, res.phase, "Phase not equal to 0");
    assert_eq!(Found(State(10)), res.results);
}

#[test]
fn dual_phase_only_base() {
    use chain::State;

    let mut searcher = SearchConfig::new_without_inv(State(0));
    searcher.set_timeout(Duration::from_secs(5));

    let res = begin_multi_phase_search(searcher)
        .search(|s| s.0 == 10)
        .add_phase()
        .search(|s| s.0 == 20);

    assert_eq!(1, res.phase);
    assert_eq!(Found(State(20)), res.results);
}

#[test]
fn dual_phase_with_propagating_error() {
    use chain::State;

    let mut searcher = SearchConfig::new(State(0));
    searcher.set_timeout(Duration::from_secs(5));

    let res = begin_multi_phase_search(searcher)
        .add_named_phase_invariant("not 5".to_string(), |s| s.0 != 5)
        .search(|s| s.0 == 10)
        .add_phase()
        .search(|s| s.0 == 20);

    assert_eq!(0, res.phase);
    assert_eq!(BrokenInvariant(State(5), "not 5".to_string()), res.results);
}

#[test]
fn dual_phase_with_phase_invariant_that_will_fail_in_other_phase() {
    use chain::State;

    let mut searcher = SearchConfig::new(State(0));
    searcher.set_timeout(Duration::from_secs(5));

    let res = begin_multi_phase_search(searcher)
        .add_named_phase_invariant("not 15".to_string(), |s| s.0 != 15)
        .search(|s| s.0 == 10)
        .add_phase()
        .search(|s| s.0 == 20);

    assert_eq!(1, res.phase);
    assert_eq!(Found(State(20)), res.results);
}
