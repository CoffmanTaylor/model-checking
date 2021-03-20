pub mod chain {
    use std::{iter, sync::Arc};

    use model_checking::SearchState;

    #[derive(Hash, Eq, PartialEq, Debug, Clone)]
    pub struct State(pub usize);

    impl SearchState for State {
        type Iter = iter::Once<State>;
        fn get_transitions(self: Arc<Self>) -> Self::Iter {
            iter::once(State(self.0 + 1))
        }
    }
}

pub mod tree {
    use std::{iter, sync::Arc};

    use model_checking::SearchState;

    #[derive(Hash, Eq, PartialEq, Debug, Clone)]
    pub struct State(pub usize, pub usize);

    impl SearchState for State {
        type Iter = iter::Chain<iter::Once<State>, iter::Once<State>>;
        fn get_transitions(self: Arc<Self>) -> Self::Iter {
            iter::once(State(self.0 + 1, self.1)).chain(iter::once(State(self.0, self.1 + 1)))
        }
    }
}
