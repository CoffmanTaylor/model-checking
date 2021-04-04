# Model Checking
`Model Checking` is an early stage model checking framework. The intent is for it to be used when testing distributed systems like those defined in [ds-libs](https://github.com/CoffmanTaylor/DS-libs).

## What is model checking?
Model checking is a method of testing programs. The idea is that you construct a representation of your program as a state machine. You then define a set of starting states and a function to go from an arbitrary state to all of the states that can be reached in one 'step' from that state.

You can then test aspects of your program by asserting invariants about the possible states that can appear. If your constructed state machine is finite, then a model checking framework can check your invariants against every possible state. If however your state machine is infinite, you can only check so many states. Therefore, my model checking framework also allows you to provide a predicate used to search for a specific end state.

## Design
Currently the only search function is a single threaded breadth first search. This is because of problems with the implementations of the parallel searchers and and depth first searcher. Theses problems can be seen in the issues.

We define a single trait that is needed to preform searchers, `SearchState`. This trait defines a function to get all of the possible states reachable in one 'step' form the given state. It also currently provides a method of managing parts of a state that are shared thorough a Rc/Arc with every state. Having shared state is useful for debugging, duplicate detection, and other wrapper states. Though the method of managing this shared state is kind of clumsy and will probably change.

### Arc/Rc
All through out the library we use Arc'/Rc'. This is not for managing ownership but for improved memory performance. Because mosts states are only slightly different from their 'parent' state, by wrapping parts of the state in Arc/Rc we can only clone the parts that change. This increases speed by removing unnecessary clones and reduces memory consumption by having many duplicate parts of a state share the same underlining date. By wrapping each field of a state in an Arc, I have seen reductions in memory usage by 80%.

Further experimentation is being done with [unifying-state](https://github.com/CoffmanTaylor/Unifying-State), which only stores a single instance of each unique state component. Using this invariant, it is attempting to use memoization to speedup the creation of new states from their parent states.