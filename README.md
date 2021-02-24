# Model Checking

This goal of this library is to provide a simple method of writing model checking tests. The driving purpose is for testing distributed systems like Paxos. 

# Design
The idea is to have some state that represents the System being tested. We then generate all posable transitions to a next state of the System. For each of the resulting states: 
- Check if the state violates any of the invariants.
- Check if the state meets the end condition.
- Check if the state is different from all of the previous states that have been searched. if so, add it to a queue of states to be searched.

We then take the next state off of queue and repeat the process. 

## v0.1.1
- Will only be able to search for an end condition.
- No support for log capturing for a failed invariant.
- No support for timed searches.
- No support for searching in parallel.
## v0.1.2
### Log capturing:
If the system we are testing is deterministic and we only used seeded RNGs, then we should be able to capture the logs up to a state that brakes an invariant. Have each state record the state and transition that lead to it. when we find a state that brakes an invariant, we can replay the steps to get to said state and capture the logs.


## License
This software is licensed under GNU GPL-v3.