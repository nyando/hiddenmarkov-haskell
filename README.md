# hiddenmarkov-haskell

The `hiddenmarkov-haskell` library implements a simple discrete distribution Hidden Markov Model (DDHMM) data structure.
To go along with the HMM strucure, this library includes algorithms for solving the basic problems around HMMs.
This includes implementations for:

- the *forward-backward* algorithm for calculating the probability of a specific string emission
- the *Viterbi* algorithm for calculating the most likely hidden state sequence that generates a given string
- the *Baum-Welch* algorithm for calculating the transition probabilities for the hidden states of the HMM.
