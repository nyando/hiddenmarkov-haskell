# Simple Hidden Markov Models for Haskell

The `hiddenmarkov-haskell` library implements a simple discretely distributed Hidden Markov Model (DDHMM) data structure.
To go along with the HMM strucure, this library includes algorithms for solving the basic problems around HMMs.
This includes implementations for:

- the *forward-backward* algorithm for calculating the probability of a specific string emission,
- the *Viterbi* algorithm for calculating the most likely hidden state sequence that generates a given string,
- and the *Baum-Welch* algorithm for calculating the transition probabilities for the hidden states of the HMM.

## Prerequisites

To be able to compile the application,
install the [Haskell Stack](https://docs.haskellstack.org/en/stable/README/).
Ensure that the `stack` executable is in the system or user `PATH` variable.

## Compilation

To download dependencies to the project directory and compile sources to binary, run `stack build` in the console of your choice.

## Running

To compile on source code changes and execute the program contained in `Main.hs`, run

```bash
stack run -- [ARGS]
```

The application offers the following execution modes:

- `print [FILE]` - print the HMM data structure defined in `FILE`
- `generate [FILE] [LENGTH]` - generate an emission string of length `LENGTH` using the HMM defined in `FILE`
- `forward [FILE] [STRING]` - calculate the forward probability of realization `STRING` given an HMM defined in `FILE`
- `backward [FILE] [STRING]` - calculate the backward probability of realization `STRING` given an HMM defined in `FILE`
- `viterbi [FILE] [STRING]` - calculate the most likely hidden state sequence generating realization `STRING` given an HMM defined in `FILE`
- `baumWelch [FILE] [STRING] [LIMIT]` - perform Baum-Welch estimation on the HMM defined in `FILE` 
  to maximize expectation of `STRING` until the sum of all parameters is smaller than `LIMIT` (convergence limit)

## Testing

`hiddenmarkov-haskell` includes a few unit tests.
These were written using the `HSpec` library.
To run them, call `stack test`.

## Defining an HMM

To define an HMM for use with the application, you will need the following parameters:

- initial state distribution
- number of hidden states and transition matrix
- emission alphabet (characters) and emission probabilities by state

The following is an example for an HMM with three hidden states and four emission characters.

```
INIT 0.4 0.2 0.4
TRANS 0 0.3 0.2 0.5
TRANS 1 0.5 0.4 0.1
TRANS 2 0.8 0.1 0.1
EMIT A C G T
EMITSTATE 0 0.3 0.2 0.5 0.0
EMITSTATE 1 0.2 0.0 0.1 0.7
EMITSTATE 2 0.0 0.5 0.0 0.5
```
