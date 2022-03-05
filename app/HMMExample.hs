module HMMExample (
  initial,
  exampleHMM
) where

import HiddenMarkovModel

initial = [0.5, 0.5]

transitions = [[0.5, 0.5],
               [0.4, 0.6]]

emissions0 = [(0.2, 'A'), (0.3, 'C'), (0.3, 'G'), (0.2, 'T')]
emissions1 = [(0.3, 'A'), (0.2, 'C'), (0.2, 'G'), (0.3, 'T')]

s0 = HS 0 emissions0
s1 = HS 1 emissions1

exampleHMM = HMM s0 [s0, s1] transitions
