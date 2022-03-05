module HMMExample (
  initial,
  exampleHMM,
  rgbInitial,
  rgbHMM
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

rgbInitial = [1.0, 0.0, 0.0]

rgbTransitions = [[0.0, 1.0, 0.0],
                  [0.0, 0.0, 1.0],
                  [1.0, 0.0, 0.0]]

rgbEmissions0 = [(1.0, 'R')]
rgbEmissions1 = [(1.0, 'G')]
rgbEmissions2 = [(1.0, 'B')]

rgbS0 = HS 0 rgbEmissions0
rgbS1 = HS 1 rgbEmissions1
rgbS2 = HS 2 rgbEmissions2

rgbHMM = HMM rgbS0 [rgbS0, rgbS1, rgbS2] rgbTransitions
