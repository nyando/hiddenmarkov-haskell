type Transitions = [[Float]]
type Emissions = [(Char, Float)]

data HiddenState = HS Int Emissions
data HiddenMarkovModel = HMM [HiddenState] Transitions

transitions = [
    [0.3, 0.5, 0.2],
    [0.8, 0.1, 0.1],
    [0.2, 0.7, 0.1]
]

emissions1 = [('R', 0.3), ('G', 0.7)]
emissions2 = [('G', 0.8), ('B', 0.2)]
emissions3 = [('R', 0.9), ('G', 0.1)]

s1 = HS 1 emissions1
s2 = HS 2 emissions2
s3 = HS 3 emissions3

hmm = HMM [s1, s2, s3] transitions