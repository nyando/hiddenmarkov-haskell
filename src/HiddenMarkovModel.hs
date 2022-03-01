type Transitions = [[Float]]
type Emissions = [(Float, Char)]
type InitialStateDistribution = [Float]

data HiddenState = HS Int Emissions deriving Show
data HiddenMarkovModel = HMM HiddenState [HiddenState] Transitions deriving Show

-- given a list of floats x_i,
-- calculate list of cumulative values y_i = \sum_{j = 1}^i x_j
cumulative :: [Float] -> [Float]
cumulative [x] = [x]
cumulative (x:xs) = x:(map (+ x) (cumulative xs))

-- given an emission distribution
-- produce cumulative distribution
cumulativeEmissions :: Emissions -> Emissions
cumulativeEmissions es = zip (cumulative $ map fst es) (map snd es)

-- given an int i and a HMM
-- transition the HMM to the state with index i
transition :: Int -> HiddenMarkovModel -> HiddenMarkovModel
transition next (HMM _ states trans) = HMM (states !! next) states trans

-- given a float, state index and transition matrix
-- get the index of the next state determined by the float
rollNextState :: Float -> Int -> Transitions -> Int
rollNextState f state trans = length $ takeWhile (\x -> f > x) $ cumulative (trans !! state)

-- transition a HMM according to a float
nextState :: Float -> HiddenMarkovModel -> HiddenMarkovModel
nextState f hmm@(HMM (HS index _) _ trans) = transition (rollNextState f index trans) hmm

-- given a floating point number and an HMM
-- generate an emission character corresponding to the current hidden state of the HMM
emit :: Float -> HiddenMarkovModel -> Char
emit f (HMM (HS _ es) _ _) = snd $ head $ dropWhile (\(x, y) -> f > x) $ cumulativeEmissions es

initialize :: Float -> InitialStateDistribution -> HiddenMarkovModel -> HiddenMarkovModel
initialize f dist (HMM _ states trans) = HMM (states !! initState) states trans
    where initState = length $ takeWhile (\x -> f > x) $ cumulative dist

generate :: [(Float, Float)] -> HiddenMarkovModel -> String
generate [] hmm = ""
generate (x:xs) hmm = [emit (fst x) hmm] ++ generate xs (nextState (snd x) hmm)

initial = [0.3, 0.2, 0.5]

transitions = [[0.3, 0.5, 0.2],
               [0.8, 0.1, 0.1],
               [0.2, 0.7, 0.1]]

emissions1 = [(0.3, 'R'), (0.7, 'G')]
emissions2 = [(0.8, 'G'), (0.2, 'B')]
emissions3 = [(0.9, 'R'), (0.1, 'G')]

s1 = HS 0 emissions1
s2 = HS 1 emissions2
s3 = HS 2 emissions3

testHMM = HMM s1 [s1, s2, s3] transitions

changes = [(0.8, 0.1), (0.7, 0.1), (0.2, 0.2)]

lol = generate changes (initialize 0.6 initial testHMM)