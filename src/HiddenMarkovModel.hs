-- transition matrix of a HMM
type Transitions = [[Float]]

-- emission probability distribution for a hidden state
type Emissions = [(Float, Char)]

-- probability distribution for the initial hidden state
type InitialStateDistribution = [Float]

-- hidden state holding identity (== index in transition matrix) and emission distribution
data HiddenState = HS Int Emissions deriving Show

-- HMM: current state, list of hidden states, transition matrix
data HiddenMarkovModel = HMM HiddenState [HiddenState] Transitions deriving Show

-- given a list of floats, group them in pairs
-- drop the last element if the list has uneven length
tuplefy :: [Float] -> [(Float, Float)]
tuplefy [x] = []
tuplefy [x, y] = [(x, y)]
tuplefy (x:y:xs) = (x, y) : tuplefy xs

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

-- initialize a HMM given a float using an initial state distribution
initialize :: Float -> InitialStateDistribution -> HiddenMarkovModel -> HiddenMarkovModel
initialize f dist (HMM _ states trans) = HMM (states !! initState) states trans
    where initState = length $ takeWhile (\x -> f > x) $ cumulative dist

-- given a list of float 2-tuples and an HMM, emit a character according to the first float
-- then transition the HMM according to the second, repeat for each tuple in the list
generate :: [(Float, Float)] -> HiddenMarkovModel -> String
generate [] hmm = ""
generate (x:xs) hmm = [emit (fst x) hmm] ++ generate xs (nextState (snd x) hmm)

-- given an emission distribution and a char,
-- find the probability of the given char being emitted
emissionProb :: HiddenState -> Char -> Float
emissionProb (HS _ es) c = if elem c (map snd es) then fst $ filter (\(_, y) -> y == c) es !! 0 else 0.0

-- given an HMM and indices of two states,
-- find the probability of transitioning from first state to second state
transitionProb :: HiddenMarkovModel -> Int -> Int -> Float
transitionProb (HMM _ _ trans) i j = (trans !! i) !! j

-- forward algorithm
-- given an emitted string and an HMM with initial state distribution,
-- compute probability of random generation
-- forwardList :: String -> HiddenMarkovModel -> InitialStateDistribution -> [Float]
-- forwardList [c] (HMM _ states _) initdist = zipWith (*) initdist $ map (\x -> emissionProb x c) states
-- forwardList (c:cs) hmm@(HMM _ states trans) initdist = 

-- viterbi algorithm
-- given an emitted string and a HMM description with initial state distribution,
-- compute most likely sequence of hidden states that caused the emission
-- viterbi :: String -> HiddenMarkovModel -> InitialStateDistribution -> [Int]

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