import Data.List
import Data.Function

-- transition matrix of a HMM
type Transitions = [[Double]]

-- emission probability distribution for a hidden state
type Emissions = [(Double, Char)]

-- probability distribution for the initial hidden state
type InitialStateDistribution = [Double]

-- hidden state holding identity (== index in transition matrix) and emission distribution
data HiddenState = HS Int Emissions deriving Show

-- HMM: current state, list of hidden states, transition matrix
data HiddenMarkovModel = HMM HiddenState [HiddenState] Transitions deriving Show

-- given a list of floats, group them in pairs
-- drop the last element if the list has uneven length
tuplefy :: [Double] -> [(Double, Double)]
tuplefy [x] = []
tuplefy [x, y] = [(x, y)]
tuplefy (x:y:xs) = (x, y) : tuplefy xs

-- given a list of floats x_i,
-- calculate list of cumulative values y_i = \sum_{j = 1}^i x_j
cumulative :: [Double] -> [Double]
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
nextStateIndex :: Double -> Int -> Transitions -> Int
nextStateIndex f state trans = length $ takeWhile (\x -> f > x) $ cumulative (trans !! state)

-- transition a HMM according to a float
nextState :: Double -> HiddenMarkovModel -> HiddenMarkovModel
nextState f hmm@(HMM (HS index _) _ trans) = transition (nextStateIndex f index trans) hmm

-- given a floating point number and an HMM
-- generate an emission character corresponding to the current hidden state of the HMM
emit :: Double -> HiddenMarkovModel -> Char
emit f (HMM (HS _ es) _ _) = snd $ head $ dropWhile (\(x, y) -> f > x) $ cumulativeEmissions es

-- initialize a HMM given a float using an initial state distribution
initialize :: Double -> InitialStateDistribution -> HiddenMarkovModel -> HiddenMarkovModel
initialize f dist (HMM _ states trans) = HMM (states !! initState) states trans
  where initState = length $ takeWhile (\x -> f > x) $ cumulative dist

-- given a list of float 2-tuples and an HMM, emit a character according to the first float
-- then transition the HMM according to the second, repeat for each tuple in the list
generate :: [(Double, Double)] -> HiddenMarkovModel -> String
generate [] hmm = ""
generate (x:xs) hmm = [emit (fst x) hmm] ++ generate xs (nextState (snd x) hmm)

-- given an emission distribution and a char,
-- find the probability of the given char being emitted
emissionProb :: HiddenState -> Char -> Double
emissionProb (HS _ es) c = if elem c (map snd es) then fst $ filter (\(_, y) -> y == c) es !! 0 else 0.0

emissionProbs :: Char -> [HiddenState] -> [Double]
emissionProbs c hs = map (\x -> emissionProb x c) hs

-- given an HMM and indices of two states,
-- find the probability of transitioning from first state to second state
transitionProb :: HiddenMarkovModel -> Int -> Int -> Double
transitionProb (HMM _ _ trans) i j = (trans !! i) !! j

forwardList :: String -> HiddenMarkovModel -> InitialStateDistribution -> [Double]
forwardList [c] (HMM _ states _) initdist = zipWith (*) initdist $ emissionProbs c states
forwardList (c:cs) hmm@(HMM _ states trans) initdist = zipWith (*) (emissionProbs c states) $ map sum transitionProbs
  where 
    transitionProbs = zipWith (zipWith (*)) (transpose trans) (replicate (length states) (forwardList cs hmm initdist))

-- forward algorithm
-- given an emitted string and an HMM with initial state distribution,
-- compute probability of random generation
forward :: String -> HiddenMarkovModel -> InitialStateDistribution -> Double
forward cs hmm initdist = sum $ forwardList cs hmm initdist

argmax :: [Double] -> Int
argmax xs = snd $ head $ reverse $ sortBy (compare `on` fst) $ zip xs [0..]

-- viterbi algorithm
-- given an emitted string and a HMM description with initial state distribution,
-- compute most likely sequence of hidden states that caused the emission
--viterbiList :: String -> HiddenMarkovModel -> InitialStateDistribution -> [(Double, Int)]
viterbiList [c] (HMM _ states _) initdist = (maximum $ zipWith (*) initdist $ emissionProbs c states, -1)
viterbiList (c:cs) hmm@(HMM _ states trans) initdist = (maximum $ zipWith (*) (emissionProbs c states) (map maximum transitionProbs), argmax $ zipWith (*) (emissionProbs c states) (map maximum transitionProbs))
--  where emissionSourceProbs = zipWith (*) (emissionProbs c states) (map maximum transitionProbs)
    where transitionProbs = zipWith (zipWith (*)) (transpose trans) (replicate (length states) (map fst $ viterbiList cs hmm initdist))
            

initial = [0.2, 0.3, 0.5]

transitions = [[0.4, 0.5, 0.1],
               [0.2, 0.3, 0.5],
               [0.4, 0.3, 0.3]]

emissions0 = [(0.3, 'R'), (0.5, 'G'), (0.2, 'B')]
emissions1 = [(0.6, 'R'), (0.2, 'G'), (0.2, 'B')]
emissions2 = [(0.1, 'R'), (0.1, 'G'), (0.8, 'B')]

s0 = HS 0 emissions0
s1 = HS 1 emissions1
s2 = HS 2 emissions2

testHMM = HMM s0 [s0, s1, s2] transitions