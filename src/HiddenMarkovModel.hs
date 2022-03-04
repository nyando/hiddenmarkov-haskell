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

-- given an HMM and two states,
-- find the probability of transitioning from first state to second state
transitionProb :: HiddenMarkovModel -> HiddenState -> HiddenState -> Double
transitionProb (HMM _ _ trans) (HS i _) (HS j _) = (trans !! i) !! j

-- given a character, HMM, and initial state distribution,
-- return a list p_i of probabilities that the character was emitted from state i
initialEmissionProb :: Char -> HiddenMarkovModel -> InitialStateDistribution -> [Double]
initialEmissionProb c (HMM _ states _) initDist = zipWith (*) initDist $ emissionProbs c states

forwardList :: String -> HiddenMarkovModel -> InitialStateDistribution -> [Double]
forwardList [c] hmm initDist = initialEmissionProb c hmm initDist
forwardList (c:cs) hmm@(HMM _ states trans) initDist = zipWith (*) (emissionProbs c states) $ map sum transitionProbs
  where 
    transitionProbs = zipWith (zipWith (*)) (transpose trans) (replicate (length states) (forwardList cs hmm initDist))

-- forward algorithm
-- given an emitted string and an HMM with initial state distribution,
-- compute probability of random generation
forward :: String -> HiddenMarkovModel -> InitialStateDistribution -> Double
forward cs hmm initDist = sum $ forwardList cs hmm initDist

argmax :: [Double] -> Int
argmax xs = snd $ head $ reverse $ sortBy (compare `on` fst) $ zip xs [0..]

-- for state i, calculate a_{ji} \theta_{t - 1}(j) for 1 <= j <= #states
prefixTransitionProbs :: HiddenMarkovModel -> HiddenState -> [Double] -> [Double]
prefixTransitionProbs hmm@(HMM _ states _) dest prefixProbs = zipWith (*) (map (\x -> transitionProb hmm x dest) states) prefixProbs

-- for state i, calculate \theta_t(i)
viterbiTransitionProb :: Char -> HiddenMarkovModel -> HiddenState -> [Double] -> Double
viterbiTransitionProb c hmm dest prefixProbs = (emissionProb dest c) * (maximum $ prefixTransitionProbs hmm dest prefixProbs)

-- calculate index of most likely previous state given prefix transition probabilities
viterbiPrevState :: HiddenMarkovModel -> HiddenState -> [Double] -> Int
viterbiPrevState hmm dest prefixProbs = argmax $ prefixTransitionProbs hmm dest prefixProbs

-- viterbi algorithm
-- given an emitted string and a HMM description with initial state distribution,
-- compute most likely sequence of hidden states that caused the emission
viterbiList :: String -> HiddenMarkovModel -> InitialStateDistribution -> [(Double, Int)]
viterbiList [c] hmm@(HMM _ states _) initDist = zip (initialEmissionProb c hmm initDist) (replicate (length states) (-1))
viterbiList (c:cs) hmm@(HMM _ states _) initDist = zip (map (\x -> viterbiTransitionProb c hmm x prefixProbs) states) (map (\x -> viterbiPrevState hmm x prefixProbs) states)
    where prefixProbs = map fst $ viterbiList cs hmm initDist

viterbi cs hmm initDist = viterbiList (reverse cs) hmm initDist

initial = [1.0, 0.0, 0.0]

transitions = [[0.0, 1.0, 0.0],
               [0.0, 0.0, 1.0],
               [1.0, 0.0, 0.0]]

emissions0 = [(1.0, 'R')]
emissions1 = [(1.0, 'G')]
emissions2 = [(1.0, 'B')]

s0 = HS 0 emissions0
s1 = HS 1 emissions1
s2 = HS 2 emissions2

testHMM = HMM s0 [s0, s1, s2] transitions