module HiddenMarkovModel (
  HiddenMarkovModel(HMM),
  HiddenState(HS),
  Transitions,
  Emissions,
  InitialStateDistribution,
  tuplefy,
  initialize,
  generate,
  forward,
  viterbi,
  viterbiTable
) where

import Data.List
import Data.Function

-- | A transition matrix of an HMM.
type Transitions = [[Double]]

-- | An emission probability distribution for a hidden state.
type Emissions = [(Double, Char)]

-- | A probability distribution for the initial hidden state.
type InitialStateDistribution = [Double]

-- | A hidden state holding its identity (its index in the transition matrix) and its emission distribution.
data HiddenState = HS Int Emissions deriving Show

-- | An HMM is specified by its current state, list of hidden states, and the transition matrix.
data HiddenMarkovModel = HMM HiddenState [HiddenState] Transitions deriving Show

-- | Group a list of floats into pairs, drop the last element if the list has uneven length.
tuplefy :: [Double] -> [(Double, Double)]
tuplefy [x] = []
tuplefy [x, y] = [(x, y)]
tuplefy (x:y:xs) = (x, y) : tuplefy xs

-- | Given a list of floats x_i, calculate list of cumulative values y_i = \sum_{j = 1}^i x_j.
cumulative :: [Double] -> [Double]
cumulative [x] = [x]
cumulative (x:xs) = x:(map (+ x) (cumulative xs))

-- | Produce a cumulative probability distribution given an emission distribution.
cumulativeEmissions :: Emissions -> Emissions
cumulativeEmissions es = zip (cumulative $ map fst es) (map snd es)

-- | Given an int i and an HMM, transition the HMM to the state with index i.
transition :: Int -> HiddenMarkovModel -> HiddenMarkovModel
transition next (HMM _ states trans) = HMM (states !! next) states trans

-- | Given a float, state index and transition matrix, get the index of the next state determined by the float.
nextStateIndex :: Double -> Int -> Transitions -> Int
nextStateIndex f state trans = length $ takeWhile (\x -> f > x) $ cumulative (trans !! state)

-- | Transition an HMM according to a float.
nextState :: Double -> HiddenMarkovModel -> HiddenMarkovModel
nextState f hmm@(HMM (HS index _) _ trans) = transition (nextStateIndex f index trans) hmm

-- | Given a floating point number and an HMM, generate an emission character corresponding to the current hidden state of the HMM.
emit :: Double -> HiddenMarkovModel -> Char
emit f (HMM (HS _ es) _ _) = snd $ head $ dropWhile (\(x, y) -> f > x) $ cumulativeEmissions es

-- | Initialize an HMM given a float using an initial state distribution.
initialize :: Double -> InitialStateDistribution -> HiddenMarkovModel -> HiddenMarkovModel
initialize f dist (HMM _ states trans) = HMM (states !! initState) states trans
  where initState = length $ takeWhile (\x -> f > x) $ cumulative dist

-- | Given a list of float 2-tuples and an HMM, emit a character according to the first float,
--   then transition the HMM according to the second, repeat for each tuple in the list.
generate :: [(Double, Double)] -> HiddenMarkovModel -> String
generate [] hmm = ""
generate (x:xs) hmm = [emit (fst x) hmm] ++ generate xs (nextState (snd x) hmm)

-- | Given a state, an emission distribution and a char,
--   find the probability of the given char being emitted from the given state.
emissionProb :: HiddenState -> Char -> Double
emissionProb (HS _ es) c = if elem c (map snd es) then fst $ filter (\(_, y) -> y == c) es !! 0 else 0.0

-- | Get emission probabilities for a given character by HMM states.
emissionProbs :: Char -> [HiddenState] -> [Double]
emissionProbs c hs = map (\x -> emissionProb x c) hs

-- | Given an HMM and two states, find the probability of transitioning from first state to second state.
transitionProb :: HiddenMarkovModel -> HiddenState -> HiddenState -> Double
transitionProb (HMM _ _ trans) (HS i _) (HS j _) = (trans !! i) !! j

-- | Given a character, HMM, and initial state distribution,
--   return a list p_i of probabilities that the character was emitted from state i
initialEmissionProb :: Char -> HiddenMarkovModel -> InitialStateDistribution -> [Double]
initialEmissionProb c (HMM _ states _) initDist = zipWith (*) initDist $ emissionProbs c states

forwardList :: String -> HiddenMarkovModel -> InitialStateDistribution -> [Double]
forwardList [c] hmm initDist = initialEmissionProb c hmm initDist
forwardList (c:cs) hmm@(HMM _ states trans) initDist = zipWith (*) (emissionProbs c states) $ map sum transitionProbs
  where transitionProbs = zipWith (zipWith (*)) (transpose trans) (replicate (length states) (forwardList cs hmm initDist))

-- | Forward Algorithm
--   Given an emitted string and an HMM with initial state distribution, compute the probability of random generation.
forward :: String -> HiddenMarkovModel -> InitialStateDistribution -> Double
forward cs hmm initDist = sum $ forwardList (reverse cs) hmm initDist

-- | Get the index of the maximum of a list.
argmax :: [Double] -> Int
argmax xs = snd $ head $ reverse $ sortBy (compare `on` fst) $ zip xs [0..]

-- | For state i, calculate a_{ji} \theta_{t - 1}(j) for 1 <= j <= #states.
prefixTransitionProbs :: HiddenMarkovModel -> HiddenState -> [Double] -> [Double]
prefixTransitionProbs hmm@(HMM _ states _) dest prefixProbs = zipWith (*) (map (\x -> transitionProb hmm x dest) states) prefixProbs

-- | For state i, calculate \theta_t(i).
viterbiTransitionProb :: Char -> HiddenMarkovModel -> HiddenState -> [Double] -> Double
viterbiTransitionProb c hmm dest prefixProbs = (emissionProb dest c) * (maximum $ prefixTransitionProbs hmm dest prefixProbs)

-- | Calculate the index of most likely previous state given prefix transition probabilities.
viterbiPrevState :: HiddenMarkovModel -> HiddenState -> [Double] -> Int
viterbiPrevState hmm dest prefixProbs = argmax $ prefixTransitionProbs hmm dest prefixProbs

-- | Generate list of likelihoods for state i being last state, along with most likely preceding state.
viterbiList :: String -> HiddenMarkovModel -> InitialStateDistribution -> [(Double, Int)]
viterbiList [c] hmm@(HMM _ states _) initDist = zip (initialEmissionProb c hmm initDist) (replicate (length states) (-1))
viterbiList (c:cs) hmm@(HMM _ states _) initDist = zip (map (\x -> viterbiTransitionProb c hmm x prefixProbs) states) (map (\x -> viterbiPrevState hmm x prefixProbs) states)
  where prefixProbs = map fst $ viterbiList cs hmm initDist

-- | Generate the dynamic programming table for the Viterbi algorithm with likelihood of the state sequence and previous state.
viterbiTable :: String -> HiddenMarkovModel -> InitialStateDistribution -> [[(Double, Int)]]
viterbiTable cs hmm initDist = map (\x -> viterbiList x hmm initDist) (init $ tails $ reverse $ cs)

-- | Read the sequence of most likely states from the Viterbi DP table.
viterbiStateSeq :: [Int] -> Int -> [[(Double, Int)]] -> [Int]
viterbiStateSeq acc _ [] = reverse acc
viterbiStateSeq acc prevIndex (x:xs) = viterbiStateSeq (acc ++ [prevIndex]) (snd $ x !! prevIndex) xs

-- | Viterbi Algorithm
--   Given an emitted string and a HMM description with initial state distribution,
--   compute the most likely sequence of hidden states that caused the emission.
viterbi :: String -> HiddenMarkovModel -> InitialStateDistribution -> [Int]
viterbi cs hmm initDist = viterbiStateSeq [] endState $ viterbiTable cs hmm initDist
  where endState = argmax $ map fst $ viterbiList (reverse cs) hmm initDist
