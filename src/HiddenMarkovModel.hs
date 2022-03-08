{- |

  Module      :  HiddenMarkovModel
  Description :  Data structure and algorithms for discrete hidden Markov models (HMMs).
  
  Stability   :  unstable
  Portability :  portable

  This module implements a simple data structure for hidden Markov models with discretely distributed emission symbols.
  Given an HMM, the 'forward' and 'backward' functions allow the user to solve the evaluation problem for a given realization.
  The 'viterbi' function implements a Viterbi algorithm for finding the most likely path of hidden states for a given realization.
  Finally, 'baumWelch' implements the Baum-Welch iterative learning procedure for HMMs under a single emission string input.

-}

module HiddenMarkovModel (
  HiddenMarkovModel(HMM),
  HiddenState(HS),
  Transitions,
  Emissions,
  InitialStateDistribution,
  tuplefy,
  initialize,
  emissions,
  emissionProb,
  generate,
  forward,
  backward,
  viterbi,
  viterbiTable,
  baumWelch
) where

import Data.List
import Data.Function
import HMMHelpers

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
emissionProb (HS _ es) c = if elem c (map snd es) then fst $ head $ filter (\(_, y) -> y == c) es else 0.0

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

-- | = Forward Algorithm
--   Given an emitted string and an HMM with initial state distribution, compute the probability of random generation.
--   Do this by running through the string, computing the probability of being in any state at time t given a prefix of the emitted string.
forward :: String -> HiddenMarkovModel -> InitialStateDistribution -> Double
forward cs hmm initDist = sum $ forwardList (reverse cs) hmm initDist

backwardVariable :: Char -> HiddenMarkovModel -> HiddenState -> [Double] -> Double
backwardVariable c (HMM _ states trans) (HS i _) backs = sum $ zipWith (*) (zipWith (*) (map (\x -> emissionProb x c) states) (trans !! i)) backs

backwardList :: String -> HiddenMarkovModel -> [Double]
backwardList [] (HMM _ states _) = replicate (length states) 1
backwardList (c:cs) hmm@(HMM _ states _) = map (\x -> backwardVariable c hmm x (backwardList cs hmm)) states

-- | = Backward Algorithm
--   Given an emitted string and an HMM with initial state distribution, compute the probability of random generation.
--   Do this by running through the string backwards, computing the probability of generating a suffix of the emitted string from any state at time t.
backward :: String -> HiddenMarkovModel -> InitialStateDistribution -> Double
backward cs hmm@(HMM _ states _) initDist = sum $ zipWith (*) (zipWith (*) initDist (backwardList (tail cs) hmm)) (emissionProbs (head cs) states)

-- | For state i, calculate \[ a_{ji} \theta_{t - 1}(j) \] for 1 <= j <= #states.
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

-- | = Viterbi Algorithm
--   Given an emitted string and a HMM description with initial state distribution,
--   compute the most likely sequence of hidden states that caused the emission.
viterbi :: String -> HiddenMarkovModel -> InitialStateDistribution -> [Int]
viterbi cs hmm initDist = viterbiStateSeq [] endState $ viterbiTable cs hmm initDist
  where endState = argmax $ map fst $ viterbiList (reverse cs) hmm initDist

-- | Compute a table of values where the forward variable \alpha_i(t) is at index (t, i) in the resulting table.
forwardTable :: String -> HiddenMarkovModel -> InitialStateDistribution -> [[Double]]
forwardTable cs hmm initDist = reverse $ map (\x -> forwardList x hmm initDist) $ (init $ tails $ reverse $ cs)

-- | Compute a table of values where the backward variable \beta_i(t) is at index (t, i) in the resulting table.
backwardTable :: String -> HiddenMarkovModel -> InitialStateDistribution -> [[Double]]
backwardTable cs hmm initDist = map (\x -> backwardList x hmm) $ (init $ tails $ cs)

-- | Table of products of forward and backward variables.
forwardBackwardTable :: String -> HiddenMarkovModel -> InitialStateDistribution -> [[Double]]
forwardBackwardTable cs hmm initDist = map (\(x, y) -> zipWith (*) x y) $ zip fwTable bwTable
  where fwTable = forwardTable cs hmm initDist
        bwTable = backwardTable cs hmm initDist

-- | For a list of lists, scale values such that the sum of each sublist is 1.
normalizeList :: [[Double]] -> [[Double]]
normalizeList xs = map (\(x, y) -> zipWith (/) x y) (zip xs sums)
  where sums = map (replicate (length $ head xs)) $ map sum xs

-- | Probability of being in state i at time t.
--   Compute \[ \gamma_i(t) = \frac{\alpha_i(t) \beta_i(t)}{\sum_{j = 1}^n \alpha_j(t) \beta_j(t)} \].
gammaTable :: String -> HiddenMarkovModel -> InitialStateDistribution -> [[Double]]
gammaTable cs hmm initDist = normalizeList $ forwardBackwardTable cs hmm initDist

transitionPairs :: String -> HiddenMarkovModel -> InitialStateDistribution -> [([Double], [Double], Char)]
transitionPairs cs hmm initDist = zip3 (init fwTable) (tail bwTable) (tail cs)
  where fwTable = forwardTable cs hmm initDist
        bwTable = backwardTable cs hmm initDist

alphaBetaProduct :: HiddenMarkovModel -> ([Double], [Double], Char) -> Int -> Int -> Double
alphaBetaProduct hmm@(HMM _ states trans) alphaBeta i j = alpha * aij * beta * eprob
  where alpha = fst3 alphaBeta !! i
        beta  = snd3 alphaBeta !! j
        aij   = trans !! i !! j
        eprob = emissionProb (states !! j) (thd3 alphaBeta)

alphaBetaProducts :: HiddenMarkovModel -> ([Double], [Double], Char) -> [Double]
alphaBetaProducts hmm@(HMM _ states _) alphaBeta = map (\(i, j) -> alphaBetaProduct hmm alphaBeta i j) (pairIndices $ length states)

xiTable :: String -> HiddenMarkovModel -> InitialStateDistribution -> [[[Double]]]
xiTable cs hmm@(HMM _ states _) initDist = map (mat) $ normalizeList $ map (\x -> alphaBetaProducts hmm x) tps
  where mat = matrixify [] (length states) (length states)
        tps = transitionPairs cs hmm initDist

updateInit :: String -> HiddenMarkovModel -> InitialStateDistribution -> InitialStateDistribution
updateInit cs hmm initDist = head $ gammaTable cs hmm initDist

sumGammaTable :: [[Double]] -> [Double]
sumGammaTable gt = foldl (zipWith (+)) (head gt) (tail gt)

sumXiTable :: [[[Double]]] -> [[Double]]
sumXiTable xt = foldl (zipWith (zipWith (+))) (head xt) (tail xt)

-- | Update the transition matrix.
--   \[ a_{ij} = \frac{\sum_{t=0}^{T-1} \xi_{ij}(t)}{\sum_{t=0}^{T-1} \gamma_i(t)} \]
updateTransitions :: String -> HiddenMarkovModel -> InitialStateDistribution -> Transitions
updateTransitions cs hmm initDist = normalizeList $ map (\x -> zipWith (/) x gammaSum) $ xiSum
  where gammaSum = sumGammaTable $ gammaTable cs hmm initDist
        xiSum = sumXiTable $ xiTable cs hmm initDist

emissionIndicator :: Char -> String -> [[Double]] -> [[Double]]
emissionIndicator c cs table = map snd $ filter (\x -> fst x == c) $ zip cs table

updateStateCharEmissionProbs :: Char -> String -> [[Double]] -> [Double]
updateStateCharEmissionProbs c cs table = zipWith (/) (foldl (zipWith (+)) (head indicatorList) (tail indicatorList)) (sumGammaTable table)
  where indicatorList = emissionIndicator c cs table

updateAllCharEmissions :: String -> [[Double]] -> [Emissions]
updateAllCharEmissions cs table = map (\(x, y) -> zip x $ replicate (length x) y) statesByChar
  where statesByChar = zip (map (\x -> updateStateCharEmissionProbs x cs table) $ nub cs) (nub cs)

updateAllStateEmissions :: String -> [[Double]] -> Int -> [HiddenState]
updateAllStateEmissions cs table stateCount = map (emissionsByStateIndex) [0 .. (stateCount - 1)]
  where emissionsByStateIndex = \x -> HS x (map (!! x) $ updateAllCharEmissions cs table)

baumWelchIteration :: String -> HiddenMarkovModel -> InitialStateDistribution -> (HiddenMarkovModel, InitialStateDistribution)
baumWelchIteration cs hmm@(HMM _ states trans) initDist = ((HMM (head newStates) newStates newTrans), newInitDist)
  where newStates = updateAllStateEmissions cs (gammaTable cs hmm initDist) (length states)
        newTrans = updateTransitions cs hmm initDist
        newInitDist = updateInit cs hmm initDist

-- | = Baum-Welch Algorithm
--   Given a string of training data and an initialized HMM, estimate the HMM parameters that maximize the expected value of the string.
--   Note: The BW algorithm optimizes locally, there is no guarantee of optimality for all initial parameters of the HMM.
baumWelch :: String -> (HiddenMarkovModel, InitialStateDistribution) -> Double -> (HiddenMarkovModel, InitialStateDistribution)
baumWelch cs (hmm, initDist) limit = if (diff (hmm, initDist) iteration) < limit then iteration else baumWelch cs iteration limit
  where iteration = baumWelchIteration cs hmm initDist

-- | Get the discrete emission probability distribution for a single hidden state.
emissions :: HiddenState -> Emissions
emissions (HS _ es) = es

emissionDiffs :: [HiddenState] -> [HiddenState] -> [Double]
emissionDiffs s1 s2 = map (abs) $ zipWith (-) es1 es2
  where es1 = map fst $ concat $ map emissions s1
        es2 = map fst $ concat $ map emissions s2

diff :: (HiddenMarkovModel, InitialStateDistribution) -> (HiddenMarkovModel, InitialStateDistribution) -> Double
diff ((HMM _ s1 t1), d1) ((HMM _ s2 t2), d2) = transDiffSum + initDistDiffSum + emissionDiffSum
  where transDiffSum = matrixSum $ matrixDiff t1 t2
        initDistDiffSum = sum $ map (abs) $ zipWith (-) d1 d2
        emissionDiffSum = sum $ emissionDiffs s1 s2