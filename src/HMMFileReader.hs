module HMMFileReader (
  parse,
  stringify
) where

import HiddenMarkovModel
import Data.List

data OutputState = INIT | TRANS | EMIT | EMITSTATE | DONE deriving Eq

parseInitialStateDistribution :: [String] -> InitialStateDistribution
parseInitialStateDistribution strs = map (read :: String -> Double) $ tail strs

parseTransitionRow :: [String] -> [Double]
parseTransitionRow strs = map (read :: String -> Double) $ drop 2 strs

parseEmissionChars :: [String] -> [Char]
parseEmissionChars chars = map head $ tail chars

parseStateEmissions :: [String] -> [Char] -> Emissions
parseStateEmissions row chars = filter (\(x, y) -> x > 0) $ zip (map (read :: String -> Double) $ drop 2 row) chars

parseHMM :: HiddenMarkovModel -> InitialStateDistribution -> [Char] -> Int -> [String] -> (HiddenMarkovModel, InitialStateDistribution)
parseHMM hmm@(HMM _ states trans) initDist es stateCount [] = ((HMM (states !! 0) states trans), initDist)
parseHMM hmm@(HMM _ states trans) initDist es stateCount (x:xs)
  | (words $ x)        == []          = parseHMM hmm initDist es stateCount xs
  | (head $ words $ x) == "INIT"      = parseHMM hmm (parseInitialStateDistribution $ words x) [] 0 xs
  | (head $ words $ x) == "TRANS"     = parseHMM (HMM (HS 0 []) states (trans ++ [parseTransitionRow $ words x])) initDist [] 0 xs
  | (head $ words $ x) == "EMIT"      = parseHMM (HMM (HS 0 []) states trans) initDist (parseEmissionChars $ words x) 0 xs
  | (head $ words $ x) == "EMITSTATE" = parseHMM (HMM (HS 0 []) (states ++ [(HS stateCount (parseStateEmissions (words x) es))]) trans) initDist es (stateCount + 1) xs

-- | Parse an HMM file.
--   The following syntax defines an example HMM with two states and two emission characters.
--
-- @
--    INIT 0.5 0.5
--    TRANS 0 0.7 0.3
--    TRANS 1 0.3 0.7
--    EMIT A B
--    EMITSTATE 0 0.5 0.5
--    EMITSTATE 1 0.5 0.5
-- @
--
parse :: [String] -> (HiddenMarkovModel, InitialStateDistribution)
parse strs = parseHMM (HMM (HS 0 []) [] []) [] [] 0 strs

concatTuple :: (String, String, String) -> String
concatTuple (a, b, c) = unwords [a, b, c]

createTransStrings :: Transitions -> [String]
createTransStrings trans = map concatTuple $ zip3 transKey index transVals
  where transKey  = repeat "TRANS"
        index     = map show [0..]
        transVals = map unwords $ map (map show) trans

emissionChars :: [HiddenState] -> [String]
emissionChars states = map (\x -> [x]) $ sort $ nub $ map snd $ concat $ map emissions states

charEmissionProbs :: HiddenState -> [String] -> [Double]
charEmissionProbs state chars = map (\x -> emissionProb state x) (concat chars) 

createEmitStateStrings :: [HiddenState] -> [String] -> [String]
createEmitStateStrings states chars = map concatTuple $ zip3 (repeat "EMITSTATE") (map show[0..]) (map (\x -> unwords $ map show $ charEmissionProbs x chars) states)

toStrings :: [String] -> OutputState -> (HiddenMarkovModel, InitialStateDistribution) -> [String]
toStrings acc DONE _ = acc
toStrings acc state (hmm@(HMM _ states trans), initDist)
  | state == INIT      = toStrings (acc ++ [unwords $ ["INIT"] ++ map show initDist]) TRANS (hmm, initDist)
  | state == TRANS     = toStrings (acc ++ createTransStrings trans) EMIT (hmm, initDist)
  | state == EMIT      = toStrings (acc ++ [unwords $ ["EMIT"] ++ emissionChars states]) EMITSTATE (hmm, initDist)
  | state == EMITSTATE = toStrings (acc ++ (createEmitStateStrings states $ emissionChars states)) DONE (hmm, initDist)

-- | Create an output string describing an HMM readable by this library.
--   See the documentation of 'parse' for a description of the syntax used.
stringify :: (HiddenMarkovModel, InitialStateDistribution) -> String
stringify hmmtuple = unlines $ toStrings [] INIT hmmtuple
