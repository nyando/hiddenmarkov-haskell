module HMMFileReader (parse) where

import HiddenMarkovModel

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
  | (head $ words $ x) == "INIT"      = parseHMM hmm (parseInitialStateDistribution $ words x) [] 0 xs
  | (head $ words $ x) == "TRANS"     = parseHMM (HMM (HS 0 []) states (trans ++ [parseTransitionRow $ words x])) initDist [] 0 xs
  | (head $ words $ x) == "EMIT"      = parseHMM (HMM (HS 0 []) states trans) initDist (parseEmissionChars $ words x) 0 xs
  | (head $ words $ x) == "EMITSTATE" = parseHMM (HMM (HS 0 []) (states ++ [(HS stateCount (parseStateEmissions (words x) es))]) trans) initDist es (stateCount + 1) xs

parse :: [String] -> (HiddenMarkovModel, InitialStateDistribution)
parse strs = parseHMM (HMM (HS 0 []) [] []) [] [] 0 strs