module Main where

import HiddenMarkovModel
import HMMExample
import System.Environment
import System.Random

main = do
  g <- getStdGen
  args <- getArgs

  case head args of
    "print"    -> print $ exampleHMM
    "generate" -> let model = initialize (head (randoms g :: [Double])) initial exampleHMM
                      randomRolls = tuplefy $ take (2 * (read (args !! 1) :: Int)) (randoms g :: [Double])
                        in print $ generate randomRolls model
    "forward"  -> print $ forward (args !! 1) exampleHMM initial
    "backward" -> print $ backward (args !! 1) exampleHMM initial
    "viterbi"  -> print $ viterbi (args !! 1) exampleHMM initial