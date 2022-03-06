module Main where

import Control.Monad
import HiddenMarkovModel
import HMMFileReader
import System.Environment
import System.IO
import System.Random

main = do
  g <- getStdGen
  args <- getArgs
  content <- readFile (args !! 1)
  
  let (hmm, initial) = parse $ lines content in
    case (args !! 0) of
      "print"      -> print $ hmm
      "generate"   -> let model = initialize (head (randoms g :: [Double])) initial hmm
                          randomRolls = tuplefy $ take (2 * (read (args !! 2) :: Int)) (randoms g :: [Double])
                            in print $ generate randomRolls model
      "forward"    -> print $ forward (args !! 2) hmm initial
      "backward"   -> print $ backward (args !! 2) hmm initial
      "viterbi"    -> print $ viterbi (args !! 2) hmm initial
      "baumWelch"  -> print $ baumWelch (args !! 2) (hmm, initial) (read (args !! 3) :: Double)