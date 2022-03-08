{- |

  Module      :  Main
  Description :  Executable demo of the capabilities of the 'HiddenMarkovModel' library.
  
  Stability   :  unstable
  Portability :  portable

  This module implements a simple command line app for working with HMMs contained in small text files.
  The formatting of these HMMs is explained in the documentation for the 'HMMSerialization' module.
  The app takes a keyword, a filename with the HMM to operate on, and (potential) function arguments as command line arguments.
  It returns either its function's output or an HMM definition in the same format as its input.
  For example, calling

  @
    hmm baumWelch ./my_hmm ASDF 0.01
  @

  will output an HMM in the format specified in 'HMMSerialization' to 'stdout',
  where the output HMM is the output of a Baum-Welch algorithm running on the input HMM specified in 'my_hmm' and the emission string 'ASDF'.
  The procedure is iterated until the sum of all parameter differences converges to a value smaller than 0.01.

-}
module Main where

import Control.Monad
import HiddenMarkovModel
import HMMSerialization
import System.Environment
import System.IO
import System.Random

main = do
  g <- getStdGen
  args <- getArgs
  content <- readFile (args !! 1)
  
  let (hmm, initial) = parse content in
    case (args !! 0) of
      "print"      -> putStrLn $ stringify $ (hmm, initial)
      "generate"   -> let model = initialize (head (randoms g :: [Double])) initial hmm
                          randomRolls = tuplefy $ take (2 * (read (args !! 2) :: Int)) (randoms g :: [Double])
                            in print $ generate randomRolls model
      "forward"    -> print $ forward (args !! 2) hmm initial
      "backward"   -> print $ backward (args !! 2) hmm initial
      "viterbi"    -> print $ viterbi (args !! 2) hmm initial
      "baumWelch"  -> putStrLn $ stringify $ baumWelch (args !! 2) (hmm, initial) (read (args !! 3) :: Double)
