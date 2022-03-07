module HMMHelpers (
    argmax,
    tuplefy,
    cumulative,
    pairIndices,
    fst3,
    snd3,
    thd3,
    matrixify,
    matrixDiff,
    matrixSum
) where

import Data.List
import Data.Function

-- | Get the index of the maximum of a list.
argmax :: (Ord a) => [a] -> Int
argmax xs = snd $ head $ reverse $ sortBy (compare `on` fst) $ zip xs [0..]

-- | Group a list of elements into pairs, drop the last element if the list has uneven length.
tuplefy :: [a] -> [(a, a)]
tuplefy [x] = []
tuplefy [x, y] = [(x, y)]
tuplefy (x:y:xs) = (x, y) : tuplefy xs

-- | Given a list of numbers \[ x_i \], calculate list of cumulative values \[ y_i = \sum_{j = 1}^i x_j \].
cumulative :: (Num a) => [a] -> [a]
cumulative [x] = [x]
cumulative (x:xs) = x:(map (+ x) (cumulative xs))

-- | Get all index pairs for a given size n of a square matrix.
pairIndices :: Int -> [(Int, Int)]
pairIndices n = [(x, y) | x <- [0..(n - 1)], y <- [0..(n - 1)]]

-- | Return the first element of a 3-tuple.
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- | Return the second element of a 3-tuple.
snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

-- | Return the third element of a 3-tuple.
thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

-- | Given a list of (m x n) elements, return a (m x n) matrix containing the elements ordered into rows.
matrixify :: [[a]] -> Int -> Int -> [a] -> [[a]]
matrixify acc _ _ [] = acc
matrixify acc m n xs = matrixify (acc ++ [take n xs]) (m - 1) n (drop n xs)

-- | Given two equal-sized numeric matrices, return a matrix of absolute values of component-wise differences.
matrixDiff :: (Num a) => [[a]] -> [[a]] -> [[a]]
matrixDiff m1 m2 = map (map abs) $ zipWith (zipWith (-)) m1 m2

-- | Sum over all components of a matrix.
matrixSum :: (Num a) => [[a]] -> a
matrixSum m = sum $ foldl (zipWith (+)) (head m) (tail m)