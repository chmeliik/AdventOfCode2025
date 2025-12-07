module Main (main) where

import Data.IntSet qualified as S
import Data.List (elemIndex, elemIndices, foldl')
import Data.Maybe (fromJust)

parseInput :: String -> (Int, [S.IntSet])
parseInput s = (startPos, map S.fromList splitters)
  where
    startPos = fromJust $ elemIndex 'S' (head $ lines s)
    splitters = map (elemIndices '^') (filterIndex odd $ tail $ lines s)

filterIndex :: (Int -> Bool) -> [a] -> [a]
filterIndex p xs = map snd $ filter (p . fst) $ zip [0 ..] xs

part1 :: (Int, [S.IntSet]) -> Int
part1 (startPos, splitters) = numSplits
  where
    (numSplits, _) = foldl' go (0, S.singleton startPos) splitters

    go :: (Int, S.IntSet) -> S.IntSet -> (Int, S.IntSet)
    go (nSplits, beams) splittersOnThisRow =
      (nSplits + S.size hits, S.union misses splitBeams)
      where
        (hits, misses) = S.partition (`S.member` splittersOnThisRow) beams
        splitBeams = S.fromList $ concatMap (\n -> [n - 1, n + 1]) $ S.elems hits

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
