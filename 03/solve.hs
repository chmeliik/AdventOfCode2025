module Main (main) where

import Data.Char (digitToInt)
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.List (tails)

parseInput :: String -> [[Int]]
parseInput = map (map digitToInt) . lines

leftMostMaxBy :: (a -> a -> Ordering) -> [a] -> a
leftMostMaxBy f = minimumBy (flip f)

joltage :: [Int] -> Int
joltage bank =
  let m = leftMostMaxBy (compare `on` head) $ filter ((>= 2) . length) (tails bank)
   in 10 * head m + maximum (tail m)

part1 :: [[Int]] -> Int
part1 = sum . map joltage

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
