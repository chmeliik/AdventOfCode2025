module Main (main) where

import Data.Char (digitToInt)
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.List (tails)

parseInput :: String -> [[Int]]
parseInput = map (map digitToInt) . lines

leftMostMaxBy :: (a -> a -> Ordering) -> [a] -> a
leftMostMaxBy f = minimumBy (flip f)

joltage :: Int -> [Int] -> Int
joltage 1 bank = maximum bank
joltage n bank =
  let m = leftMostMaxBy (compare `on` head) $ filter ((>= n) . length) (tails bank)
   in (10 ^ (n - 1)) * head m + joltage (n - 1) (tail m)

part1 :: [[Int]] -> Int
part1 = sum . map (joltage 2)

part2 :: [[Int]] -> Int
part2 = sum . map (joltage 12)

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
