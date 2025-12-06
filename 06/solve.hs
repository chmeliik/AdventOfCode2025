module Main (main) where

import Data.List (transpose)

data Op = Add | Mul

parseInput :: String -> [(Op, [Int])]
parseInput = map (parse . reverse) . transpose . map words . lines
  where
    parse :: [String] -> (Op, [Int])
    parse (op : nums) = (if op == "+" then Add else Mul, map read nums)

eval :: Op -> [Int] -> Int
eval Add = sum
eval Mul = product

part1 :: [(Op, [Int])] -> Int
part1 = sum . map (uncurry eval)

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
