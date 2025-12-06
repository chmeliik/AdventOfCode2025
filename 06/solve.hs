module Main (main) where

import Data.Bifunctor (Bifunctor (second))
import Data.Char (isSpace)
import Data.List (transpose)

data Op = Add | Mul

parseInput :: String -> [(Op, [String])]
parseInput = map parseColumn . columns
  where
    columns = splitBy (all isSpace) . transpose . lines

    parseColumn :: [String] -> (Op, [String])
    parseColumn c = (parseOp (last (transpose c)), init (transpose c))

    parseOp :: String -> Op
    parseOp s
      | '+' `elem` s = Add
      | otherwise = Mul

    splitBy :: (a -> Bool) -> [a] -> [[a]]
    splitBy p xs =
      case break p xs of
        (h, []) -> [h]
        (h, _ : t) -> h : splitBy p t

eval :: Op -> [Int] -> Int
eval Add = sum
eval Mul = product

part1 :: [(Op, [String])] -> Int
part1 = sum . map (\(op, nums) -> eval op (map read nums))

part2 :: [(Op, [String])] -> Int
part2 = part1 . map (transpose `second`)

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
