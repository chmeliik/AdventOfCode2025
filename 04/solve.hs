module Main (main) where

import Data.Set qualified as S

parseInput :: String -> S.Set (Int, Int)
parseInput s =
  S.fromList
    [ (x, y)
      | (x, line) <- zip [0 ..] (lines s),
        (y, c) <- zip [0 ..] line,
        c == '@'
    ]

surroundings :: (Int, Int) -> [(Int, Int)]
surroundings (x, y) =
  [ (x + dx, y + dy)
    | dx <- [-1 .. 1],
      dy <- [-1 .. 1],
      dx /= 0 || dy /= 0
  ]

part1 :: S.Set (Int, Int) -> Int
part1 papers = length $ filter isAccessible (S.elems papers)
  where
    isAccessible :: (Int, Int) -> Bool
    isAccessible p = length [p' | p' <- surroundings p, S.member p' papers] < 4

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
