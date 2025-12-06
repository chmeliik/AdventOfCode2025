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

removeAccessible :: S.Set (Int, Int) -> S.Set (Int, Int)
removeAccessible papers = S.filter (not . isAccessible) papers
  where
    isAccessible :: (Int, Int) -> Bool
    isAccessible p = length [p' | p' <- surroundings p, S.member p' papers] < 4

part1 :: S.Set (Int, Int) -> Int
part1 papers = length papers - length (removeAccessible papers)

part2 :: S.Set (Int, Int) -> Int
part2 papers = length papers - length (repeatedlyRemoveAccessible papers)
  where
    repeatedlyRemoveAccessible =
      fst
        . head
        . dropWhile ((/=) <$> fst <*> snd)
        . (zip <$> id <*> tail)
        . iterate removeAccessible

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
