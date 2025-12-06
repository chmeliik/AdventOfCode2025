module Main (main) where

import Data.List (foldl', partition)

parseInput :: String -> ([(Int, Int)], [Int])
parseInput s = (map parseRange ranges, map read ids)
  where
    (ranges, _ : ids) = break null (lines s)

    parseRange :: String -> (Int, Int)
    parseRange s = let (a, b) = break (== '-') s in (read a, read (tail b))

collapseOverlappingRanges :: [(Int, Int)] -> [(Int, Int)]
collapseOverlappingRanges = foldl' addRange []

addRange :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
addRange ranges range = newRange : nonOverlapping
  where
    (overlapping', nonOverlapping) = partition (hasOverlap range) ranges
    overlapping = range : overlapping'
    newRange = (minimum (map fst overlapping), maximum (map snd overlapping))

    hasOverlap :: (Int, Int) -> (Int, Int) -> Bool
    hasOverlap r1 r2 = leftOverlap r1 r2 || leftOverlap r2 r1
      where
        leftOverlap (a1, b1) (a2, b2) = a1 <= a2 && b1 >= a2

part1 :: ([(Int, Int)], [Int]) -> Int
part1 (ranges, ids) = length $ filter inRange ids
  where
    inRange :: Int -> Bool
    inRange n = any ((&&) <$> ((n >=) . fst) <*> ((n <=) . snd)) ranges

part2 :: ([(Int, Int)], [Int]) -> Int
part2 (ranges, _) = sum $ map (\(a, b) -> b - a + 1) (collapseOverlappingRanges ranges)

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
