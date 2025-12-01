module Main (main) where

parseInput :: String -> [Int]
parseInput = map parseLine . lines
  where
    parseLine :: String -> Int
    parseLine (direction : num) = if direction == 'L' then -n else n
      where
        n = read num

part1 :: [Int] -> Int
part1 = length . filter (== 0) . map (`mod` 100) . scanl (+) 50

part2 :: [Int] -> Int
part2 = sum . map countZerosPassed . pairwise . scanl (+) 50
  where
    countZerosPassed :: (Int, Int) -> Int
    countZerosPassed (a, b)
      | a < b = distance (a `div` 100) (b `div` 100)
      | otherwise = distance ((a - 1) `div` 100) ((b - 1) `div` 100)
      where
        distance a b = abs (b - a)

    pairwise :: [a] -> [(a, a)]
    pairwise = zip <$> id <*> tail

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
