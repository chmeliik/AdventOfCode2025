module Main (main) where

parseInput :: String -> [(Int, Int)]
parseInput = map parse . splitOn ','
  where
    splitOn :: (Eq a) => a -> [a] -> [[a]]
    splitOn _ [] = []
    splitOn c s = let (h, t) = break (== c) s in h : splitOn c (drop 1 t)

    parse :: String -> (Int, Int)
    parse s =
      let (a, b) = break (== '-') s
       in (read a, read $ tail b)

nDigits :: Int -> Int
nDigits = length . takeWhile (> 0) . iterate (`div` 10)

part1 :: [(Int, Int)] -> Int
part1 = sum . map (sum . invalidIds)
  where
    invalidIds :: (Int, Int) -> [Int]
    invalidIds (a, b) = filter isInvalid [a .. b]

    isInvalid :: Int -> Bool
    isInvalid n =
      let x = 10 ^ (nDigits n `div` 2)
       in div n x == mod n x

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
