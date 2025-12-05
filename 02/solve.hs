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

groupDigits :: Int -> Int -> [Int]
groupDigits len = map (`mod` (10 ^ len)) . takeWhile (> 0) . iterate (`div` (10 ^ len))

allEq :: (Eq a) => [a] -> Bool
allEq = all (uncurry (==)) . (zip <$> id <*> tail)

consistsOfNEqualGroups :: Int -> Int -> Bool
consistsOfNEqualGroups n nGroups =
  let d = nDigits n
   in d `mod` nGroups == 0 && allEq (groupDigits (d `div` nGroups) n)

invalidIDsInRange :: (Int -> Bool) -> (Int, Int) -> [Int]
invalidIDsInRange isInvalid (a, b) = filter isInvalid [a .. b]

part1 :: [(Int, Int)] -> Int
part1 = sum . map (sum . invalidIDsInRange isInvalid)
  where
    isInvalid :: Int -> Bool
    isInvalid = (`consistsOfNEqualGroups` 2)

part2 :: [(Int, Int)] -> Int
part2 = sum . map (sum . invalidIDsInRange isInvalid)
  where
    isInvalid :: Int -> Bool
    isInvalid n = any (n `consistsOfNEqualGroups`) [2 .. (nDigits n)]

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
