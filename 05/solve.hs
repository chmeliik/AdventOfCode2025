module Main (main) where

parseInput :: String -> ([(Int, Int)], [Int])
parseInput s = (map parseRange ranges, map read ids)
  where
    (ranges, _ : ids) = break null (lines s)

    parseRange :: String -> (Int, Int)
    parseRange s = let (a, b) = break (== '-') s in (read a, read (tail b))

part1 :: ([(Int, Int)], [Int]) -> Int
part1 (ranges, ids) = length $ filter inRange ids
  where
    inRange :: Int -> Bool
    inRange n = any ((&&) <$> ((n >=) . fst) <*> ((n <=) . snd)) ranges

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
