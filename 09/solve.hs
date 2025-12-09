module Main (main) where

parseInput :: String -> [(Int, Int)]
parseInput = map parse . lines
  where
    parse :: String -> (Int, Int)
    parse s = let (a, _ : b) = break (== ',') s in (read a, read b)

rectangleArea :: (Int, Int) -> (Int, Int) -> Int
rectangleArea (x1, y1) (x2, y2) = tileDist x1 x2 * tileDist y1 y2
  where
    tileDist a b = 1 + abs (b - a)

part1 :: [(Int, Int)] -> Int
part1 redTiles = maximum [rectangleArea a b | a <- redTiles, b <- redTiles]

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
