module Main (main) where

import Data.Bits (Bits (shiftL, xor, (.|.)))
import Data.IntSet qualified as S
import Data.List (foldl')
import Data.Maybe (fromJust, isNothing, listToMaybe, mapMaybe)

parseInput :: String -> [(Int, S.IntSet)]
parseInput = map parseLine . lines
  where
    parseLine :: String -> (Int, S.IntSet)
    parseLine line = (parseLights indicatorLights, S.fromList $ map parseButton buttons)
      where
        (indicatorLights : tail) = words line
        buttons = init tail

    parseLights :: String -> Int
    parseLights s = asBinaryShifts $ reverse indices
      where
        lights = init (tail s)
        indices = map fst . filter ((== '#') . snd) $ zip [0 ..] lights

    parseButton :: String -> Int
    parseButton s = asBinaryShifts nums
      where
        nums = map read $ splitOn ',' $ tail (init s)

    splitOn :: (Eq a) => a -> [a] -> [[a]]
    splitOn _ [] = []
    splitOn c s = let (h, t) = break (== c) s in h : splitOn c (drop 1 t)

asBinaryShifts :: [Int] -> Int
asBinaryShifts = foldl' (\n d -> n .|. shiftL 1 d) 0

buttonsToPress' :: Int -> Int -> S.IntSet -> Maybe [Int]
buttonsToPress' depth target availableButtons
  | depth == 0 || S.null availableButtons = Nothing
  | target `S.member` availableButtons = Just [target]
  | otherwise =
      let buttonsToTry = availableButtons
       in listToMaybe $
            mapMaybe
              ( \button ->
                  ( let bs =
                          buttonsToPress'
                            (depth - 1)
                            (target `xor` button)
                            (S.delete button buttonsToTry)
                     in if isNothing bs then Nothing else Just (button : fromJust bs)
                  )
              )
              (S.elems buttonsToTry)

buttonsToPress :: Int -> S.IntSet -> [Int]
buttonsToPress target availableButtons =
  fromJust $
    listToMaybe $
      mapMaybe (\depth -> buttonsToPress' depth target availableButtons) [1 ..]

part1 :: [(Int, S.IntSet)] -> Int
part1 = sum . map (length . uncurry buttonsToPress)

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
