module Main (main) where

import Data.List (foldl', sort, sortOn)
import Data.Map qualified as M

type Box = (Float, Float, Float)

parseInput :: String -> [Box]
parseInput = map parse . lines
  where
    parse :: String -> Box
    parse s = (read a, read b, read c)
      where
        (a, _ : s') = break (== ',') s
        (b, _ : c) = break (== ',') s'

distance :: Box -> Box -> Float
distance (a1, b1, c1) (a2, b2, c2) = sqrt ((a2 - a1) ^ 2 + (b2 - b1) ^ 2 + (c2 - c1) ^ 2)

pairsByDistance :: [Box] -> [(Box, Box)]
pairsByDistance boxes =
  map (\(_, a, b) -> (a, b)) $
    sort
      [ (distance a b, a, b)
        | (i, a) <- zip [0 ..] boxes,
          (j, b) <- zip [0 ..] boxes,
          j > i
      ]

connectCircuits :: [(Box, Box)] -> M.Map Box Int
connectCircuits = foldl' addPair M.empty
  where
    addPair :: M.Map Box Int -> (Box, Box) -> M.Map Box Int
    addPair circuits (a, b) =
      let cA = M.lookup a circuits
          cB = M.lookup b circuits
       in case (cA, cB) of
            (Nothing, Nothing) -> add a newID $ add b newID circuits
            (Just aID, Nothing) -> add b aID circuits
            (Nothing, Just bID) -> add a bID circuits
            (Just aID, Just bID) ->
              M.map
                (\cID -> if cID == bID then aID else cID)
                circuits
      where
        add = M.insert
        newID = M.size circuits

circuitSizes :: M.Map Box Int -> M.Map Int Int
circuitSizes = M.fromListWith (+) . map (,1) . M.elems

part1 :: [Box] -> Int
part1 =
  product
    . take 3
    . sortOn ((-1) *)
    . M.elems
    . circuitSizes
    . connectCircuits
    . take 1000
    . pairsByDistance

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
