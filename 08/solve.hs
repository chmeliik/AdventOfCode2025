module Main (main) where

import Data.List (foldl', scanl', sort, sortOn)
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

type CircuitID = Int

addPair :: (Int, M.Map Box CircuitID) -> (Box, Box) -> (Int, M.Map Box CircuitID)
addPair (nc, circuits) (a, b) =
  let cA = M.lookup a circuits
      cB = M.lookup b circuits
      add = M.insert
      newID = M.size circuits
   in case (cA, cB) of
        (Nothing, Nothing) -> (nc + 1, add a newID $ add b newID circuits)
        (Just aID, Nothing) -> (nc, add b aID circuits)
        (Nothing, Just bID) -> (nc, add a bID circuits)
        (Just aID, Just bID)
          | aID == bID -> (nc, circuits)
          | otherwise ->
              ( nc - 1,
                M.map (\cID -> if cID == bID then aID else cID) circuits
              )

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
  where
    connectCircuits :: [(Box, Box)] -> M.Map Box CircuitID
    connectCircuits = snd . foldl' addPair (0, M.empty)

    circuitSizes :: M.Map Box CircuitID -> M.Map CircuitID Int
    circuitSizes = M.fromListWith (+) . map (,1) . M.elems

part2 :: [Box] -> Int
part2 boxes = truncate x1 * truncate x2
  where
    scanCircuits :: [(Box, Box)] -> [(Int, M.Map Box CircuitID)]
    scanCircuits = scanl' addPair (0, M.empty)

    pairs = pairsByDistance boxes

    stepsToConnectAll =
      length $
        takeWhile
          (\(nc, circuits) -> nc /= 1 || M.size circuits /= length boxes)
          (scanCircuits pairs)

    ((x1, _, _), (x2, _, _)) = pairs !! (stepsToConnectAll -1)

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
