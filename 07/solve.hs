module Main (main) where

import Control.Monad.State (State, evalState, gets, modify)
import Data.List (elemIndex, elemIndices)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S

parseInput :: String -> ((Int, Int), S.Set (Int, Int))
parseInput s = (startPos, splitters)
  where
    startPos = (0, fromJust $ elemIndex 'S' (head $ lines s))
    splitters =
      S.fromList
        [ (i + 1, j)
          | (i, line) <- zip [0 ..] (filterIndex odd $ tail $ lines s),
            j <- elemIndices '^' line
        ]

    filterIndex :: (Int -> Bool) -> [a] -> [a]
    filterIndex p xs = map snd $ filter (p . fst) $ zip [0 ..] xs

type Cached i o = State (M.Map i o) o

withCached :: (Ord i) => i -> (i -> Cached i o) -> Cached i o
withCached i f = do
  cached <- gets (M.lookup i)
  case cached of
    Just o -> pure o
    Nothing -> do
      o <- f i
      modify (M.insert i o)
      pure o

runCached :: Cached i o -> o
runCached = (`evalState` M.empty)

countSplitterHits :: (Int, Int) -> S.Set (Int, Int) -> M.Map (Int, Int) Int
countSplitterHits startPos splitters = runCached (splittersHit startPos)
  where
    lastPos = S.findMax splitters

    splittersHit :: (Int, Int) -> Cached (Int, Int) (M.Map (Int, Int) Int)
    splittersHit pos
      | pos > lastPos = pure M.empty
      | otherwise = withCached pos $ \(x, y) ->
          if pos `S.notMember` splitters
            then splittersHit (x + 1, y)
            else do
              splitters <-
                M.unionWith (+)
                  <$> splittersHit (x + 1, y - 1)
                  <*> splittersHit (x + 1, y + 1)
              pure (M.insert (x, y) 1 splitters)

part1 :: ((Int, Int), S.Set (Int, Int)) -> Int
part1 = M.size . uncurry countSplitterHits

part2 :: ((Int, Int), S.Set (Int, Int)) -> Int
part2 = (+ 1) . sum . uncurry countSplitterHits

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
