module AdventOfCode.DayThree.PartTwo where

import RIO
import qualified RIO.List as List
import qualified Data.Monoid as Monoid
import qualified Data.Foldable as Foldable

type Input = [[Int]]

main :: Input -> Int
main input = Foldable.sum do
  group <- chunk 3 input
  pure $ fromMaybe 0 do
    head <- List.headMaybe group
    tail <- List.tailMaybe group
    flip findMap head \n -> do
      guard $ Monoid.getAll $ foldMap (Monoid.All . (n `elem`)) tail
      pure n

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk i xs = take i xs : chunk i (drop i xs)

findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap f = listToMaybe . mapMaybe f
