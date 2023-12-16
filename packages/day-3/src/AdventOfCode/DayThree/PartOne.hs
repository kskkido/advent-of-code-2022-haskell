module AdventOfCode.DayThree.PartOne where

import RIO
import qualified RIO.List as List
import qualified Data.Foldable as Foldable

type Input = [([Int], [Int])]

main :: Input -> Int
main input = Foldable.sum do
  (left, right) <- input
  pure $ fromMaybe 0 $ List.find (`elem` right) left

