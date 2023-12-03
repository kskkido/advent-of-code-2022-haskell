module AdventOfCode.DayOne where

import RIO
import qualified RIO.List as List
import qualified Data.Foldable as Foldable

type Input a = [[a]]

main :: (Ord a, Num a) => Input a -> [a]
main = List.sortBy (flip compare) . fmap Foldable.sum
