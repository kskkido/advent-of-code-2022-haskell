module AdventOfCode.DayEight.Core where

import RIO
import qualified RIO.Map as Map
import qualified Data.Foldable as Foldable

type Forest = Map (Int,Int) Int

forestFromGrid :: [[Int]] -> Forest
forestFromGrid grid = Map.fromList do
  (row, a) <- zip grid [0..]
  (col, b) <- zip row [0..]
  pure ((a,b), col)

filterLeft :: (Int,Int) -> Forest -> Forest
filterLeft (rowA, colA) = Map.filterWithKey predicate
  where predicate (rowB, colB) _ = Foldable.and
          [ rowA == rowB
          , colA > colB
          ]

filterRight :: (Int,Int) -> Forest -> Forest
filterRight (rowA, colA) = Map.filterWithKey predicate
  where predicate (rowB, colB) _ = Foldable.and
          [ rowA == rowB
          , colB > colA
          ]

filterTop :: (Int,Int) -> Forest -> Forest
filterTop (rowA, colA) = Map.filterWithKey predicate
  where predicate (rowB, colB) _ = Foldable.and
          [ colA == colB
          , rowA > rowB
          ]

filterBottom :: (Int,Int) -> Forest -> Forest
filterBottom (rowA, colA) = Map.filterWithKey predicate
  where predicate (rowB, colB) _ = Foldable.and
          [ colA == colB
          , rowB > rowA
          ]

compareRow :: ((Int,Int), Int) -> ((Int,Int), Int) -> Ordering
compareRow ((rowA, _), _) ((rowB, _), _) = compare rowA rowB

compareCol :: ((Int,Int), Int) -> ((Int,Int), Int) -> Ordering
compareCol ((_, colA), _) ((_, colB), _) = compare colA colB

