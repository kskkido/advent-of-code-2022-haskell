module AdventOfCode.DayEight.PartTwo where

import RIO hiding (product)
import qualified RIO.Map as Map
import qualified RIO.List as List
import qualified Data.Foldable as Foldable
import qualified AdventOfCode.DayEight.Core as Core

type Input = [[Int]]

main :: Input -> Int
main rows = Foldable.maximum do 
  let forest = Core.forestFromGrid rows
  (positionA, heightA) <- Map.toList forest
  let product = Foldable.product
        [ length do
            let trees = List.sortBy (flip Core.compareCol) $ Map.toList $ Core.filterLeft positionA forest
            takeWhileInclusive (\(_, heightB) -> heightA > heightB) trees
        , length do
            let trees = List.sortBy Core.compareCol $ Map.toList $ Core.filterRight positionA forest
            takeWhileInclusive (\(_, heightB) -> heightA > heightB) trees
        , length do
            let trees = List.sortBy (flip Core.compareRow) $ Map.toList $ Core.filterTop positionA forest
            takeWhileInclusive (\(_, heightB) -> heightA > heightB) trees
        , length do
            let trees = List.sortBy Core.compareRow $ Map.toList $ Core.filterBottom positionA forest
            takeWhileInclusive (\(_, heightB) -> heightA > heightB) trees
        ]
  pure product

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []
