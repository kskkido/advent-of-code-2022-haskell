module AdventOfCode.DayEight.PartOne where

import RIO hiding (to)
import qualified RIO.Map as Map
import qualified Data.Foldable as Foldable
import qualified AdventOfCode.DayEight.Core as Core

type Input = [[Int]]

main :: Input -> Int
main rows = length do 
  let forest = Core.forestFromGrid rows
  (positionA, heightA) <- Map.toList forest
  guard $ Foldable.or
    [ Foldable.and do
        (_, heightB) <- Map.toList $ Core.filterLeft positionA forest
        pure $ heightA > heightB
    , Foldable.and do
        (_, heightB) <- Map.toList $ Core.filterRight positionA forest
        pure $ heightA > heightB
    , Foldable.and do
        (_, heightB) <- Map.toList $ Core.filterTop positionA forest
        pure $ heightA > heightB
    , Foldable.and do
        (_, heightB) <- Map.toList $ Core.filterBottom positionA forest
        pure $ heightA > heightB
    ]
