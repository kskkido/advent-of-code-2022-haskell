module AdventOfCode.DayTwo.PartOne where

import RIO
import qualified Data.Foldable as Foldable
import qualified AdventOfCode.DayTwo.Core as Core

type Input = [(Core.Shape, Core.Shape)]

main :: Input -> Int
main = Foldable.sum . fmap (uncurry Core.roundToScore)

