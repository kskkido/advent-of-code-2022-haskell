module AdventOfCode.DayTwo.PartTwo where

import RIO
import qualified Data.Foldable as Foldable
import qualified AdventOfCode.DayTwo.Core as Core

type Input = [(Core.Shape, Core.RoundResult)]

playerFromOpponentByRoundResult :: Core.Shape -> Core.RoundResult -> Core.Shape
playerFromOpponentByRoundResult Core.Rock Core.Win = Core.Paper
playerFromOpponentByRoundResult Core.Rock Core.Loss = Core.Scissor
playerFromOpponentByRoundResult Core.Paper Core.Win = Core.Scissor
playerFromOpponentByRoundResult Core.Paper Core.Loss = Core.Rock
playerFromOpponentByRoundResult Core.Scissor Core.Win = Core.Rock
playerFromOpponentByRoundResult Core.Scissor Core.Loss = Core.Paper
playerFromOpponentByRoundResult shape _ = shape

main :: Input -> Int
main = Foldable.sum . fmap \(opponent, result) ->
  let player = playerFromOpponentByRoundResult opponent result
   in Core.roundToScore opponent player


