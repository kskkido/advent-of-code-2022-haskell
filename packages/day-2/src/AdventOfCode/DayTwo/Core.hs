module AdventOfCode.DayTwo.Core where

import RIO

data Shape =
    Rock
  | Paper
  | Scissor

data RoundResult =
    Win
  | Loss
  | Draw

type Input = [(Shape, Shape)]

shapeToScore :: Num a => Shape -> a
shapeToScore Rock = 1
shapeToScore Paper = 2
shapeToScore Scissor = 3

roundResultToScore :: Num a => RoundResult -> a
roundResultToScore Win = 6
roundResultToScore Loss = 0
roundResultToScore Draw = 3

playRound :: Shape -> Shape -> RoundResult
playRound Rock Paper = Win
playRound Rock Scissor = Loss
playRound Paper Scissor = Win
playRound Paper Rock = Loss
playRound Scissor Rock = Win
playRound Scissor Paper = Loss
playRound _ _ = Draw

roundToScore :: Num a => Shape -> Shape -> a
roundToScore m1 =
  (+)
  <$> shapeToScore
  <*> roundResultToScore . playRound m1
