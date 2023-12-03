module AdventOfCode.DayFour.PartOne where

import RIO

type Input = [((Int,Int), (Int,Int))]

main :: Input -> [((Int,Int), (Int,Int))]
main input = do
  pair@((xa,xb),(ya,yb)) <- input
  guard ((ya <= xa && xb <= yb) || (xa <= ya && yb <= xb))
  pure pair

