module AdventOfCode.DayFour.PartTwo where

import RIO

type Input = [((Int,Int), (Int,Int))]

main :: Input -> [((Int,Int), (Int,Int))]
main input = do
  pair@((xa,xb),(ya,yb)) <- input
  guard $ fromMaybe False $ asum
    [ guard $ between xa ya yb
    , guard $ between xb ya yb
    , guard $ between ya xa xb
    , guard $ between yb xa xb
    ] $> True
  pure pair

between :: Int -> Int -> Int -> Bool
between x start end = start <= x && x <= end
