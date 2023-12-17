module AdventOfCode.DaySix.PartTwo where

import RIO hiding (to)
import qualified RIO.List as List

type Input = [Char]

main :: Input -> Int
main input = fromMaybe 0 do
  let as = zip input [0..]
  (_,i) <- find as
  pure (i + 1)

find :: [(Char, Int)] -> Maybe (Char, Int)
find [] = Nothing
find (x:xs) = asum
  [ do
      let range = x : take 13 xs
      guard $ length (List.nubBy ((==) `on` fst) range) == 14
      List.lastMaybe range
  , find xs
  ]
