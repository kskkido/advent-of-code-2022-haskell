module AdventOfCode.DayFive.PartOne where

import RIO hiding (to)
import qualified RIO.List as List
import qualified RIO.Map as Map
import qualified Data.Foldable as Foldable

type Input = (Map.Map Int [Char], [(Int, Int, Int)])

main :: Input -> [Char]
main (stacks, moves) =
  Foldable.foldl (\acc (n, from, to) -> move n from to acc) stacks moves &
  foldMap \stack -> fromMaybe [] do
    last <- List.headMaybe stack
    pure [last]

move :: Int -> Int -> Int -> Map.Map Int [Char] -> Map.Map Int [Char]
move n from to stacks = fromMaybe stacks do
  currStackA <- Map.lookup from stacks
  currStackB <- Map.lookup to stacks
  let (crates, nextStackA) = List.splitAt n currStackA
      nextStackB = reverse crates ++ currStackB
  pure
    ( stacks &
      Map.insert from nextStackA &
      Map.insert to nextStackB
    )

