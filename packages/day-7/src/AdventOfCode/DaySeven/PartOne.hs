module AdventOfCode.DaySeven.PartOne where

import RIO hiding (to)
import qualified Data.Foldable as Foldable
import qualified AdventOfCode.DaySeven.Core as Core

type Input = [Core.Command]

main :: Input -> Int
main input = fromMaybe 0 do
  directory <- Core.directoryFromCommands input
  let directorySizes = Core.directorySizesFromDirectory directory
  pure $ Foldable.sum $ filter (<= 100000) directorySizes

