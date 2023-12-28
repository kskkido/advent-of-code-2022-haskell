module AdventOfCode.DaySeven.PartTwo where

import RIO hiding (to)
import qualified RIO.List as List
import qualified AdventOfCode.DaySeven.Core as Core

type Input = [Core.Command]

main :: Input -> Int -> Int -> Int
main input capacity target = fromMaybe 0 do
  directory <- Core.directoryFromCommands input
  let directorySizes = Core.directorySizesFromDirectory directory
  rootDirectorySize <- List.headMaybe directorySizes
  let freeSpace = capacity - rootDirectorySize
      directorySizesAesc = List.sort directorySizes
  List.find (\size -> size + freeSpace >= target) directorySizesAesc

