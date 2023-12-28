module AdventOfCode.DaySeven.Core where

import RIO
import qualified RIO.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Foldable as Foldable

data Command = 
    Cd String
  | Ls [FileMetadata]
  deriving (Eq, Show)

data FileMetadata =
    FileMetadata Int String
  | DirectoryMetadata String
  deriving (Eq, Show)

data File = File Int String

data Directory = Directory String [FileNode]
  deriving (Eq, Show)

data FileNode = 
    FileNode Int String
  | DirectoryNode String [FileNode]
  deriving (Eq, Show)

directorySizesFromDirectory :: Directory -> [Int]
directorySizesFromDirectory (Directory _ nodes) = 
  let files = Maybe.mapMaybe fileNodeToFile nodes
      fileSizes = files <&> \(File size _) -> size
      subDirectories = Maybe.mapMaybe fileNodeToDirectory nodes
      subDirectorySizes = subDirectories <&> directorySizesFromDirectory
      directorySizes = Maybe.mapMaybe List.headMaybe subDirectorySizes
  in [Foldable.sum fileSizes + Foldable.sum directorySizes] <> concat subDirectorySizes

directoryFromCommands :: [Command] -> Maybe Directory
directoryFromCommands ((Cd name):xs) = Just $ Directory name $ fst $ fileNodesFromCommands xs
directoryFromCommands _ = Nothing

-- State Monad?
fileNodesFromCommands :: [Command] -> ([FileNode], [Command])
fileNodesFromCommands [] = ([],[])
fileNodesFromCommands ((Cd ".."):xs) = ([], xs)
fileNodesFromCommands ((Cd name):xs) = (DirectoryNode name ca : cb, rb)
  where (ca, ra) = fileNodesFromCommands xs
        (cb, rb) = fileNodesFromCommands ra
fileNodesFromCommands (command:xs) = (fileNodesFromCommand command <> ca, ra)
  where (ca, ra) = fileNodesFromCommands xs

fileNodesFromCommand :: Command -> [FileNode]
fileNodesFromCommand (Ls metadatas) = flip Maybe.mapMaybe metadatas \metadata -> do
  case metadata of
    FileMetadata size name -> Just $ FileNode size name
    _ -> Nothing
fileNodesFromCommand _ = []

fileNodeToDirectory :: FileNode -> Maybe Directory
fileNodeToDirectory (DirectoryNode name nodes) = Just $ Directory name nodes
fileNodeToDirectory _ = Nothing

fileNodeToFile :: FileNode -> Maybe File
fileNodeToFile (FileNode size name) = Just $ File size name
fileNodeToFile _ = Nothing

