module Main where

import RIO
import qualified RIO.List as List
import qualified Control.Monad.Trans.Except as Except
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified AdventOfCode.DaySix.PartOne as PartOne
import qualified AdventOfCode.DaySix.PartTwo as PartTwo

parseInput :: Parsec.Parser [Char]
parseInput = do
  Parsec.manyTill Parsec.anyChar Parsec.eof

main :: IO.IO ()
main = do
  result <- Except.runExceptT do
    arguments <- liftIO do
      Environment.getArgs
    inputPath <-
      List.headMaybe arguments &
      maybe (Except.throwE "Input filepath not provided") pure
    inputText <- liftIO do
      IO.readFile inputPath
    input <- Except.withExceptT show do
      case Parsec.parse parseInput "input" inputText of
        Left err -> Except.throwE err
        Right val -> pure val
    do 
      let output = PartOne.main input
      liftIO do
        IO.putStrLn "PART ONE:"
        IO.print output
    do 
      let output = PartTwo.main input
      liftIO do
        IO.putStrLn "PART TWO:"
        IO.print output
  either IO.print (\_ -> IO.putStr "Success") result
