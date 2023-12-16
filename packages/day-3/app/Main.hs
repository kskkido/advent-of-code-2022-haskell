module Main where

import RIO
import qualified RIO.Map as Map
import qualified RIO.List as List
import qualified Control.Monad.Trans.Except as Except
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified AdventOfCode.DayThree.PartOne as PartOne
import qualified AdventOfCode.DayThree.PartTwo as PartTwo

priorityByItemType :: Map Char Int
priorityByItemType = Map.fromList $ zip types [1..]
  where types = fold [['a'..'z'], ['A'..'Z']]

validateItemPriority :: Char -> Either String Int
validateItemPriority char = maybe (Left "Unknown item type") Right do
  Map.lookup char priorityByItemType

parsePartOneInputText :: Parsec.Parser [([Int], [Int])]
parsePartOneInputText = do
  Parsec.many do
    line <- Parsec.many1 Parsec.letter
    void Parsec.newline <|> Parsec.eof
    either fail pure do
      let i :: Int = floor (fromIntegral (length line) / 2.0 :: Double)
      priorities <- forM line validateItemPriority
      pure $ List.splitAt i priorities

parsePartTwoInputText :: Parsec.Parser [[Int]]
parsePartTwoInputText = do
  Parsec.many do
    line <- Parsec.many1 Parsec.letter
    void Parsec.newline <|> Parsec.eof
    either fail pure do
      forM line validateItemPriority

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
    do
      input <- Except.withExceptT show do
        case Parsec.parse parsePartOneInputText "input" inputText of
          Left err -> Except.throwE err
          Right val -> pure val
      let output = PartOne.main input
      liftIO do
        IO.putStrLn "PART ONE:"
        IO.print output
    do
      input <- Except.withExceptT show do
        case Parsec.parse parsePartTwoInputText "input" inputText of
          Left err -> Except.throwE err
          Right val -> pure val
      let output = PartTwo.main input
      liftIO do
        IO.putStrLn "PART TWO:"
        IO.print output
  either IO.print (\_ -> IO.putStr "Success") result
