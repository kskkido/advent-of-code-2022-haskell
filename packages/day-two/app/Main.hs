module Main where

import RIO
import qualified RIO.List as List
import qualified Control.Monad.Trans.Except as Except
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified AdventOfCode.DayTwo.Core as DayTwo.Core
import qualified AdventOfCode.DayTwo.PartOne as DayTwo.PartOne
import qualified AdventOfCode.DayTwo.PartTwo as DayTwo.PartTwo
import qualified Text.ParserCombinators.Parsec as Parsec

validateOpponentInput :: Char -> Either String DayTwo.Core.Shape
validateOpponentInput char = asum
  [ maybe (Left "Invalid shape input") Right do
      guard $ char == 'A'
      pure DayTwo.Core.Rock
  , maybe (Left "Invalid shape input") Right do
      guard $ char == 'B'
      pure DayTwo.Core.Paper
  , maybe (Left "Invalid shape input") Right do
      guard $ char == 'C'
      pure DayTwo.Core.Scissor
  ]

validatePlayerInput :: Char -> Either String DayTwo.Core.Shape
validatePlayerInput char = asum
  [ maybe (Left "Invalid shape input") Right do
      guard $ char == 'X'
      pure DayTwo.Core.Rock
  , maybe (Left "Invalid shape input") Right do
      guard $ char == 'Y'
      pure DayTwo.Core.Paper
  , maybe (Left "Invalid shape input") Right do
      guard $ char == 'Z'
      pure DayTwo.Core.Scissor
  ]

validateRoundResultInput :: Char -> Either String DayTwo.Core.RoundResult
validateRoundResultInput char = asum
  [ maybe (Left "Invalid shape input") Right do
      guard $ char == 'X'
      pure DayTwo.Core.Loss
  , maybe (Left "Invalid shape input") Right do
      guard $ char == 'Y'
      pure DayTwo.Core.Draw
  , maybe (Left "Invalid shape input") Right do
      guard $ char == 'Z'
      pure DayTwo.Core.Win
  ]

parsePartOneInputText :: Parsec.Parser [(DayTwo.Core.Shape, DayTwo.Core.Shape)]
parsePartOneInputText = do
  Parsec.many do
    ca <- Parsec.anyChar
    void Parsec.space
    cb <- Parsec.anyChar
    void Parsec.newline <|> Parsec.eof
    either fail pure do
      opponent <- validateOpponentInput ca
      player <- validatePlayerInput cb
      pure (opponent,player)

parsePartTwoInputText :: Parsec.Parser [(DayTwo.Core.Shape, DayTwo.Core.RoundResult)]
parsePartTwoInputText = do
  Parsec.many do
    ca <- Parsec.anyChar
    void Parsec.space
    cb <- Parsec.anyChar
    void Parsec.newline <|> Parsec.eof
    either fail pure do
      opponent <- validateOpponentInput ca
      roundResult <- validateRoundResultInput cb
      pure (opponent,roundResult)

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
      let output = DayTwo.PartOne.main input
      liftIO do
        IO.putStrLn "PART ONE:"
        IO.print output
    do
      input <- Except.withExceptT show do
        case Parsec.parse parsePartTwoInputText "input" inputText of
          Left err -> Except.throwE err
          Right val -> pure val
      let output = DayTwo.PartTwo.main input
      liftIO do
        IO.putStrLn "PART TWO:"
        IO.print output
  either IO.print (\_ -> IO.putStr "Success") result
