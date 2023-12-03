module Main where

import RIO
import qualified RIO.List as List
import qualified Control.Monad.Trans.Except as Except
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified AdventOfCode.DayFour.PartOne as PartOne
import qualified AdventOfCode.DayFour.PartTwo as PartTwo


parseInputText :: Parsec.Parser [((Int,Int), (Int,Int))]
parseInputText = do
  Parsec.many do
    line <- do
      ca <- Parsec.many1 Parsec.digit
      void $ Parsec.char '-'
      cb <- Parsec.many1 Parsec.digit
      void $ Parsec.char ','
      cc <- Parsec.many1 Parsec.digit
      void $ Parsec.char '-'
      cd <- Parsec.many1 Parsec.digit
      maybe (fail "Failed to parse digit") pure do
        da <- readMaybe ca
        db <- readMaybe cb
        dc <- readMaybe cc
        dd <- readMaybe cd
        pure ((da,db),(dc,dd))
    void Parsec.newline <|> Parsec.eof
    pure line

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
      case Parsec.parse parseInputText "input" inputText of
        Left err -> Except.throwE err
        Right val -> pure val
    liftIO do
      let output = PartOne.main input
      IO.putStrLn "PART ONE:"
      IO.print $ length output
    liftIO do
      let output = PartTwo.main input
      IO.putStrLn "PART TWO:"
      IO.print $ length output
  either IO.print (\_ -> IO.putStr "Success") result
