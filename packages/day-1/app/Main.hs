module Main where

import RIO
import qualified RIO.List as List
import qualified Data.Foldable as Foldable
import qualified Control.Monad.Trans.Except as Except
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified AdventOfCode.DayOne
import qualified Text.ParserCombinators.Parsec as Parsec

parseGroupedCalories :: Parsec.Parser [[Int]]
parseGroupedCalories = do
  flip Parsec.sepBy Parsec.newline do
    Parsec.many do
      calorie <- do
        digits <- Parsec.many Parsec.digit
        maybe (fail "Failed to parse digit") pure do
          readMaybe digits
      void Parsec.newline <|> Parsec.eof
      pure calorie

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
      case Parsec.parse parseGroupedCalories "input" inputText of
        Left err -> Except.throwE err
        Right val -> pure val
    let groups = AdventOfCode.DayOne.main input
    liftIO do
      IO.print $ Foldable.maximum groups
      IO.print $ Foldable.sum $ take 3 groups
  either IO.print (\_ -> IO.putStr "Success") result
