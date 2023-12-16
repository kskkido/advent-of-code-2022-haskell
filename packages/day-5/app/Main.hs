module Main where

import RIO
import qualified RIO.List as List
import qualified RIO.Map as Map
import qualified Control.Monad.Trans.Except as Except
import qualified Data.Maybe as Maybe
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified AdventOfCode.DayFive.PartOne as PartOne
import qualified AdventOfCode.DayFive.PartTwo as PartTwo

-- (Parsec.try . Parsec.lookAhead) is your friend

parseCrateRow :: Parsec.Parser [Maybe Char]
parseCrateRow = do
  flip Parsec.sepBy1 (Parsec.char ' ') $ Parsec.choice
    [ Just <$> do
        void $ Parsec.char '['
        c <- Parsec.anyChar
        void $ Parsec.char ']'
        pure c
    ,  Nothing <$ do
        void Parsec.space
        void Parsec.space
        void Parsec.space
    ]

parseCrateRows :: Parsec.Parser [[Char]]
parseCrateRows = do
  rows <- Parsec.manyTill (parseCrateRow <* Parsec.newline) do
    Parsec.try $ Parsec.lookAhead parseStackLabelRow
  pure $ Maybe.catMaybes <$> List.transpose rows

parseStackLabelRow :: Parsec.Parser [Int]
parseStackLabelRow = do
  flip Parsec.manyTill (void Parsec.newline <|> Parsec.eof) do
    void $ Parsec.many1 Parsec.space
    cs <- Parsec.manyTill Parsec.digit Parsec.space
    maybe (fail "Failed to parse digit") pure do
      readMaybe cs :: Maybe Int

parseStacks :: Parsec.Parser (Map.Map Int [Char])
parseStacks = do
  crateRows <- parseCrateRows
  stackLabelRow <- parseStackLabelRow
  pure $ Map.fromList $ zip stackLabelRow crateRows

parseMoves :: Parsec.Parser [(Int, Int, Int)]
parseMoves = do
  flip Parsec.sepEndBy (void Parsec.newline <|> Parsec.eof) do
    void $ Parsec.string "move "
    ca <- Parsec.manyTill Parsec.digit (Parsec.try $ Parsec.lookAhead Parsec.space)
    void $ Parsec.string " from "
    cb <- Parsec.manyTill Parsec.digit (Parsec.try $ Parsec.lookAhead Parsec.space)
    void $ Parsec.string " to "
    cc <- Parsec.manyTill Parsec.digit (Parsec.try $ Parsec.lookAhead Parsec.space)
    maybe (fail "Failed to parse digits") pure do
      da <- readMaybe ca
      db <- readMaybe cb
      dc <- readMaybe cc
      pure (da, db, dc)

parseInput :: Parsec.Parser (Map.Map Int [Char], [(Int, Int, Int)])
parseInput = do
  stacks <- parseStacks
  void Parsec.newline
  moves <- parseMoves
  pure (stacks, moves)

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
