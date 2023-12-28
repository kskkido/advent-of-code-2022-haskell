module Main where

import RIO
import qualified RIO.List as List
import qualified Control.Monad.Trans.Except as Except
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified AdventOfCode.DaySeven.Core as Core
import qualified AdventOfCode.DaySeven.PartOne as PartOne
import qualified AdventOfCode.DaySeven.PartTwo as PartTwo

cdParser :: Parsec.Parser Core.Command
cdParser = do
  void $ Parsec.oneOf "$"
  void $ Parsec.many $ Parsec.char ' '
  void $ Parsec.string "cd"
  void $ Parsec.many $ Parsec.char ' '
  command <- Core.Cd <$> Parsec.manyTill Parsec.anyChar (Parsec.try $ Parsec.lookAhead Parsec.newline)
  void Parsec.newline <|> Parsec.eof
  pure command

lsParser :: Parsec.Parser Core.Command
lsParser = do
  void $ Parsec.oneOf "$"
  void $ Parsec.many $ Parsec.char ' '
  void $ Parsec.string "ls"
  void Parsec.newline
  files <- Parsec.many do
    file <- Parsec.choice
      [ do
          void $ Parsec.string "dir"
          void $ Parsec.many $ Parsec.char ' '
          Core.DirectoryMetadata <$> Parsec.manyTill Parsec.anyChar (Parsec.try $ Parsec.lookAhead Parsec.newline)
      , do
          digits <- Parsec.many1 Parsec.digit
          void $ Parsec.many $ Parsec.char ' '
          file <- Parsec.manyTill Parsec.anyChar (Parsec.try $ Parsec.lookAhead Parsec.newline)
          maybe (fail "Failed to parse digits") pure do
            size <- readMaybe digits :: Maybe Int
            pure $ Core.FileMetadata size file
      ]
    void Parsec.newline <|> Parsec.eof
    pure file
  pure $ Core.Ls files

commandsParser :: Parsec.Parser [Core.Command]
commandsParser = do
  Parsec.many $ Parsec.choice
    [ Parsec.try cdParser
    , Parsec.try lsParser
    ]

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
      case Parsec.parse commandsParser "input" inputText of
        Left err -> Except.throwE err
        Right val -> pure val
    do 
      let output = PartOne.main input
      liftIO do
        IO.putStrLn "PART ONE:"
        IO.print output
    do 
      let output = PartTwo.main input 70000000 30000000
      liftIO do
        IO.putStrLn "PART TWO:"
        IO.print output
  either IO.print (\_ -> IO.putStr "Success") result
