module Parser where

import Control.Monad (when)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Parsec
import Text.Parsec.String

parseTextThenColon :: Parser String
parseTextThenColon
  = do
      text <- many (upper <|> lower)
      _ <- oneOf ":" >> oneOf "\n"
      return text

parseCommands :: Parser String -> Parser a -> Parser [String]
parseCommands intro end
  = do
      _ <- intro
      commands <- manyTill anyChar (try . lookAhead $ end)
      let strip = T.unpack . T.strip . T.pack
      return . lines . strip $ commands

parseBuildFile :: Parser ([String], [String])
parseBuildFile
  = do
      skipMany parseComment
      buildInstrs <- parseCommands (string "Build:") parseTextThenColon
      runInstrs <- parseCommands (string "Run:") eof
      skipMany parseComment
      let strip = T.unpack . T.strip . T.pack
          runs = map strip runInstrs
          builds = map strip buildInstrs
      when (length runs /= 1) $ error "Need exactly one run command"
      return (builds, runs)

eof' :: Parser String
eof'
  = eof >> return ""

parseComment :: Parser String
parseComment
  = do
      _ <- string "#"
      comment <- manyTill anyChar (try (string "\n" <|> eof'))
      return comment
