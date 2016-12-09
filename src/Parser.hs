module Parser (parseBuildFile) where

import Control.Monad (when)
import Data.Either (rights)

import qualified Data.Text as T

import Text.Parsec
import Text.Parsec.String

parseIgnore :: Parser String
parseIgnore
  = parseComment <|> blankLine

parseTextThenColon :: Parser String
parseTextThenColon
  = do
      text <- many (upper <|> lower)
      _ <- oneOf ":" >> parseEndOfLine
      return text

parseCommands :: Parser String -> Parser a -> Parser [String]
parseCommands intro end
  = do
      _ <- intro
      commands' <- manyTill parseCommentOrLine (try . lookAhead $ end)
      let commands = filter (\x -> length x > 0) $ rights commands'
          strip = T.unpack . T.strip . T.pack
          stripCommands = map strip commands
      return stripCommands

parseBuildFile :: Parser ([String], [String])
parseBuildFile
  = do
      skipMany parseIgnore
      buildInstrs <- parseCommands (string "Build:") parseTextThenColon
      runInstrs <- parseCommands (string "Run:") eof
      skipMany parseIgnore
      let strip = T.unpack . T.strip . T.pack
          runs = map strip runInstrs
          builds = map strip buildInstrs
      when (length runs /= 1) $ fail "Need exactly one run command"
      return (builds, runs)

parseCommentOrLine :: Parser (Either String String)
parseCommentOrLine
  = do
      comment <- optionMaybe (try parseComment)
      case comment of
        Nothing -> Right <$> manyTill anyChar parseEndOfLine
        Just x -> Left <$> return x

eof' :: Parser String
eof'
  = eof >> return ""

parseComment :: Parser String
parseComment
  = do
      _ <- parseSpace >> string "#"
      manyTill anyChar (try (string "\n" <|> eof'))

parseEndOfLine :: Parser String
parseEndOfLine
  = try parseComment <|> string "\n"

parseSpace :: Parser String
parseSpace
  = many . oneOf $ " \t"

blankLine :: Parser String
blankLine
  = do
      whitespace <- parseSpace
      _ <- parseEndOfLine
      return whitespace
