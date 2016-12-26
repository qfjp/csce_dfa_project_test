{-|
Module      : Parser
Description : File parsing functions

This module controls various parsing related functions. Build file
parsing and the output of user functions is done here.
-}
module Parser (parseBuildFile) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Either (rights)

import qualified Data.Text as T

import Text.Parsec
import Text.Parsec.String

-- | Content to be ignored while parsing
parseIgnore :: Parser String
parseIgnore
  = parseComment <|> blankLine

-- | Parses content of the form \"Something:\"
parseTextThenColon :: Parser String
parseTextThenColon
  = do
      text <- many (upper <|> lower)
      _ <- oneOf ":" >> parseEndOfLine
      return text

-- | Parses one set of commands within a build file (e.g. Build and
-- Run commands)
parseCommands :: Parser String -- ^ The (ignorable) start of the command list
              -> Parser a -- ^ The Parser that determines the end of the list
              -> Parser [String]
parseCommands intro end
  = do
      _ <- intro
      commands' <- manyTill parseCommentOrLine (try . lookAhead $ end)
      let commands = filter (\x -> length x > 0) $ rights commands'
          strip = T.unpack . T.strip . T.pack
          stripCommands = map strip commands
      return stripCommands

-- | Parses build files as documented in the project handout. Ignores
-- line and end-of-line comments beginning with a \'#\'
parseBuildFile :: Parser ([String], String)
parseBuildFile
  = do
      skipMany parseIgnore
      buildInstrs <- parseCommands (string "Build:") parseTextThenColon
      runInstrs <- parseCommands (string "Run:") eof
      skipMany parseIgnore
      let runs = head runInstrs
      return (buildInstrs, runs)

-- | Parses a single line, where a Left denotes a comment and a Right
-- denotes a valid line.
parseCommentOrLine :: Parser (Either String String)
parseCommentOrLine
  = do
      comment <- optionMaybe (try parseComment)
      case comment of
        Nothing -> Right <$> manyTill anyChar parseEndOfLine
        Just x -> Left <$> return x

-- | eof Parser that returns a string to be used with other
-- combinators
eof' :: Parser String
eof'
  = eof >> return ""

-- | Parses a single comment beginning with a \'#\'
parseComment :: Parser String
parseComment
  = do
      _ <- parseSpace >> string "#"
      manyTill anyChar (try (string "\n" <|> eof'))

-- | A parser which denotes a valid end-of-line, which could be a
-- comment or a newline character
parseEndOfLine :: Parser String
parseEndOfLine
  = try parseComment <|> string "\n"

-- | Denotes all valid non-line-ending whitespace
parseSpace :: Parser String
parseSpace
  = many . oneOf $ " \t"

-- | Denotes all lines which only contain whitespace
blankLine :: Parser String
blankLine
  = do
      whitespace <- parseSpace
      _ <- parseEndOfLine
      return whitespace
