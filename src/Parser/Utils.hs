{-|
Module      : Parser.Utils
Description : Useful parsing functions
-}


module Parser.Utils (parseInt, parseList, parseIntList
  , parseIntListList, parseTextThenColon, stripLines) where

import qualified Data.Text          as T

import           Data.Maybe         (fromMaybe)

import           Text.Parsec
import           Text.Parsec.String

-- | Parses content of the form \"Something:\"
parseTextThenColon :: Parser String
parseTextThenColon
  = do
      text <- many (upper <|> lower <|> space)
      _ <- try (string ": ") <|> string ":"
      return text

parseInt :: Parser Int
parseInt
  = read <$> many1 digit

parseList :: Parser a -> Maybe (Parser String) -> Parser [a]
parseList parseOne mSep
  = do
      first <- parseOne
      rest  <- option [] parseRemaining
      return (first : rest)
  where
      parseRemaining
        = let pSep = fromMaybe (string "") mSep
          in try $ pSep >> choice [parseList parseOne mSep, return []]

parseIntList :: Parser [Int]
parseIntList
  = parseList parseInt (Just $ string " ")

parseIntListList :: Parser [[Int]]
parseIntListList
  = parseList parseIntList (Just $ string "\n")

stripLines :: T.Text -> T.Text
stripLines = T.unlines . map T.strip . T.lines
