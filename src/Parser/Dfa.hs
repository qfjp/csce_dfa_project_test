{-|
Module      : Parser.Dfa
Description : Module to parse a Dfa

Error-reporting Dfa parse functions.
-}
module Parser.Dfa (parseDfa) where

import Control.Applicative ((<$>))

import Debug.Trace

import qualified Data.Map as M
import qualified Data.Set as S

import Data.Dfa (Dfa(..))
import Data.Maybe

import Control.Monad

import Text.Parsec
import Text.Parsec.String

-- | A Dfa parser.
parseDfa :: Parser Dfa
parseDfa
  = do
      numQs <- parseStateNum
      fs    <- fmap S.fromList $ parseAcceptingStates numQs
      _     <- newline
      σ     <- fmap S.fromList parseAlphabet
      _     <- newline
      δ     <- parseTransFunction numQs (S.toAscList σ)
      return Dfa {_Q = numQs, _σ = σ, _δ = δ, _F = fs}

parseTextThenColon :: Parser String
parseTextThenColon
  = do
      text <- many (upper <|> lower <|> space)
      _ <- oneOf ":" >> oneOf " "
      return text

parseInt :: Parser Int
parseInt
  = read <$> many1 digit

parseList :: Parser a -> Maybe (Parser String) -> Parser [a]
parseList parseOne mSep
  = do
      first <- parseOne
      rest  <- parseRemaining
      return (first : rest)
  where
      parseRemaining
        = let pSep = fromMaybe (string "") mSep
          in try (try pSep >> try (parseList parseOne mSep)) <|> return []

parseIntList :: Parser [Int]
parseIntList
  = parseList parseInt (Just $ string " ")

parseAlphChar :: Parser Char
parseAlphChar
  = oneOf $ map (toEnum :: Int -> Char) [32..126]

parseLines :: Parser [String]
parseLines
  = parseList (many parseAlphChar) (Just $ string "\n")

parseIntListList :: Parser [[Int]]
parseIntListList
  = parseList parseIntList (Just $ string "\n")

parseAlphabetChars :: Parser [Char]
parseAlphabetChars
  = parseList parseAlphChar Nothing

parseStateNum :: Parser Int
parseStateNum
  = do
      _ <- parseTextThenColon
      parseInt

parseAcceptingStates :: Int -> Parser [Int]
parseAcceptingStates numQs
  = do
      _ <- parseTextThenColon
      fs <- parseIntList
      when (any (>= numQs) fs) $
          fail "State out of range"
      return fs

parseAlphabet :: Parser [Char]
parseAlphabet
  = do
      _ <- parseTextThenColon
      parseAlphabetChars

parseTransFunction :: Int -> [Char] -> Parser (M.Map (Int, Char) Int)
parseTransFunction numQs abet
  = do
      let states = [0..numQs - 1] :: [Int]
          statesCrossAlphabet
            = [ (state, symb)
              | state <- states, symb <- abet] :: [(Int, Char)]
      rawTrans <- parseIntListList
      when ((length rawTrans) < numQs) $
          fail "Error in transition table"
      let rawTransLengths = map length rawTrans
      when (null rawTrans || (any (/= head rawTransLengths) (tail rawTransLengths))) $
          fail "Error in transition table"
      let rawTransOneD = join rawTrans
      when (any (>= numQs) rawTransOneD) $
          fail "State out of range in transition table"
      return . M.fromList $ zip statesCrossAlphabet rawTransOneD
