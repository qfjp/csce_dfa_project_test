{-|
Module      : Parser.Dfa
Description : Module to parse a Dfa

Error-reporting Dfa parse functions.
-}
module Parser.Dfa (doParseDfa, parseDfa, parseAcceptingStates) where

import           Control.Applicative ((<$>))

import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text           as T

import           Data.Maybe

import           Control.Monad

import           Text.Parsec
import           Text.Parsec.String

import           Data.Dfa            (Dfa (..))
import           Parser.Utils

doParseDfa :: T.Text -> Either ParseError Dfa
doParseDfa
  = runParser parseDfa () "" . T.unpack . stripLines

-- | A Dfa parser.
parseDfa :: Parser Dfa
parseDfa
  = do
      numQs <- parseStateNum
      fs    <- S.fromList <$> (parseAcceptingStates numQs)
      _     <- newline
      σ     <- S.fromList <$> parseAlphabet
      _     <- newline
      δ     <- parseTransFunction numQs (S.toAscList σ)
      return Dfa {_Q = numQs, _Σ = σ, _δ = δ, _F = fs}

parseAlphChar :: Parser Char
parseAlphChar
  = oneOf $ map (toEnum :: Int -> Char) [32..126]


parseAlphabetChars :: Parser String
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
      fs <- try parseIntList <|> return []
      when (any (>= numQs) fs) $
          fail "State out of range"
      return fs

parseAlphabet :: Parser String
parseAlphabet
  = do
      _ <- parseTextThenColon
      parseAlphabetChars

parseTransFunction :: Int -> String -> Parser (M.Map (Int, Char) Int)
parseTransFunction numQs abet
  = do
      let states = [0..numQs - 1] :: [Int]
          statesCrossAlphabet
            = [ (state, symb)
              | state <- states, symb <- abet] :: [(Int, Char)]
      rawTrans <- parseIntListList
      when (length rawTrans < numQs) $
          fail "Error in transition table"
      let rawTransLengths = map length rawTrans
      when (null rawTrans || any (/= head rawTransLengths) (tail rawTransLengths)) $
          fail "Error in transition table"
      let rawTransOneD = join rawTrans
      when (any (>= numQs) rawTransOneD) $
          fail "State out of range in transition table"
      return . M.fromList $ zip statesCrossAlphabet rawTransOneD
