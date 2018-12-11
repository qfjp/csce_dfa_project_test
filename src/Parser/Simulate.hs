module Parser.Simulate (doParseSim) where

import qualified Data.Text          as T

import           Parser.Utils

import           Text.Parsec
import           Text.Parsec.String

doParseSim :: T.Text -> Either ParseError [Bool]
doParseSim
  = runParser parseSim () "" . T.unpack . stripLines

parseOneSim :: Parser Bool
parseOneSim
  = parseAccept <|> parseReject

parseAccept :: Parser Bool
parseAccept
  = string "accept"
  >> return True

parseReject :: Parser Bool
parseReject
  = string "reject"
  >> return False

parseSim :: Parser [Bool]
parseSim
  = parseList parseOneSim (Just $ string "\n")
