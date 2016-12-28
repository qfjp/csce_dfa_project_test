{-|
Module      : Data.Dfa
Description : Dfa data type and associated functions.

This module stores the representation of a Dfa, and provides some
useful transformations.
-}
module Data.Dfa (Dfa(..), hPrintDfa, printDfa, hPrintLst) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Maybe (fromMaybe)

import System.IO (Handle, stdout, hPrint, hPutStr, hPutStrLn)

-- | A representation of the Dfa as a 4-tuple (the set of states should
-- always be between 0 and Q).
data Dfa
    = Dfa { _Q :: Int
          , _σ :: S.Set Char
          , _δ :: M.Map (Int, Char) Int
          , _F :: S.Set Int
          }
    deriving (Eq, Show)

-- | Print a Dfa according to the provided specification.
printDfa :: Dfa -> IO ()
printDfa
  = hPrintDfa stdout

-- | Print a Dfa to a file.
hPrintDfa :: Handle -> Dfa -> IO ()
hPrintDfa h dfa
  = do
      hPutStr h "Number of states: "
      hPrint h . _Q $ dfa
      hPutStr h "Accepting States: "
      hPrintLst h . S.toAscList . _F $ dfa
      hPutStr h "Alphabet: "
      hPutStrLn h . S.toAscList . _σ $ dfa
      hPrintTransFunction h dfa

-- | Given a file handle, print the transition function as per the
-- specification.
hPrintTransFunction :: Handle -> Dfa -> IO ()
hPrintTransFunction h' dfa'
  = let states = [0.._Q dfa' - 1]
    in mapM_ (\x -> hPrintLine h' x dfa') states
  where
      hPrintLine :: Handle -> Int -> Dfa -> IO ()
      hPrintLine h stateNum dfa
        = do
            let trans    = _δ dfa
                σLst     = S.toAscList $ _σ dfa
                queries  = zip (repeat stateNum) σLst
                mResults = mapM (flip M.lookup $ trans) queries
                results  = fromMaybe [] mResults
            hPrintLst h results

-- | Print the space separated contents of the list, ending in a
-- newline.
hPrintLst :: Show a => Handle -> [a] -> IO ()
hPrintLst h xs
  | null xs
      = hPutStrLn h ""
  | length xs == 1
      = hPrint h . head $ xs
  | otherwise
      = do
          hPutStr h . show . head $ xs
          hPutStr h " "
          hPrintLst h . tail $ xs
