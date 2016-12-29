{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Data.Dfa
Description : Dfa data type and associated functions.

This module stores the representation of a Dfa, and provides some
useful transformations.
-}
module Data.Dfa (Dfa(..), hPrintDfa, printDfa, showDfa) where

import           Control.Monad   (join)

import qualified Data.Map        as M
import           Data.Maybe      (fromMaybe)
import           Data.Monoid     ((<>))
import qualified Data.Set        as S
import           Data.Text       (Text, pack)
import           Data.Text.IO    (hPutStr, hPutStrLn)

import           System.IO       (Handle, stdout)

import           Test.QuickCheck

-- | A representation of the Dfa as a 4-tuple (the set of states should
-- always be between 0 and Q).
data Dfa
    = Dfa { _Q :: Int
          , _σ :: S.Set Char
          , _δ :: M.Map (Int, Char) Int
          , _F :: S.Set Int
          }
    deriving (Eq, Show)

instance Arbitrary Dfa where
    arbitrary
      = do
          numQs <- arbitrary `suchThat` (> 0)
          σ' <- sublistOf (map toEnum [32..126] :: [Char]) `suchThat` (not . null)
          fs' <- sublistOf [0..numQs - 1] `suchThat` (not . null) -- TODO: allow for empty list
          let σ  = S.fromList σ'
              fs = S.fromList fs'
          δ <- genArbitraryTrans numQs σ
          return $ Dfa numQs σ δ fs
      where
          genArbitraryTrans :: Int -> S.Set Char -> Gen (M.Map (Int, Char) Int)
          genArbitraryTrans numQs σ
            = do
                let σ' = S.toAscList σ
                    states = [0..numQs - 1] :: [Int]
                    keys :: [[(Int, Char)]]
                    keys = map (\x -> zip x σ') (map repeat states)
                    gensByRow = map (\x -> zip x (repeat $ elements states)) keys
                    gens :: [((Int, Char), Gen Int)]
                    gens = join gensByRow
                mapAsList <- sequenceA . map sequenceA $ gens
                return $ M.fromList mapAsList

-- | Print a Dfa according to the provided specification.
printDfa :: Dfa -> IO ()
printDfa
  = hPrintDfa stdout

-- | Print a Dfa to a file.
hPrintDfa :: Handle -> Dfa -> IO ()
hPrintDfa h
  = hPutStr h . showDfa

showT :: Show a => a -> Text
showT = pack . show

-- | Print a Dfa to a file.
showDfa :: Dfa -> Text
showDfa dfa
  = ("Number of states: " <> (showT . _Q $ dfa) <> "\n")
    <> ("Accepting States: " <> (showLst . S.toAscList . _F $ dfa) <> "\n")
    <> "Alphabet: " <> (pack . S.toAscList . _σ $ dfa) <> "\n"
    <>  showTransFunction dfa

-- | Convert the transition function to a string as per the
-- specification.
showTransFunction :: Dfa -> Text
showTransFunction dfa'
  = let states = [0.._Q dfa' - 1]
    in foldr (\state txt -> showLine state dfa' <> txt) "" states
  where
      showLine :: Int -> Dfa -> Text
      showLine stateNum dfa
        = do
            let trans    = _δ dfa
                σLst     = S.toAscList $ _σ dfa
                queries  = zip (repeat stateNum) σLst
                mResults = mapM (flip M.lookup $ trans) queries
                results  = fromMaybe [] mResults
            showLst results <> "\n"

-- | Take a list and convert it to a space separated string
-- representation.
showLst :: Show a => [a] -> Text
showLst xs
  | null xs
      = ""
  | length xs == 1
      = showT . head $ xs
  | otherwise
      = (showT . head $ xs)
        <> " "
        <>  (showLst . tail $ xs)
