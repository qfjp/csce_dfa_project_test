{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Dfas where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Dfa (Dfa(..))

import ExternChecks (checkDfa)

import Parser.Dfa (parseDfa)

import qualified Data.Text as T

import Text.Parsec
import Text.RawString.QQ

import Test.Hspec

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

doParseDfa :: T.Text -> Either ParseError Dfa
doParseDfa
  = runParser parseDfa () "" . T.unpack

dfa1 :: T.Text
dfa1
  = [r|
Number of states: 2
Accepting states: 1
Alphabet: 01
0 1
1 0
|]

dfa1' :: Dfa
dfa1' = Dfa
         2
         (S.fromList ['0', '1'])
         (M.fromList [((0, '0'), 0), ((0, '1'), 1), ((1, '0'), 1), ((1, '1'), 0)])
         (S.fromList [1])

badNumberOfStates :: T.Text
badNumberOfStates
  = [r|
Number of states:  
Accepting states: 1
Alphabet: 01
0 1
1 0
|]

negNumberOfStates :: T.Text
negNumberOfStates
  = [r|
Number of states: -3
Accepting states: 1
Alphabet: 01
0 1
1 0
|]

badAcceptStates :: T.Text
badAcceptStates
  = [r|
Number of states: 2
Accepting states: a
Alphabet: 01
0 1
1 0
|]

oorAcceptStates :: T.Text
oorAcceptStates
  = [r|
Number of states: 2
Accepting states: 2
Alphabet: 01
0 1
1 0
|]

badTransTable :: T.Text
badTransTable
  = [r|
Number of states: 2
Accepting states: 1
Alphabet: 01
0  
1 0
|]

oorTransTable :: T.Text
oorTransTable
  = [r|
Number of states: 2
Accepting states: 1
Alphabet: 01
0 1
2 0
|]

--data Dfa
--    = Dfa { _Q :: Int
--          , _σ :: S.Set Char
--          , _δ :: M.Map (Int, Char) Int
--          , _F :: S.Set Int
--          }
--    deriving Show
dfaSpec :: SpecWith ()
dfaSpec
  = do
      describe "DFA Parser" $ do
          it "Even # of 1's" $
              doParseDfa dfa1 `shouldBe` Right dfa1'
          it "Noninteger states" $ do
              isLeft (doParseDfa badNumberOfStates) `shouldBe` True
          it "Negative states" $
              isLeft (doParseDfa negNumberOfStates) `shouldBe` True
          --it "Noninteger accept states" $ -- TODO: Fix isDFA
          --    checkDfa badAcceptStates "c_files" `shouldReturn` False
          it "Out of range accept states" $
              isLeft (doParseDfa oorAcceptStates) `shouldBe` True
          it "Noninteger transition table" $
              isLeft (doParseDfa badTransTable) `shouldBe` True
          it "Out of Range transition table" $
              isLeft (doParseDfa oorTransTable) `shouldBe` True
      describe "DFA Verification" $ do
          it "Even # of 1's" $
              checkDfa dfa1 "c_files" `shouldReturn` True
          it "Noninteger states" $
              checkDfa badNumberOfStates "c_files" `shouldReturn` False
          it "Negative states" $
              checkDfa negNumberOfStates "c_files" `shouldReturn` False
          --it "Noninteger accept states" $ -- TODO: Fix isDFA
          --    checkDfa badAcceptStates "c_files" `shouldReturn` False
          it "Out of range accept states" $
              checkDfa oorAcceptStates "c_files" `shouldReturn` False
          it "Noninteger transition table" $
              checkDfa badTransTable "c_files" `shouldReturn` False
          it "Out of Range transition table" $
              checkDfa oorTransTable "c_files" `shouldReturn` False
