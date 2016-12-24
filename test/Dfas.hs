{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Dfas where

import ExternChecks (checkDfa)

import qualified Data.Text as T

import Text.RawString.QQ

import Test.Hspec

dfa1 :: T.Text
dfa1
  = [r|
Number of states: 2
Accepting states: 1
Alphabet: 01
0 1
1 0
|]

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

dfaSpec :: SpecWith ()
dfaSpec
  = describe "DFA Verification" $ do
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
