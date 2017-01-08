{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Data.Dfa.EquivalenceSpec where

import           Data.Dfa
import           Data.Dfa.Equivalence
import qualified Data.Map             as M
import qualified Data.Set             as S
import qualified Data.Text            as T

import           Parser.Dfa           (parseDfa)

import           Text.Parsec          (ParseError, runParser)
import           Text.RawString.QQ

import           Test.Hspec

doParseDfa :: T.Text -> Either ParseError Dfa
doParseDfa
  = runParser parseDfa () "" . T.unpack

oddZeros1 :: Dfa
oddZeros1
  = Dfa
      2
      (S.fromList ['0', '1'])
      (M.fromList [ ((0, '0'), 1), ((0, '1'), 0)
                  , ((1, '0'), 0), ((1, '1'), 1)])
      (S.fromList [1])

oddZeros2 :: Dfa
oddZeros2
  = Dfa
      4
      (S.fromList ['0', '1'])
      (M.fromList [ ((0, '0'), 1), ((0, '1'), 2)
                  , ((1, '0'), 0), ((1, '1'), 3)
                  , ((2, '0'), 3), ((2, '1'), 0)
                  , ((3, '0'), 2), ((3, '1'), 1)])
      (S.fromList [1, 3])

minDfa1 :: T.Text
minDfa1
  = [r|
Number of states: 5
Accepting states: 2
Alphabet: 01
1 3
4 2
0 2
2 4
4 0
|]

minDfa2 :: T.Text
minDfa2
  = [r|
Number of states: 2
Accepting states: 0
Alphabet: 01
0 1
0 1
|]

minDfa3 :: T.Text
minDfa3
  = [r|
Number of states: 4
Accepting states: 3
Alphabet: 01
1 0
0 2
3 1
3 0
|]

nonminimalDfa1 :: T.Text
nonminimalDfa1
  = [r|
Number of states: 8
Accepting states: 2
Alphabet: 01
1 5
6 2
0 2
2 6
7 5
2 6
6 4
6 2
|]

nonminimalDfa2 :: T.Text
nonminimalDfa2
  = [r|
Number of states: 3
Accepting states: 0 1
Alphabet: 01
1 2
1 2
0 2
|]

nonminimalDfa3 :: T.Text
nonminimalDfa3
  = [r|
Number of states: 8
Accepting states: 3
Alphabet: 01
1 0
0 2
3 1
3 0
3 5
6 4
5 6
6 3
|]


equivalenceSpec :: SpecWith ()
equivalenceSpec
  = describe "Equivalence using Hopcroft-Karp" $ do
      it "Odd zeros (2 states) === Odd zeros (4 states)" $
          hopcroftKarp oddZeros1 oddZeros2 `shouldBe` True
      it "minDfa1 === nonminDfa1" $
          let (Right minDfa) = doParseDfa minDfa1
              (Right nonMin) = doParseDfa nonminimalDfa1
          in hopcroftKarp minDfa nonMin `shouldBe` True
      it "minDfa2 === nonminDfa2" $
          let (Right minDfa) = doParseDfa minDfa2
              (Right nonMin) = doParseDfa nonminimalDfa2
          in hopcroftKarp minDfa nonMin `shouldBe` True
      it "minDfa3 === nonminDfa3" $
          let (Right minDfa) = doParseDfa minDfa3
              (Right nonMin) = doParseDfa nonminimalDfa3
          in hopcroftKarp minDfa nonMin `shouldBe` True
      it "minDfa1 != minDfa2" $
          let (Right dfa1) = doParseDfa minDfa1
              (Right dfa2) = doParseDfa minDfa2
          in hopcroftKarp dfa1 dfa2 `shouldBe` False
      it "minDfa1 != minDfa3" $
          let (Right dfa1) = doParseDfa minDfa1
              (Right dfa2) = doParseDfa minDfa3
          in hopcroftKarp dfa1 dfa2 `shouldBe` False
      it "minDfa1 != nonminDfa2" $
          let (Right dfa1) = doParseDfa minDfa1
              (Right dfa2) = doParseDfa nonminimalDfa2
          in hopcroftKarp dfa1 dfa2 `shouldBe` False
      it "minDfa2 != nonminDfa1" $
          let (Right dfa1) = doParseDfa minDfa2
              (Right dfa2) = doParseDfa nonminimalDfa1
          in hopcroftKarp dfa1 dfa2 `shouldBe` False
      it "minDfa3 != nonminDfa2" $
          let (Right dfa1) = doParseDfa minDfa3
              (Right dfa2) = doParseDfa nonminimalDfa2
          in hopcroftKarp dfa1 dfa2 `shouldBe` False
