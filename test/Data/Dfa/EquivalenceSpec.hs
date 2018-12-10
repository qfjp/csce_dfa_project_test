{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Data.Dfa.EquivalenceSpec where

import           Data.Dfa
import           Data.Dfa.Equivalence    (equivalent, equivalentText,
                                          isomorphic, isomorphicText)
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Text               as T

import           Parser.Dfa              (parseDfa)

import           Text.Parsec             (ParseError, runParser)
import           Text.RawString.QQ

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Control.Monad.Except    (MonadError, runExcept,
                                          throwError)

instance MonadError DfaError (PropertyM IO)

doParseDfa :: T.Text -> Either ParseError Dfa
doParseDfa
  = runParser parseDfa () "" . T.unpack

simple :: Dfa
simple
  = Dfa
      2
      (S.fromList ['0', '1'])
      (M.fromList [ ((0, '0'), 1), ((0, '1'), 0)
                  , ((1, '0'), 0), ((1, '1'), 1)])
      (S.fromList [])

simpleText :: T.Text
simpleText
  = [r|
Number of states: 1
Accepting States:
Alphabet: 0123456789abcdef
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
|]

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

dfaTextComparisonSpec :: SpecWith ()
dfaTextComparisonSpec
  = describe "Comparisons on textual representations of Dfas" $ do
      it "Any Dfa (as text) is self-isomorphic" $
          monadicIO $ pick arbitrary >>= \dfa ->
                              let dfa' = showDfa dfa
                              in isomorphicText dfa' dfa'
      it "No final states" $ do
          runExcept (isomorphicText simpleText simpleText) `shouldBe` (Right True)
      it "minDfa2 === nonminDfa2" $
          runExcept (equivalentText minDfa2 nonminimalDfa2) `shouldBe` (Right True)
      it "minDfa3 === nonminDfa3" $
          runExcept (equivalentText minDfa3 nonminimalDfa3) `shouldBe` (Right True)
      it "minDfa1 != minDfa2" $
          runExcept (equivalentText minDfa1 minDfa2) `shouldBe` (Right False)
      it "minDfa1 != minDfa3" $
          runExcept (equivalentText minDfa1 minDfa3) `shouldBe` (Right False)

parseTest :: MonadError DfaError m
          => (Dfa -> Dfa -> m Bool)
          -> T.Text -> T.Text -> m Bool
parseTest f d1' d2'
  = case doParseDfa d1' of
      (Left e) -> throwError $ DfaParseError e
      (Right d1) ->
          case doParseDfa d2' of
            (Left e)   -> throwError $ DfaParseError e
            (Right d2) -> f d1 d2

isomorphismSpec :: SpecWith ()
isomorphismSpec
  = describe "Isomorphism using Hopcroft-Karp" $ do
      it "Any Dfa is self-isomorphic" $
          monadicIO $ pick arbitrary >>= \dfa -> isomorphic dfa dfa
      it "Odd zeros (2 states) ~= Odd zeros (4 states)" $
          runExcept (isomorphic oddZeros1 oddZeros2) `shouldBe` (Right False)
      it "No final states" $
          runExcept (isomorphic simple simple) `shouldBe` (Right True)
      it "minDfa1 ~= nonminDfa1" $
          runExcept (parseTest isomorphic minDfa1 nonminimalDfa1) `shouldBe` (Right False)
      it "minDfa2 ~= nonminDfa2" $
          runExcept (parseTest isomorphic minDfa2 nonminimalDfa2) `shouldBe` (Right False)
      it "minDfa3 ~= nonminDfa3" $
          runExcept (parseTest isomorphic minDfa3 nonminimalDfa3) `shouldBe` (Right False)

equivalenceSpec :: SpecWith ()
equivalenceSpec
  = describe "Equivalence using Hopcroft-Karp" $ do
      it "Odd zeros (2 states) === Odd zeros (4 states)" $
          runExcept (equivalent oddZeros1 oddZeros2) `shouldBe` (Right True)
      it "minDfa1 === nonminDfa1" $
          runExcept (parseTest equivalent minDfa1 nonminimalDfa1) `shouldBe` (Right True)
      it "minDfa2 === nonminDfa2" $
          runExcept (parseTest equivalent minDfa2 nonminimalDfa2) `shouldBe` (Right True)
      it "minDfa3 === nonminDfa3" $
          runExcept (parseTest equivalent minDfa3 nonminimalDfa3) `shouldBe` (Right True)
      it "minDfa1 != minDfa2" $
          runExcept (parseTest equivalent minDfa1 minDfa2) `shouldBe` (Right False)
      it "minDfa1 != minDfa3" $
          runExcept (parseTest equivalent minDfa1 minDfa3) `shouldBe` (Right False)
      it "minDfa1 != nonminDfa2" $
          runExcept (parseTest equivalent minDfa1 nonminimalDfa2) `shouldBe` (Right False)
      it "minDfa2 != nonminDfa1" $
          runExcept (parseTest equivalent minDfa2 nonminimalDfa1) `shouldBe` (Right False)
      it "minDfa3 != nonminDfa2" $
          runExcept (parseTest equivalent nonminimalDfa3 nonminimalDfa2) `shouldBe` (Right False)
