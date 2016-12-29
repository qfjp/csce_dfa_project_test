{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Parser.DfaSpec where

import           Data.Dfa
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Text               as T

import           ExternChecks            (checkDfa)

import           Parser.Dfa              (parseDfa)

import           System.Directory        (removeFile)
import           System.IO               (hClose, openTempFile)

import           Text.Parsec
import           Text.Parsec.String      (parseFromFile)
import           Text.RawString.QQ

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

testDfa :: Dfa
testDfa
  = Dfa
      { _Q = 1
      , _σ = S.fromList " $&')+./0345678:;<=>ABCDFGIJMOPQSTXZ\\]_abcdknopqsuz{"
      , _δ = M.fromList [((0,' '),0),((0,'$'),0),((0,'&'),0),((0,'\''),0),((0,')'),0),((0,'+'),0),((0,'.'),0),((0,'/'),0),((0,'0'),0),((0,'3'),0),((0,'4'),0),((0,'5'),0),((0,'6'),0),((0,'7'),0),((0,'8'),0),((0,':'),0),((0,';'),0),((0,'<'),0),((0,'='),0),((0,'>'),0),((0,'A'),0),((0,'B'),0),((0,'C'),0),((0,'D'),0),((0,'F'),0),((0,'G'),0),((0,'I'),0),((0,'J'),0),((0,'O'),0),((0,'P'),0),((0,'Q'),0),((0,'S'),0),((0,'T'),0),((0,'X'),0),((0,'Z'),0),((0,'\\'),0),((0,']'),0),((0,'_'),0),((0,'a'),0),((0,'b'),0),((0,'c'),0),((0,'d'),0),((0,'k'),0),((0,'n'),0),((0,'o'),0),((0,'p'),0),((0,'q'),0),((0,'s'),0),((0,'u'),0),((0,'z'),0),((0,'{'),0)]
      , _F = S.fromList [0]
      }

propWriteThenRead :: Dfa -> Property
propWriteThenRead dfa
  = monadicIO $ do
      maybeDfa <- run $ writeThenRead dfa
      case maybeDfa of
        Left x          -> fail (show x)
        Right parsedDfa -> assert $ dfa == parsedDfa

writeThenRead :: Dfa -> IO (Either ParseError Dfa)
writeThenRead dfa
  = do (path, h) <- openTempFile "." "quickcheckDfa.tmp"
       hPrintDfa h dfa
       hClose h
       result <- parseFromFile parseDfa path
       removeFile path
       return result

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

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

dfaSpec :: SpecWith ()
dfaSpec
  = do
      describe "Writing and Reading" $
          it "Dfa === parsing . printing" $
              property $ propWriteThenRead
      describe "DFA Parser" $ do
          it "Even # of 1's" $
              doParseDfa dfa1 `shouldBe` Right dfa1'
          it "Noninteger states" $
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
              checkDfa dfa1 `shouldBe` True
          it "Noninteger states" $
              checkDfa badNumberOfStates `shouldBe` False
          it "Negative states" $
              checkDfa negNumberOfStates `shouldBe` False
          --it "Noninteger accept states" $ -- TODO: Fix isDFA
          --    checkDfa badAcceptStates "c_files" `shouldReturn` False
          it "Out of range accept states" $
              checkDfa oorAcceptStates `shouldBe` False
          it "Noninteger transition table" $
              checkDfa badTransTable `shouldBe` False
          it "Out of Range transition table" $
              checkDfa oorTransTable `shouldBe` False
