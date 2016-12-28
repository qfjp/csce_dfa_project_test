module Main where

import Test.Hspec

import RunType
import Parser.BuildSpec
import Parser.DfaSpec
import ProgExecSpec

main :: IO ()
main
  = hspec $ do
      rtSpec
      peSpec
      parseSpec
      dfaSpec
