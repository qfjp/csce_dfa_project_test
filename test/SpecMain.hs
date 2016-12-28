module Main where

import Test.Hspec

import Dfas
import RunType
import Parser.BuildSpec
import ProgExecSpec

main :: IO ()
main
  = hspec $ do
      rtSpec
      peSpec
      parseSpec
      dfaSpec
