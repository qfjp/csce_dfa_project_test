module Main where

import Test.Hspec

import Dfas
import RunType
import ParseBuilds
import ProgExecSpec

main :: IO ()
main
  = hspec $ do
      rtSpec
      peSpec
      parseSpec
      dfaSpec
