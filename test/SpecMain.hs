module Main where

import Test.Hspec

import RunType
import ProgExecSpec
import ParseBuilds

main :: IO ()
main
  = hspec $ do
      rtSpec
      peSpec
      parseSpec
