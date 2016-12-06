module Main where

import Test.Hspec

import RunType
import ProgramExecution

main :: IO ()
main
  = hspec $ do
      rtSpec
      peSpec
