module Main where

import Test.Hspec

import RunType
import ProgramExecution
import ParseBuilds

main :: IO ()
main
  = hspec $ do
      rtSpec
      peSpec
      parseSpec