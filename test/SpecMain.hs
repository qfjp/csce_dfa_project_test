module Main where

import           Test.Hspec

import           Data.Dfa.EquivalenceSpec
import           Parser.BuildSpec
import           Parser.DfaSpec
import           ProgramExecutionSpec

main :: IO ()
main
  = hspec $ do
      rtSpec
      peSpec
      parseSpec
      dfaSpec
      equivalenceSpec
      isomorphismSpec
