module ProgExecSpec where

import           ProgramExecution        (ProgramExecution (..))

import           Control.Applicative     ((<*>))
import           Control.Monad           (ap)

import           Test.Hspec
import           Test.Hspec.Checkers
import           Test.QuickCheck
import           Test.QuickCheck.Classes

peSpec :: SpecWith ()
peSpec
  = describe "Program Execution ?Monad?" $ do
      testBatch $ functor (undefined :: ProgramExecution String (Int, String, Char))
      testBatch $ applicative (undefined :: ProgramExecution String (Int, String, Double))
      testBatch $ monad (undefined :: ProgramExecution String (Int, String, Double))
      it "  <*> == ap" $
        property $ \x y -> (x <*> y)
          == ((x :: ProgramExecution String (Int -> Int))
               `ap` (y :: ProgramExecution String Int))
