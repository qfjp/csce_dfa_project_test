module RunType where

import           ProgramExecution        (RunType (..))

import           Test.Hspec
import           Test.Hspec.Checkers
import           Test.QuickCheck.Classes

rtSpec :: SpecWith ()
rtSpec
  = describe "RunType ?Monoid?" $
      testBatch $ monoid (undefined :: RunType)
