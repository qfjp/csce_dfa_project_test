module ProgramExecution where

import ProjectTest (ProgramExecution(..), RunType(Simulate))

import Control.Monad (ap)

import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance (Arbitrary a, Arbitrary b) => Arbitrary (ProgramExecution a b) where
    arbitrary = PE Simulate <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (ProgramExecution a b) where
    (=-=) = eq

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
