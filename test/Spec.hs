import Control.Monad (ap)
import Data.Monoid

import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import ProjectTest

instance Arbitrary RunType where
    arbitrary
      = frequency $ map (\x -> (1, return x))
                        [ RunTypeUndefined, Simulate, Minimize
                        , Searcher, BoolopComp, BoolopProd, Invhom
                        , Properties ]

instance EqProp RunType where
    (=-=) = eq

rtSpec :: SpecWith ()
rtSpec
  = describe "RunType ?Monoid?" $
      testBatch $ monoid (undefined :: RunType)

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

main :: IO ()
main
  = hspec peSpec
