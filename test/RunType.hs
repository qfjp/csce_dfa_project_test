module RunType where

import ProgramExecution (RunType(..))

import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

instance Arbitrary RunType where
    arbitrary
      = frequency $ map (\x -> (1, return x))
                        [ Continued, Simulate, Minimize
                        , Searcher, BoolopComp, BoolopProd, Invhom
                        , Properties ]

instance EqProp RunType where
    (=-=) = eq

rtSpec :: SpecWith ()
rtSpec
  = describe "RunType ?Monoid?" $
      testBatch $ monoid (undefined :: RunType)
