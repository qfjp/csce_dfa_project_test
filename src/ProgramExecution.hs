{-|
Module      : ProgramExecution
Description : Types for the test-run data

This module stores the representation of a Dfa, and provides some
useful transformations.
-}
module ProgramExecution (
    -- * Program Execution Container
      ProgramExecution(..)
    , showProgExec
    -- * Project task tags
    , RunType(..)
    , rtToFile
    -- * Execution results
    , RunLevel(..)
    , resultToText
    ) where

import           Control.Applicative      (Applicative, pure, (<$>),
                                           (<*>))
import           Data.Monoid              (Monoid (..), Sum (..), (<>))
import           Test.QuickCheck
import           Test.QuickCheck.Checkers

-- | Type for different portions of the project
data RunType
    -- | Continued is only available as the identity, representing
    -- an execution that already is typed.
    = Continued
    | Simulate
    | Minimize
    | Searcher
    | Boolop
    | Invhom
    | Properties
  deriving (Eq, Show)

instance Monoid RunType where
    mempty = Continued
    -- Continued is technically only a left identity, but for
    -- convenience we use it on the right as well
    Continued `mappend` x = x
    x `mappend` Continued = x
    x `mappend` _ = x

instance Arbitrary RunType where
    arbitrary
      = frequency $ map (\x -> (1, return x))
                        [ Continued, Simulate, Minimize
                        , Searcher, Boolop, Invhom
                        , Properties ]

instance EqProp RunType where
    (=-=) = eq

-- | This converts RunType values into the names given on the project
-- description
rtToFile :: RunType -> String
rtToFile Simulate   = "simulator"
rtToFile Minimize   = "minimizer"
rtToFile Searcher   = "searcher"
rtToFile Boolop     = "boolop"
rtToFile Invhom     = "invhom"
rtToFile Properties = "properties"
rtToFile Continued  = "UNDEFINED"

-- | A ProgramExecution is a test run over one task of the project.
data ProgramExecution
    = PE { -- | The task which is being tested
            _tag       :: RunType
           -- | A monoid that keeps track of errors. Most likely
           -- Data.Monoid.Sum Int
         , _errorCount :: Sum Int
           -- | The run level (grade status) of the execution.
         , _runLevel   :: RunLevel }
    deriving (Eq, Show)

instance Monoid ProgramExecution where
    mempty = PE mempty mempty mempty
    (PE a i x) `mappend` (PE b j y)
      = PE (a <> b) (i <> j) (x <> y)

-- | The function to show the results of a ProgramExecution. Use at
-- the end of the comments.txt file
showProgExec :: ProgramExecution -> String
showProgExec PE {_tag = t, _errorCount = (Sum e), _runLevel = r}
  = show t ++ ": " ++ resultToText t result ++
      "\nprogress level " ++ show r ++ " with " ++ show e ++
      " execution errors"
  where result = toEnum . fromEnum $ r

-- | The status of the test run.
data RunLevel
  -- | not implemented at all ($prog.txt file does not exist)
  = NotImplemented
  -- | $prog.txt file exists, but there was an error parsing it
  | ParseError
  -- | $prog.txt parsed OK, but the build failed (error return value)
  | BuildFail
  -- | $prog built OK, but execution timed out at least once
  | TimeOut
  -- | $prog execution always completed (but there were errors)
  | FinishWithError
  -- | $prog execution always completed without errors
  | FinishPerfect
  | SoFar
  deriving (Enum, Eq, Ord)

instance Show RunLevel where
    show = show . fromEnum

instance Monoid RunLevel where
    mempty = SoFar
    x `mappend` y = if y > x then x else y

-- | Convert a 'RunType' and a 'RunLevel' into a user friendly
-- description of what went wrong.
resultToText :: RunType -> RunLevel -> String
resultToText rt NotImplemented
  = "not implemented -- " ++ rtToFile rt ++ ".txt does not exist"
resultToText rt ParseError
  = rtToFile rt ++ ".txt exists, but there was an error parsing it."
resultToText rt BuildFail
  = rtToFile rt ++ ".txt parsed OK, but build failed."
resultToText _ TimeOut
  = "built OK, but execution timed out at least once."
resultToText _ FinishWithError
  = "execution always completed, but there were errors."
resultToText _ FinishPerfect
  = "execution always completed without errors."
