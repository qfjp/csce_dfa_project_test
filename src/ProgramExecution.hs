module ProgramExecution (
    -- * Program Execution Container
      ProgramExecution(..)
    , showProgExec
    -- * Project task tags
    , RunType(..)
    , rtToFile
    -- * Execution results
    , Result(..)
    , resultToText
    ) where

import Data.Monoid (Sum(..), (<>))

-- | Type for different portions of the project
data RunType
    -- | Continued is only available as the identity, representing
    -- an execution that already is typed.
    = Continued
    | Simulate
    | Minimize
    | Searcher
    | BoolopComp -- ^ Part 1 of the Boolop task
    | BoolopProd -- ^ Part 2 of the Boolop task
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

-- | This converts RunType values into the names given on the project
-- description
rtToFile :: RunType -> String
rtToFile Simulate   = "simulator"
rtToFile Minimize   = "minimizer"
rtToFile Searcher   = "searcher"
rtToFile BoolopComp = "boolop"
rtToFile BoolopProd = "boolop"
rtToFile Invhom     = "invhom"
rtToFile Properties = "properties"
rtToFile Continued  = "UNDEFINED"

-- | A ProgramExecution is a test run over one task of the project.
data ProgramExecution a b
    = PE { -- | The task which is being tested
            _tag :: RunType
           -- | A monoid that keeps track of errors. Most likely
           -- Data.Monoid.Sum Int
         , _errorCount :: a
           -- | The result of the execution. Most likely 'Result'
         , _result :: b }
    deriving (Eq, Show)

instance Functor (ProgramExecution a) where
    fmap f (PE t e r) = PE t e (f r)

instance Monoid a => Applicative (ProgramExecution a) where
    pure = PE Simulate mempty
    PE t1 e1 f <*> PE t2 e2 x
      = PE (t1 <> t2) (e1 <> e2) (f x)

instance Monoid a => Monad (ProgramExecution a) where
    return = pure
    PE t e r >>= f
      = let PE t' e' r' = f r
        in PE (t <> t') (e <> e') r'

-- | The function to show the results of a ProgramExecution. Use at
-- the end of the comments.txt file
showProgExec :: (Enum a, Show a) => ProgramExecution (Sum Int) a -> String
showProgExec PE {_tag = t, _errorCount = (Sum e), _result = r}
  = show t ++ ": " ++ resultToText t result ++
      "\nprogress level " ++ show r ++ " with " ++ show e ++
      " execution errors"
  where result = toEnum . fromEnum $ r

data Result
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
  -- | Useful to avoid partial functions with the Enum instance
  | Unknown
  deriving (Enum, Eq, Show)

-- | Convert a 'RunType' and a 'Result' into a user friendly
-- description of what went wrong.
resultToText :: RunType -> Result -> String
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
resultToText rt _
  = "??? unknown progress status for " ++ rtToFile rt
