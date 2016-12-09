module ProgramExecution where

import Data.Monoid (Sum(..), (<>))

data RunType
    = Continued
    | Simulate
    | Minimize
    | Searcher
    | BoolopComp
    | BoolopProd
    | Invhom
    | Properties
  deriving (Eq, Show)

instance Monoid RunType where
    mempty = Continued
    Continued `mappend` x = x
    x `mappend` Continued = x
    x `mappend` _ = x

rtToFile :: RunType -> String
rtToFile Simulate   = "simulator"
rtToFile Minimize   = "minimizer"
rtToFile Searcher   = "searcher"
rtToFile BoolopComp = "boolop"
rtToFile BoolopProd = "boolop"
rtToFile Invhom     = "invhom"
rtToFile Properties = "properties"
rtToFile Continued  = "UNDEFINED"

data ProgramExecution a b
    = PE { _tag :: RunType
         , _errorCount :: a
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

showProgExec :: (Enum a, Show a) => ProgramExecution (Sum Int) a -> String
showProgExec PE {_tag = t, _errorCount = (Sum e), _result = r}
  = show t ++ ": " ++ resultToText t result ++
      "\nprogress level " ++ show r ++ " with " ++ show e ++
      " execution errors"
  where result = toEnum . fromEnum $ r

data Result
  = NotImplemented
  | ParseError
  | BuildFail
  | TimeOut
  | FinishWithError
  | FinishPerfect
  | Unknown
  deriving (Enum, Eq, Show)

-- Holds which programs were implemented and what progress was made on
-- each:
-- Values:
--    0 - not implemented at all ($prog.txt file does not exist)
--    1 - $prog.txt file exists, but there was an error parsing it
--    2 - $prog.txt parsed OK, but the build failed (error return value)
--    3 - $prog built OK, but execution timed out at least once
--    4 - $prog execution always completed (but there were errors)
--    5 - $prog execution always completed without errors
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
