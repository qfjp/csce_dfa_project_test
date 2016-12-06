#!/usr/bin/env runhaskell

module ProjectTest where

{- Haskell script for testing a CSCE 355 project submission on a linux
 -  box.
 -
 - If you are running this on the lab machines, you may need to do the
 - following:
 -
 - cabal update
 - cabal install process
 - cabal install directory
 -
 - Usage:
 -  $ ProjectTest.hs -t [your-submission-root-directory] -d [location of bin and test-suite]
 -}

import Parser
import Text.Parsec.String (parseFromFile)

--import Data.Default
import Control.Applicative (Applicative (..), (<$>))
import Data.Foldable (foldlM)
import Data.Maybe
import Data.Monoid (Monoid (..), Sum(Sum), (<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Concurrent
import Control.Monad

import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs, getProgName, getEnv)
import System.Exit
import System.IO
import System.Process
import System.FilePath



 -- dfas to feed to simulator
simTestCases :: [String]
simTestCases
  = [ "bigDFA", "biggerDFA", "handoutDFA", "randomDFA1"
    , "randomDFA2", "randomDFA3", "randomDFA4", "randomDFA5"
    ]
-- dfas to feed to minimizer
minTestCases :: [String]
minTestCases = map show [1..7 :: Int]
-- dfas to feed to searcher
searchTestCases :: [String]
searchTestCases = map show [1..8 :: Int]
 -- dfas to feed to complement
bopCompTestCases :: [String]
bopCompTestCases
  = ["randomHexDFA", "handoutDFA", "bigDFA", "bigDFA-comp"]
 -- dfas to feed to the product construction
bopProdTestCases :: [(String, String)]
bopProdTestCases
  = [ ("smallDFA", "smallDFA2")
    , ("smallerDFA", "handoutDFA")
    , ("handoutDFA", "smallerDFA")
    , ("searchDFA1", "searchDFA2")
    , ("searchDFA2", "searchDFA1")
    , ("bigDFA", "randomHexDFA")
    , ("bigDFA", "bigDFA-comp")
    , ("bigDFA-comp", "randomHexDFA")
    ]
 -- dfas to feed to the inverse homomorphism construction
homTestCases :: [String]
homTestCases
  = [ "bigDFA", "biggerDFA", "handoutDFA"
    , "randomDFA1", "randomDFA2", "randomDFA3"
    ]
-- dfas to feed to properties
propTestCases :: [String]
propTestCases
  = map show [1..8 :: Int]

------{ Miscellaneous values }-------
-- time limit for each run of your program (in seconds).
timeout :: Int
timeout = 11

--------------------------{ HERE BE DRAGONS }--------------------------
--------------{ NOTHING BELOW THIS LINE SHOULD BE CHANGED }------------
data ComparisonType
    = Isomorphism
    | Equivalence

data RunType
    = RunTypeUndefined
    | Simulate
    | Minimize
    | Searcher
    | BoolopComp
    | BoolopProd
    | Invhom
    | Properties
  deriving (Eq, Show)

instance Monoid RunType where
    mempty = RunTypeUndefined
    RunTypeUndefined `mappend` x = x
    x `mappend` RunTypeUndefined = x
    x `mappend` _ = x

rtToFile :: RunType -> String
rtToFile Simulate   = "simulator"
rtToFile Minimize   = "minimizer"
rtToFile Searcher   = "searcher"
rtToFile BoolopComp = "boolop"
rtToFile BoolopProd = "boolop"
rtToFile Invhom     = "invhom"
rtToFile Properties = "properties"
rtToFile RunTypeUndefined = "UNDEFINED"

data Result
  = NotImplemented
  | ParseError
  | BuildFail
  | TimeOut
  | FinishWithError
  | FinishPerfect
  | Unknown
  deriving (Enum, Eq, Show)

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

testCases :: RunType -> [([String], String)]
testCases typ
  = case typ of
      RunTypeUndefined -> []
      Simulate
        -> let simIn x = [x ++ ".txt", x ++ "-strings.txt"]
               simOut x = (x ++"-out.txt")
           in makeTests simIn simOut simTestCases
      Minimize
        -> let minIn x = ["nonminimalDFA" ++ x ++ ".txt" ]
               minOut x ="minDFA" ++ x ++ ".txt"
           in makeTests minIn minOut minTestCases
      Searcher
        -> let searchIn x = ["str" ++ x ++ ".txt"]
               searchOut x = "DFA" ++ x ++ ".txt"
           in makeTests searchIn searchOut searchTestCases
      BoolopComp
        -> let compIn x = [x ++ ".txt"]
               compOut x = x ++ "-comp.txt"
           in makeTests compIn compOut bopCompTestCases
      BoolopProd
        -> let prodIn (x, y) = [x ++ ".txt", y ++ ".txt"]
               prodOut (x, y) = x ++ "-x-" ++ y ++ ".txt"
           in makeTests prodIn prodOut bopProdTestCases
      Invhom
        -> let invIn x = [x ++ ".txt", x ++ "-hom.txt"]
               invOut x = x ++ "-out.txt"
           in makeTests invIn invOut homTestCases
      Properties
        -> let propIn x = ["DFA" ++ x ++ ".txt"]
               propOut x = "DFA" ++ x ++ "-out.txt"
           in makeTests propIn propOut propTestCases
  where
      makeTests :: (a -> [String]) -> (a -> String) -> [a] -> [([String], String)]
      makeTests inputs answers = map (liftM2 (,) inputs answers)

defBaseDir :: IO FilePath
defBaseDir
  = getEnv "HOME" >>=
      (\h -> return $ h ++ "/public_html/csce355/prog-proj")

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

exitPermissions :: FilePath -> IO ()
exitPermissions path
  = do
      putStrLn $ "Bad permissions on file: " ++ path
      exitWith (ExitFailure 1)

compete :: [IO a] -> IO a
compete actions
  = do
      mvar <- newEmptyMVar
      tids <- mapM (\a -> forkIO $ a >>= putMVar mvar) actions
      result <- takeMVar mvar
      mapM_ killThread tids
      return result

timeProc :: IO a -> IO (Maybe a)
timeProc action
  = compete [ fmap Just action
            , threadDelay (timeout * 1000000) >> return Nothing ]

trunc :: String -> String
trunc str
  | length str < num = str
  | otherwise = take (num - 3) str ++ "..."
  where
      num = 80

runProc :: Handle -> Maybe FilePath -> [String] -> [String]
        -> IO (Maybe (String, (ExitCode, String, String)))
runProc _ _ _ [] = return Nothing
runProc h maybCwd testFiles (process:args)
  = do
      testFiles' <- mapM canonicalizePath testFiles
      let fullProcStr'
            = process ++ " " ++
                foldr (\s x -> s ++ " " ++ x) "" (args ++ testFiles)
          fullProcStr
            = T.unpack . T.strip . T.pack $ fullProcStr'
          procToExec
            = (proc process (args ++ testFiles')){cwd = maybCwd}
          action = readCreateProcessWithExitCode procToExec ""
      hPutStr h "    "
      hPutStrLn h . trunc $ fullProcStr
      result <- timeProc action
      case result of
        Nothing -> hPutStrLn h "    TIMED OUT" >> return Nothing
        Just x -> return . Just $ (fullProcStr, x)

failExecution :: Handle -> [(String, (ExitCode, String, String))] -> Int
              -> RunType -> IO (ProgramExecution (Sum Int) Int)
failExecution h failures failCode typ
  = do
      let first = head failures
          errorMsg = (\(_, _, x) -> x) . snd $ first
      hPutStrLn h $ "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv"
      hPutStrLn h $ "  ERROR: " ++ (show . fst) first ++ " "
                             ++ "failed with error:"
      hPutStrLn h $ unlines . map ("         " ++) . lines $ errorMsg
      hPutStrLn h $ "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
      hPutStrLn h $ "  Abandoning " ++ show typ
      return PE { _tag = typ
                    , _errorCount = Sum $ length failures
                    , _result = failCode}

-- TODO break into blocks
execute :: Handle -> (FilePath, FilePath, FilePath) -> RunType
        -> IO (ProgramExecution (Sum Int) Int)
execute h (tests, bins, this) typ
  = do
      let buildFile = this ++ "/" ++ rtToFile typ ++ ".txt"
          testDir = tests ++ "/" ++ rtToFile typ ++ "/"
      buildExists <- doesFileExist buildFile
      if not buildExists
      then
        return PE { _tag = typ, _errorCount = Sum 0
                  , _result = 0
                  }
      else do
        parsedFile <- parseFromFile parseBuildFile buildFile
        case parsedFile of
          Right (builds, runs) -> do
            hPutStrLn h $ "Building" ++ " " ++ show typ ++ "..."
            buildResults' <- sequence <$>
                mapM (runProc h (Just this) [] . words) builds
            when (isNothing buildResults') $
                hPutStrLn h "  Build timed out"
            let buildResults
                  = fromMaybe [] buildResults'
                buildFailures
                  = filter (\(_, (x, _, _)) -> x /= ExitSuccess)
                           buildResults
            if not (null buildFailures)
            then failExecution h buildFailures 2 typ
            else do
                hPutStrLn h $ "Running " ++ show typ ++ "..."
                let userCmd = words . head $ runs
                    testFilesNoPath = map fst . testCases $ typ
                    testFiles = map (map (testDir ++)) testFilesNoPath
                    --cmds = map (\x -> (userCmd, x)) testFiles
                runResults <-
                    mapM (\x -> runProc h (Just this) x userCmd)
                         testFiles
                case sequence runResults of
                  Nothing -> do
                      let numTimeOuts
                            = length . filter isNothing $ runResults
                      hPutStrLn h "  ERROR: Time out"
                      hPutStrLn h $ "  Abandoning " ++ show typ
                      return PE { _tag = typ, _errorCount = Sum numTimeOuts
                                , _result = 3
                                }
                  Just results -> do
                      let failures
                            = filter (\(_, (x, _, _)) -> x /= ExitSuccess)
                                     results
                      if not (null failures)
                      then failExecution h failures 4 typ
                      else do
                          hPutStrLn h "  Run succeeded, comparing output..."
                          let outputs = map (\(_, (_, x, _)) -> T.pack x) results
                              answerFiles = map snd . testCases $ typ
                              answerFsInDir = map (testDir ++) answerFiles
                          comparisons <- compareAnswers outputs
                                                        answerFsInDir
                                                        bins typ
                          let numWrong = length . filter not $ comparisons
                              wrongAnswers
                                = map (fst . snd) . filter (not . fst) $
                                  zip comparisons results
                          unless (null wrongAnswers) $ do
                              hPutStrLn h
                                "  Errors encountered in the following runs: "
                              mapM_ (\x -> hPutStr h "    " >> hPutStrLn h x)
                                    wrongAnswers
                          when (null wrongAnswers) $
                              hPutStrLn h "  No errors encountered"
                          return PE { _tag = typ
                                    , _errorCount = Sum numWrong
                                    , _result = if numWrong == 0
                                                then 5
                                                else 4
                                    }
          Left err -> do
            hPutStr h "  Parse Error: "
            hPrint h err
            return PE { _tag = typ
                      , _errorCount = Sum 0
                      , _result = 1}

checkIsomorphism :: T.Text -> FilePath -> FilePath -> IO Bool
checkIsomorphism dfaText dfaFile isoChecker
  = do
      isoCheckPerms <- getPermissions isoChecker
      unless (readable isoCheckPerms && executable isoCheckPerms) $
          exitPermissions isoChecker
      isomorphism <- readProcess isoChecker
                                 [dfaFile] (T.unpack dfaText)
      let firstLine =  head . lines $ isomorphism
      return $ firstLine == "The two DFAs are isomorphic."

checkEquivalence :: T.Text -> FilePath -> FilePath -> IO Bool
checkEquivalence dfaText dfaFile isoChecker
  = do
      tempFilePath <- getCurrentDirectory
      let dfaFileName = takeFileName dfaFile
      (tempFname,  tempHandle) <- openTempFile tempFilePath (dfaFileName ++ ".tmp")
      T.hPutStrLn tempHandle dfaText
      hClose tempHandle
      isoCheckPerms <- getPermissions isoChecker
      unless (readable isoCheckPerms && executable isoCheckPerms) $
          exitPermissions isoChecker
      isomorphism <- readProcess isoChecker [dfaFile, tempFname] ""
      let firstLine =  head . lines $ isomorphism
      removeFile tempFname
      return $ firstLine == "The two DFAs are equivalent."

compareAnswers :: [T.Text] -> [FilePath] -> FilePath -> RunType
               -> IO [Bool]
compareAnswers outputs ansFilePaths binPath typ
  = case typ of
      RunTypeUndefined -> return [False]
      Simulate -> compareStringAnswers outputs ansFilePaths
      Minimize -> compareAs Isomorphism outputs ansFilePaths binPath
      Searcher -> compareAs Isomorphism outputs ansFilePaths binPath
      BoolopComp -> compareAs Equivalence outputs ansFilePaths binPath
      BoolopProd -> compareAs Equivalence outputs ansFilePaths binPath
      Invhom -> compareAs Equivalence outputs ansFilePaths binPath
      Properties -> compareStringAnswers outputs ansFilePaths

compareAs :: ComparisonType -> [T.Text] -> [FilePath] -> FilePath
          -> IO [Bool]
compareAs tag outputs ansFilePaths binPath
  = do
      let outsAndAns = zip outputs ansFilePaths
          isoCheckPath = binPath ++ "/" ++ execChecker
      mapM (\(o, a) -> checkFunc o a isoCheckPath) outsAndAns
  where
      (execChecker, checkFunc)
        = case tag of
            Isomorphism -> ("DFAiso", checkIsomorphism)
            Equivalence -> ("DFAequiv", checkEquivalence)

compareStringAnswers :: [T.Text] -> [FilePath] -> IO [Bool]
compareStringAnswers outputs ansFilePaths
  = do
      answers <- mapM T.readFile ansFilePaths
      let results = zipWith (==) outputs answers
      return results

helpMessage :: String
helpMessage
  = usageInfo "" options

parseArgs :: IO Options
parseArgs
  = do
      argv <- getArgs
      case getOpt RequireOrder options argv of
        (opts, _, []) -> foldlM (flip id) defaultOptions opts
        (_, _, errs) -> ioError (userError (concat errs))

data Options
    = Options { _selftest :: Bool
              , _testdir :: IO FilePath
              , _progdir :: FilePath
              , _deleteTemps :: Bool
              , _optionsSet :: Bool
              }

defaultOptions :: Options
defaultOptions
  = Options { _selftest = False
            , _testdir = defBaseDir
            , _progdir = "."
            , _deleteTemps = True
            , _optionsSet = False
            }

options :: [OptDescr (Options -> IO Options)]
options
  = [ Option ['h'] ["help"]      (NoArg $ const printHelp)
             "print usage information"
    , Option ['t'] ["single-test"] (ReqArg setSingleTest "DIR")
             "test program at given directory"
    , Option ['d'] ["test-suite"] (ReqArg setTestDirectory "DIR")
             "the location of the test-suite and auxiliary binaries"
    ]

setTestDirectory :: String -> Options -> IO Options
setTestDirectory str opts
  = return $ opts { _testdir = return str, _optionsSet = True }

setSingleTest :: FilePath -> Options -> IO Options
setSingleTest str opts
  = return $ opts { _selftest = True, _progdir = str, _optionsSet = True }

printHelp :: IO Options
printHelp
  = do
      progName <- getProgName
      putStr progName
      putStr ": "
      putStrLn "A program to test the CSCE 355 project"
      putStr helpMessage
      exitSuccess
