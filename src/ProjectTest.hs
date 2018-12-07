#!/usr/bin/env runhaskell
{-# LANGUAGE FlexibleContexts #-}

module ProjectTest where

import           Parser.Build
import           ProgramExecution

import           Text.Parsec.String        (parseFromFile)

import           Control.Applicative       ((<$>))
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.IO.Class    (MonadIO, liftIO)

import           Data.Dfa.Equivalence      (equivalentText,
                                            isomorphicText)
import           Data.Maybe
import           Data.Monoid               (Sum (Sum))
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T

import           System.Directory
import           System.Exit
import           System.IO
import           System.IO.Error           (catchIOError)
import           System.Process



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

-- | Returns a list of (inputs, output) pairs
testCases :: RunType -> [([String], String)]
testCases typ
  = case typ of
      Continued -> []
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
      Boolop
        -> let compIn x = [x ++ ".txt"]
               compOut x = x ++ "-comp.txt"
               complementTests = makeTests compIn compOut bopCompTestCases
               prodIn (x, y) = [x ++ ".txt", y ++ ".txt"]
               prodOut (x, y) = x ++ "-x-" ++ y ++ ".txt"
               productTests = makeTests prodIn prodOut bopProdTestCases
           in complementTests ++ productTests
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

runProc :: (MonadIO m, Functor m)
        => Handle -> Maybe FilePath -> [String] -> [String]
        -> m (Maybe (String, (ExitCode, String, String)))
runProc _ _ _ [] = return Nothing
runProc h maybCwd testFiles (process:args)
  = do
      testFiles' <- liftIO $ mapM canonicalizePath testFiles
      let fullProcStr'
            = process ++ " " ++
                foldr (\s x -> s ++ " " ++ x) "" (args ++ testFiles)
          fullProcStr
            = T.unpack . T.strip . T.pack $ fullProcStr'
          procToExec
            = (proc process (args ++ testFiles')){cwd = maybCwd}
          action = readCreateProcessWithExitCode procToExec ""
      liftIO $ hPutStr h "    "
      liftIO $ hPutStrLn h fullProcStr
      result <- liftIO $ timeProc action
      case result of
        Nothing -> liftIO (hPutStrLn h "    TIMED OUT") >> return Nothing
        Just x  -> return . Just $ (fullProcStr, x)

failExecution :: Handle -> [(String, (ExitCode, String, String))] -> Int
              -> RunType -> IO (ProgramExecution (Sum Int) Int)
failExecution h failures failCode typ
  = do
      let first = head failures
          errorMsg = (\(_, _, x) -> x) . snd $ first
      hPutStrLn h "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv"
      hPutStrLn h $ "  ERROR: " ++ (show . fst) first ++ " "
                             ++ "failed with error:"
      hPutStrLn h $ unlines . map ("         " ++) . lines $ errorMsg
      hPutStrLn h "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
      hPutStrLn h $ "  Abandoning " ++ show typ
      return PE { _tag = typ
                    , _errorCount = Sum $ length failures
                    , _result = failCode}

execute :: (MonadIO m, MonadError IOError m, Functor m)
        => Handle -> (FilePath, FilePath) -> RunType
        -> m (ProgramExecution (Sum Int) Int)
execute h (tests, this) typ
  = do
      let buildFile = this ++ "/" ++ rtToFile typ ++ ".txt"
          testDir = tests ++ "/" ++ rtToFile typ ++ "/"
      buildExists <- liftIO $ doesFileExist buildFile
      if not buildExists
      then
        return PE { _tag = typ, _errorCount = Sum 0
                  , _result = 0
                  }
      else do
        parsedFile <- liftIO $ parseFromFile parseBuildFile buildFile
        case parsedFile of
          Right (builds, run) -> do
            liftIO $ putStrLn $ "Building" ++ " " ++ show typ ++ "..."
            liftIO $ hPutStrLn h $ "Building" ++ " " ++ show typ ++ "..."
            buildResults' <- sequence <$>
                mapM (runProc h (Just this) [] . words) builds
            when (isNothing buildResults') $
                liftIO $ hPutStrLn h "  Build timed out"
            let buildResults
                  = fromMaybe [] buildResults'
                buildFailures
                  = filter (\(_, (x, _, _)) -> x /= ExitSuccess)
                           buildResults
            if not (null buildFailures)
            then liftIO $ failExecution h buildFailures 2 typ
            else do
                liftIO $ putStrLn $ "Running " ++ show typ ++ "..."
                liftIO $ hPutStrLn h $ "Running " ++ show typ ++ "..."
                let userCmd = words run
                    testFilesNoPath = map fst . testCases $ typ
                    testFiles = map (map (testDir ++)) testFilesNoPath
                runResults <-
                    mapM (\x -> runProc h (Just this) x userCmd)
                         testFiles
                case sequence runResults of
                  Nothing -> do
                      let numTimeOuts
                            = length . filter isNothing $ runResults
                      liftIO $ hPutStrLn h "  ERROR: Time out"
                      liftIO $ hPutStrLn h $ "  Abandoning " ++ show typ
                      return PE { _tag = typ, _errorCount = Sum numTimeOuts
                                , _result = 3
                                }
                  Just results -> do
                      let failures
                            = filter (\(_, (x, _, _)) -> x /= ExitSuccess)
                                     results
                      if not (null failures)
                      then liftIO $ failExecution h failures 4 typ
                      else do
                          liftIO $ hPutStrLn h "  Run succeeded, comparing output..."
                          let outputs = map (\(_, (_, x, _)) -> T.pack x) results
                              answerFiles = map snd . testCases $ typ
                              answerFsInDir = map (testDir ++) answerFiles
                          comparisons <- liftIO $ catchIOError
                                           (compareAnswers outputs
                                                           answerFsInDir
                                                           typ)
                                           (\e -> liftIO $ hPutStrLn h (show e) >> return [False])
                          let numWrong = length . filter not $ comparisons
                              wrongAnswers
                                = map (fst . snd) . filter (not . fst) $
                                  zip comparisons results
                          unless (null wrongAnswers) $ do
                              liftIO $ hPutStrLn h
                                "  Errors encountered in the following runs: "
                              mapM_ (\x -> liftIO $ hPutStr h "    " >> hPutStrLn h x)
                                    wrongAnswers
                          when (null wrongAnswers) $
                              liftIO $ hPutStrLn h "  No errors encountered"
                          return PE { _tag = typ
                                    , _errorCount = Sum numWrong
                                    , _result = if numWrong == 0
                                                then 5
                                                else 4
                                    }
          Left err -> do
            liftIO $ hPutStr h "  Parse Error: "
            liftIO $ hPrint h err
            return PE { _tag = typ
                      , _errorCount = Sum 0
                      , _result = 1}

compareAnswers :: (MonadError IOError m, MonadIO m, Functor m)
               => [T.Text] -> [FilePath] -> RunType
               -> m [Bool]
compareAnswers outputs ansFilePaths typ
  = case typ of
      Continued  -> return [False]
      Simulate   -> compareStringAnswers outputs ansFilePaths
      Minimize   -> compareAs Isomorphism outputs ansFilePaths
      Searcher   -> compareAs Isomorphism outputs ansFilePaths
      Boolop     -> compareAs Equivalence outputs ansFilePaths
      Invhom     -> compareAs Equivalence outputs ansFilePaths
      Properties -> compareStringAnswers outputs ansFilePaths

compareAs :: (MonadError IOError m, MonadIO m, Functor m)
          => ComparisonType -> [T.Text] -> [FilePath] -> m [Bool]
compareAs tag outputs ansFilePaths
  = do
      answers <- liftIO $ mapM T.readFile ansFilePaths
      let outsAndAns = zip outputs answers
      mapM (uncurry checkFunc) outsAndAns
  where
      checkFunc :: (MonadError IOError m, Functor m)
                => T.Text -> T.Text -> m Bool
      checkFunc
        = case tag of
            Isomorphism -> isomorphicText
            Equivalence -> equivalentText

compareStringAnswers :: (MonadError IOError m, MonadIO m, Functor m)
                     => [T.Text] -> [FilePath] -> m [Bool]
compareStringAnswers outputs ansFilePaths
  = do
      answers <- liftIO $ mapM T.readFile ansFilePaths
      let results = zipWith (==) outputs answers
      return results
