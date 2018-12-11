#!/usr/bin/env runhaskell
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module ProjectTest where

import           Parser.Build
import           Parser.Simulate
import           ProgramExecution

import           Debug.Trace

import           Text.Parsec.String   (parseFromFile)

import           Control.Applicative  ((<$>))
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Except (MonadError, runExceptT,
                                       throwError)

import           Data.Dfa.Equivalence (equivalentText, isomorphicText)
import           Data.Maybe
import           Data.Monoid          (Sum (Sum))
import qualified Data.Text            as T
import qualified Data.Text.IO         as T

import           System.Directory
import           System.Exit
import           System.IO
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
timeout = 90

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

-- | (Name, (ExitStates, StdOut, StdErr))
type ProgramOutput = (String, (ExitCode, String, String))
progErr :: ProgramOutput -> T.Text
progErr (_, (_, _, x)) = T.pack x
progOut :: ProgramOutput -> T.Text
progOut (_, (_, x, _)) = T.pack x
progExitCode :: ProgramOutput -> ExitCode
progExitCode (_, (x, _, _)) = x
progName :: ProgramOutput -> T.Text
progName (x, (_, _, _)) = T.pack x

progNameErr :: ProgramOutput -> (String, T.Text)
progNameErr (x, (_, _, y)) = (x, T.pack y)

runProc :: Handle -> RunType -> Maybe FilePath -> [String] -> [String]
        -> IO (Maybe ProgramOutput)
runProc _ _ _ _ [] = return Nothing
runProc h typ maybCwd testFiles (process:args)
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
      hPutStrLn h fullProcStr
      result <- timeProc action
      case result of
        Nothing -> failExecution h [(fullProcStr, "TIMED OUT")] TimeOut typ
                   >> return Nothing -- TODO: get rid of this Nothing
                                     -- since it masks the failure of
                                     -- the timeout
        Just x  -> return . Just $ (fullProcStr, x)

failExecution :: Handle -> [(String, T.Text)] -> RunLevel -> RunType
              -> IO (ProgramExecution)
failExecution h failures failType typ
  = do
      let (command, errorMsg) = head failures
      hPutStrLn h "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv"
      hPutStrLn h $ "  ERROR: " ++ (show command) ++ " "
                             ++ "failed with error:"
      hPutStrLn h $ unlines . map ("         " ++) . lines . T.unpack $ errorMsg
      hPutStrLn h "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
      return PE { _tag = typ
                , _errorCount = Sum $ length failures
                , _runLevel = failType }


execute :: Handle -> (FilePath, FilePath) -> RunType
        -> IO (ProgramExecution)
execute h (tests, this) typ
  = do
      let buildFile = this ++ "/" ++ rtToFile typ ++ ".txt"
          testDir = tests ++ "/" ++ rtToFile typ ++ "/"
      buildExists <- doesFileExist buildFile
      if not buildExists
      then
        return PE { _tag = typ, _errorCount = Sum 0
                  , _runLevel = NotImplemented }
      else do
        parsedFile <- parseFromFile parseBuildFile buildFile
        case parsedFile of
          Right (builds, run) -> do
            putStrLn $ "Building" ++ " " ++ show typ ++ "..."
            hPutStrLn h $ "Building" ++ " " ++ show typ ++ "..."
            buildResults' <- sequence <$>
                mapM (runProc h typ (Just this) [] . words) builds
            when (isNothing buildResults') $
                hPutStrLn h "  Build timed out"
            let buildResults
                  = fromMaybe [] buildResults'
                buildFailures
                  = filter ((/= ExitSuccess) . progExitCode)
                           buildResults
            if not (null buildFailures)
            then failExecution h (map progNameErr buildFailures) ParseError typ
            else do
                putStrLn $ "Running " ++ show typ ++ "..."
                hPutStrLn h $ "Running " ++ show typ ++ "..."
                let userCmd = words run
                    testFilesNoPath = map fst . testCases $ typ
                    testFiles = map (map (testDir ++)) testFilesNoPath
                runResults <-
                    mapM (\x -> runProc h typ (Just this) x userCmd)
                         testFiles
                case sequence runResults of
                  Nothing -> do
                      let numTimeOuts
                            = length . filter isNothing $ runResults
                      return PE { _tag = typ, _errorCount = Sum numTimeOuts
                                , _runLevel = TimeOut
                                }
                  Just results -> do
                      let failures
                            = filter ((/= ExitSuccess) . progExitCode)
                                     results
                      if not (null failures)
                      then failExecution h (map progNameErr failures) FinishWithSignalError typ
                      else do
                          hPutStrLn h "  Run succeeded, comparing output..."
                          let cmds = map progName results
                              outputs = map progOut results
                              answerFiles = map snd . testCases $ typ
                              answerFsInDir = map (testDir ++) answerFiles
                          comparisons <- (compareAnswers h
                                                         (zip cmds outputs)
                                                         answerFsInDir
                                                         typ)
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
                                    , _runLevel
                                        = if numWrong == 0
                                          then FinishPerfect
                                          else FinishWithError
                                    }
          Left err -> do
            failExecution h [(show typ, T.pack (show err))] ParseError typ

compareAnswers :: Handle -> [(T.Text, T.Text)] -> [FilePath] -> RunType
               -> IO [Bool]
compareAnswers h nameOuts ansFilePaths typ
  = case typ of
      Continued  -> return [False]
      Simulate   -> compareStringAnswers h typ nameOuts ansFilePaths
      Minimize   -> compareAs h typ Isomorphism nameOuts ansFilePaths
      Searcher   -> compareAs h typ Isomorphism nameOuts ansFilePaths
      Boolop     -> compareAs h typ Equivalence nameOuts ansFilePaths
      Invhom     -> compareAs h typ Equivalence nameOuts ansFilePaths
      Properties -> compareStringAnswers h typ nameOuts ansFilePaths

compareAs ::Handle -> RunType
          -> ComparisonType -> [(T.Text, T.Text)] -> [FilePath] -> IO [Bool]
compareAs h typ tag nameOuts ansFilePaths
  = do
      answers <- mapM T.readFile ansFilePaths
      let outsAndAns = map (\((cmd, out), ans) -> (cmd, (out, ans))) $ zip nameOuts answers
      mapM (checkFunc h typ) outsAndAns
  where
      checkFunc :: Handle -> RunType -> (T.Text, (T.Text, T.Text)) -> IO Bool
      checkFunc h typ x
        = do
            let f = case tag of
                    Isomorphism -> uncurry isomorphicText
                    Equivalence -> uncurry equivalentText
            results <- runExceptT . f . snd $ x
            case results of
               Right x -> return x
               Left e ->
                   failExecution h
                                 [(T.unpack . fst $ x, T.pack . show $ e)]
                                 FinishWithError
                                 typ
                   >> return False

compareStringAnswers :: Handle -> RunType
                     -> [(T.Text, T.Text)] -> [FilePath] -> IO [Bool]
compareStringAnswers h typ cmdOuts ansFilePaths
  = do
      answers <- mapM T.readFile ansFilePaths
      let compares = zip (map snd cmdOuts) answers
          withCmds = zip (map fst cmdOuts) compares
      mapM equalFunc withCmds
  where
      equalFunc :: (T.Text, (T.Text, T.Text)) -> IO Bool
      equalFunc (cmd, (out, ans))
        | typ == Simulate = simEqualFunc (cmd, (out, ans))
        | otherwise
            = if out == ans
              then return True
              else failExecution
                     h
                     [(T.unpack cmd, out `T.append` "\n" `T.append` ans)]
                     FinishWithError
                     typ
                   >> return False
      simEqualFunc :: (T.Text, (T.Text, T.Text)) -> IO Bool
      simEqualFunc (cmd', (out, ans))
        = let outP' = doParseSim out
              ansP' = doParseSim ans
              cmd = T.unpack cmd'
          in case outP' of
            Left e -> failExecution h [(cmd, T.pack . show $ e)] FinishWithError typ >> return False
            Right outP ->
                case ansP' of
                  Left e -> failExecution h [(cmd, T.pack . show $ e)] FinishWithError typ >> return False
                  Right ansP ->
                      if outP == ansP then return True
                      else failExecution h [(cmd, shrinkSimConflict outP ansP)] FinishWithError typ
                           >> return False

shrinkSimConflict :: [Bool] -> [Bool] -> T.Text
shrinkSimConflict out ans
  = if length out /= length ans
    then "number of answers don't match"
    else "Output disagrees:\n" `T.append` showShrink out ans
  where
      showShrink out ans
        = listToResponses out
        `T.append` "\n"
        `T.append` listToResponses ans
        `T.append` "\n"
        `T.append` T.stripEnd indicatorString
      listToResponses
        = foldr (\x y -> (if x then "A" else "R") `T.append` y) ""
      indicatorString = foldr (\x y -> (if x then " " else "^") `T.append` y) ""
                      $ zipWith (==) ans out


