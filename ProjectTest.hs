#!/usr/bin/env runhaskell

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

import Text.Parsec
import Text.Parsec.String


------------------------{ INSTANCE UNIT TESTS }-----------------------
------------------------{ IGNORE THIS SECTION }-----------------------
-- import Test.Hspec
-- import Test.Hspec.Checkers
-- import Test.QuickCheck hiding (Result)
-- import Test.QuickCheck.Checkers
-- import Test.QuickCheck.Classes
--
-- instance Arbitrary RunType where
--     arbitrary
--       = frequency $ map (\x -> (1, return x))
--                         [ RunTypeUndefined, Simulate, Minimize
--                         , Searcher, BoolopComp, BoolopProd, Invhom
--                         , Properties ]
--
-- instance EqProp RunType where
--     (=-=) = eq
--
-- rtSpec :: SpecWith ()
-- rtSpec
--   = describe "RunType ?Monoid?" $
--       testBatch $ monoid (undefined :: RunType)
--
-- instance Arbitrary a => Arbitrary (ProgramExecution a) where
--     arbitrary = PE Simulate <$> arbitrary <*> arbitrary
--
-- instance Eq a => EqProp (ProgramExecution a) where
--     (=-=) = eq
--
-- peSpec :: SpecWith ()
-- peSpec
--   = describe "Program Execution ?Monad?" $ do
--       testBatch $ functor (undefined :: ProgramExecution (Int, String, Char))
--       testBatch $ applicative (undefined :: ProgramExecution (Int, String, Double))
--       testBatch $ monad (undefined :: ProgramExecution (Int, String, Double))
--       it "  <*> == ap" $
--         property $ \x y -> (x <*> y)
--           == ((x :: ProgramExecution (Int -> Int))
--                `ap` (y :: ProgramExecution Int))
----------------------------------------------------------------------

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
  = [ ("smallDFA","smallDFA2")
    , ("smallerDFA","handoutDFA")
    , ("handoutDFA","smallerDFA")
    , ("searchDFA1","searchDFA2")
    , ("searchDFA2","searchDFA1")
    , ("bigDFA","randomHexDFA")
    , ("bigDFA","bigDFA-comp")
    , ("bigDFA-comp","randomHexDFA")
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

testCases :: RunType -> [([String], String)]
testCases typ
  = case typ of
      Simulate
        -> map (\x ->
                 ( [x ++ ".txt", x ++ "-strings.txt"]
                 , x ++ "-out.txt"))
               simTestCases
      Minimize
        -> map (\x ->
                 ( ["nonminimalDFA" ++ x ++ ".txt" ]
                 , "minDFA" ++ x ++ ".txt"))
               minTestCases
      Searcher
        -> map (\x -> (["str" ++ x ++ ".txt"], "DFA" ++ x ++ ".txt"))
               searchTestCases
      Boolop
        -> let complements
                 = map (\x ->
                         ([x ++ ".txt"], x ++ "-comp.txt"))
                       bopCompTestCases
               products
                 = map (\(x,y) ->
                         ( [x ++ ".txt", y ++ ".txt"]
                         , x ++ "-x-" ++ y ++ ".txt"))
                       bopProdTestCases
          in complements ++ products
      Invhom
        -> map (\x ->
                 ([x ++ ".txt", x ++ "-hom.txt"], x ++ "-out.txt"))
               homTestCases
      Properties
        -> map (\x ->
                 (["DFA" ++ x ++ ".txt"], "DFA" ++ x ++ "-out.txt"))
               propTestCases

{------ Miscellaneous values -------}
-- time limit for each run of your program (in seconds).
timeout :: Int
timeout = 11

{-------------------------- HERE BE DRAGONS --------------------------}
{---------------------------------------------------------------------}
data RunType = Simulate | Minimize | Searcher
             | Boolop | Invhom | Properties
instance Show RunType where
    show Simulate   = "simulator"
    show Minimize   = "minimizer"
    show Searcher   = "searcher"
    show Boolop     = "boolop"
    show Invhom     = "invhom"
    show Properties = "properties"

parseTextThenColon :: Parser String
parseTextThenColon
  = do
      text <- many (upper <|> lower)
      _ <- oneOf ":" >> oneOf "\n"
      return text

parseCommands :: Parser String -> Parser a -> Parser [String]
parseCommands intro end
  = do
      _ <- intro
      commands <- manyTill anyChar (try . lookAhead $ end)
      let strip = T.unpack . T.strip . T.pack
      return . lines . strip $ commands

parseBuildFile :: Parser ([String], [String])
parseBuildFile
  = do
      buildInstrs <- parseCommands (string "Build:") parseTextThenColon
      runInstrs <- parseCommands (string "Run:") eof
      let strip = T.unpack . T.strip . T.pack
          runs = map strip runInstrs
          builds = map strip buildInstrs
      when (length runs /= 1) $ error "Need exactly one run command"
      return (builds, runs)

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

progressToText :: RunType -> Int -> String
progressToText rt p
  | p == 0 = "not implemented -- " ++ show rt ++ ".txt does not exist"
  | p == 1 = show rt ++ ".txt exists, but there was an error parsing it."
  | p == 2 = show rt ++ ".txt parsed OK, but build failed."
  | p == 3 = "built OK, but execution timed out at least once."
  | p == 4 = "execution always completed, but there were errors."
  | p == 5 = "execution always completed without errors."
  | otherwise = "??? unknown progress status for " ++ show rt

data ProgramExecution
    = PE { _tag :: RunType
         , _errorCount :: Int
         , _progress :: Int }

instance Show ProgramExecution where
    show PE {_tag = t, _errorCount = e, _progress = p}
      = show t ++ ": " ++ progressToText t p ++
          "\nprogress level " ++ show p ++ " with " ++ show e ++
          " execution errors"

--printOptions :: Options -> IO ()
--printOptions Options { _selftest = s, _dir = d }
--  = do
--      print s
--      d' <- d
--      print d'

main :: IO ()
main = do
    opts <- parseArgs
    unless (_optionsSet opts) $ printHelp >> return ()
    testSuiteRoot <- (++ "/test-suite") <$> _testdir opts
    binDir <- (++ "/bin") <$> _testdir opts
    binPerms <- getPermissions binDir
    let progDir = _progdir opts
    testPerms <- getPermissions testSuiteRoot
    unless (readable binPerms || readable testPerms) $
        error "unreadable directory"
    let commentsFile = _progdir opts ++ "/comments.txt"
        commentsFileBackup = _progdir opts ++ "/comments.bak"
    commentsExist <- doesFileExist commentsFile
    when commentsExist $ do
        putStrLn $
            commentsFile ++ " exists -- making backup comments.bak"
        renameFile commentsFile commentsFileBackup
    withFile commentsFile WriteMode $ \h -> do
      results <- mapM (execute h (testSuiteRoot, binDir, progDir))
                      [ Simulate, Minimize, Searcher
                      , Boolop, Invhom, Properties
                      ]

      hPutStrLn h "-----------------------------------------------------"
      --mapM_ (hPrint h) results
      mapM_ (flip (>>) (hPutStrLn h "") . hPrint h) results
    putStrLn $ "Done.\nComments are in " ++ commentsFile

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

failExecution :: [(String, (ExitCode, String, String))] -> Int
              -> RunType -> IO ProgramExecution
failExecution failures failCode typ
  = do
      let first = head failures
      putStrLn $ "  ERROR: " ++ (show . fst) first ++ " "
                             ++ "failed with error:"
      putStrLn $ "         " ++ (show . (\(_,_,x) -> x) . snd)
                                first
      putStrLn $ "  Abandoning " ++ show typ
      return PE { _tag = typ
                    , _errorCount = length failures
                    , _progress = failCode}

-- TODO break into blocks
execute :: Handle -> (FilePath, FilePath, FilePath) -> RunType
        -> IO ProgramExecution
execute h (tests, bins, this) typ
  = do
      let buildFile = this ++ "/" ++ show typ ++ ".txt"
          testDir = tests ++ "/" ++ show typ ++ "/"
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
          then failExecution buildFailures 2 typ
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
                    return PE { _tag = typ, _errorCount = numTimeOuts
                              , _progress = 3
                              }
                Just results -> do
                    let failures
                          = filter (\(_, (x, _, _)) -> x /= ExitSuccess)
                                   results
                    if not (null failures)
                    then failExecution failures 4 typ
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
                                  , _errorCount = numWrong
                                  , _progress = if numWrong == 0
                                                then 5
                                                else 4
                                  }
        Left err -> do
          hPutStr h "  Parse Error: "
          hPrint h err
          return PE { _tag = typ
                    , _errorCount = 0
                    , _progress = 1}

checkIsomorphism :: T.Text -> FilePath -> FilePath -> IO Bool
checkIsomorphism dfaText dfaFile isoChecker
  = do
      isoCheckPerms <- getPermissions isoChecker
      unless (readable isoCheckPerms && executable isoCheckPerms) $
          error $ isoChecker ++ " not executable"
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
          error $ isoChecker ++ " not executable"
      isomorphism <- readProcess isoChecker [dfaFile, tempFname] ""
      let firstLine =  head . lines $ isomorphism
      removeFile tempFname
      return $ firstLine == "The two DFAs are equivalent."

data ComparisonType = Isomorphism | Equivalence

compareAnswers :: [T.Text] -> [FilePath] -> FilePath -> RunType
               -> IO [Bool]
compareAnswers outputs ansFilePaths binPath typ
  = case typ of
      Simulate -> compareStringAnswers outputs ansFilePaths
      Minimize -> compareAs Isomorphism outputs ansFilePaths binPath
      Searcher -> compareAs Isomorphism outputs ansFilePaths binPath
      Boolop -> compareAs Equivalence outputs ansFilePaths binPath
      Invhom -> compareAs Equivalence outputs ansFilePaths binPath
      Properties -> compareStringAnswers outputs ansFilePaths

compareAs :: ComparisonType -> [T.Text] -> [FilePath] -> FilePath -> IO [Bool]
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
