{-|
Module      : Arguments
Description : Control command line arguments to the CLI

This module controls the options that are passed in by the user on
first invocation. Options are available for single-user and batch modes
-}
module Arguments (Options(..), parseArgs, printHelp) where

import Data.Foldable (foldlM)
import System.Console.GetOpt
import System.Environment (getArgs, getEnv, getProgName)
import System.Exit (exitSuccess)

-- | The record storing command line options
data Options
    = Options { -- | Single user test mode flag
                _selftest :: Bool
                -- | The location of the test suite
              , _testdir :: IO FilePath
                -- | The location of the project to test
              , _progdir :: FilePath
                -- | Whether any options have been invoked
              , _optionsSet :: Bool
              }

-- | Command line options
options :: [OptDescr (Options -> IO Options)]
options
  = [ Option ['h'] ["help"]      (NoArg $ const printHelp)
             "print usage information"
    , Option ['t'] ["single-test"] (ReqArg setSingleTest "DIR")
             "test program at given directory"
    , Option ['d'] ["test-suite"] (ReqArg setTestDirectory "DIR")
             "the location of the test-suite and auxiliary binaries"
    ]

-- | The default test-suite directory
defBaseDir :: IO FilePath
defBaseDir
  = getEnv "HOME" >>=
      (\h -> return $ h ++ "/public_html/csce355/prog-proj")

-- | The message to print on invoking the program incorrectly.
helpMessage :: String
helpMessage
  = usageInfo "" options

-- | Parse the arguments list as an Options record
parseArgs :: IO Options
parseArgs
  = do
      argv <- getArgs
      case getOpt RequireOrder options argv of
        (opts, _, []) -> foldlM (flip id) defaultOptions opts
        (_, _, errs) -> ioError (userError (concat errs))

-- | The default state of the options record without invoking
-- any flags
defaultOptions :: Options
defaultOptions
  = Options { _selftest = False
            , _testdir = defBaseDir
            , _progdir = "."
            , _optionsSet = False
            }

-- | Set the location of the test-suite
setTestDirectory :: String -> Options -> IO Options
setTestDirectory str opts
  = return $ opts { _testdir = return str, _optionsSet = True }

-- | Set the location of the project for single user mode
setSingleTest :: FilePath -> Options -> IO Options
setSingleTest str opts
  = return $ opts { _selftest = True, _progdir = str, _optionsSet = True }

-- | Print usage and die
printHelp :: IO Options
printHelp
  = do
      progName <- getProgName
      putStr progName
      putStr ": "
      putStrLn "A program to test the CSCE 355 project"
      putStr helpMessage
      exitSuccess
