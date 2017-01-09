module Main where

import           Control.Applicative ((<$>))
import           Control.Monad       (unless, void, when)

import           System.Directory    (doesFileExist, getPermissions,
                                      readable, renameFile)
import           System.IO           (IOMode (WriteMode), hPutStrLn,
                                      withFile)

import           Arguments           (Options (..), parseArgs,
                                      printHelp)
import           ExternChecks        (exitPermissions)
import           ProgramExecution    (RunType (..), showProgExec)
import           ProjectTest         (execute)

main :: IO ()
main = do
    opts <- parseArgs
    let progDir = _progdir opts
        commentsFile = _progdir opts ++ "/comments.txt"
        commentsFileBackup = _progdir opts ++ "/comments.bak"

    unless (_optionsSet opts) $ void printHelp

    testSuiteRoot <- (++ "/test-suite") <$> _testdir opts
    testPerms     <- getPermissions testSuiteRoot
    commentsExist <- doesFileExist commentsFile

    unless (readable testPerms) $ exitPermissions testSuiteRoot

    when commentsExist $ do
        putStrLn $
            commentsFile ++ " exists -- making backup comments.bak"
        renameFile commentsFile commentsFileBackup
    withFile commentsFile WriteMode $ \h -> do
      results <- mapM (execute h (testSuiteRoot, progDir))
                      [ Simulate, Minimize, Searcher
                      , BoolopComp, BoolopProd, Invhom, Properties
                      ]
      hPutStrLn h "-----------------------------------------------------"
      mapM_ (flip (>>) (hPutStrLn h "") . hPutStrLn h . showProgExec) results
    putStrLn $ "Done.\nComments are in " ++ commentsFile
