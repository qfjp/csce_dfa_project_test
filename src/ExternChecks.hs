module ExternChecks where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad (unless)
import System.Directory
  ( executable
  , getCurrentDirectory
  , getPermissions
  , readable
  , removeFile
  )
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.FilePath (takeFileName)
import System.IO (hClose, openTempFile)
import System.Process (readProcess, readProcessWithExitCode)

exitPermissions :: FilePath -> IO ()
exitPermissions path
  = do
      putStrLn $ "Bad permissions on file: " ++ path
      exitWith (ExitFailure 1)

-- TODO: A lot
checkIsomorphism :: T.Text -> FilePath -> FilePath -> FilePath -> IO Bool
checkIsomorphism dfaText dfaFile binPath isoChecker
  = do
      let isDfaPath = binPath ++ "/isDFA"
      isoCheckPerms <- getPermissions isoChecker
      unless (readable isoCheckPerms && executable isoCheckPerms) $
          exitPermissions isoChecker

      (_, isDfa, isDfaErr)  <- readProcessWithExitCode isDfaPath [] (T.unpack dfaText)
      let isDfaResult = head . lines $ isDfa
      if isDfaResult == "Not a DFA"
      then return False
      else do
        isomorphism <- readProcess isoChecker
                                   [dfaFile] (T.unpack dfaText)
        let firstLine =  head . lines $ isomorphism
        return $ firstLine == "The two DFAs are isomorphic."

checkEquivalence :: T.Text -> FilePath -> FilePath -> FilePath -> IO Bool
checkEquivalence dfaText dfaFile binPath isoChecker
  = do
      let isDfaPath = binPath ++ "/isDFA"
      isoCheckPerms <- getPermissions isoChecker
      unless (readable isoCheckPerms && executable isoCheckPerms) $
          exitPermissions isoChecker

      (_, isDfa, isDfaErr)  <- readProcessWithExitCode isDfaPath [] (T.unpack dfaText)
      let isDfaResult = head . lines $ isDfa
      if isDfaResult == "Not a DFA"
      then return False
      else do
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
