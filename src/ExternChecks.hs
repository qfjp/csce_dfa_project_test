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
import System.Info (os)
import System.Process (readProcess, readProcessWithExitCode)

binExtension :: String
binExtension = if os == "mingw32"
               then ".exe"
               else ""

exitPermissions :: FilePath -> IO ()
exitPermissions path
  = do
      putStrLn $ "Bad permissions on file: " ++ path
      exitWith (ExitFailure 1)

checkDfa :: T.Text -> FilePath -> IO Bool
checkDfa dfaText binPath
  = do
      let isDfaPath = binPath ++ "/isDFA" ++ binExtension
      dfaCheckPerms <- getPermissions isDfaPath
      unless (readable dfaCheckPerms && executable dfaCheckPerms) $
          exitPermissions isDfaPath
      (_, isDfa, isDfaErr) <- readProcessWithExitCode isDfaPath [] (T.unpack dfaText)
      let isDfaResult = head . lines $ isDfa
      return . not $ isDfaResult == "Not a DFA"

-- TODO: A lot
checkIsomorphism :: T.Text -> FilePath -> FilePath -> FilePath -> IO Bool
checkIsomorphism dfaText dfaFile binPath isoChecker
  = do
      isoCheckPerms <- getPermissions isoChecker
      unless (readable isoCheckPerms && executable isoCheckPerms) $
          exitPermissions isoChecker
      isDfa <- checkDfa dfaText binPath
      if not isDfa
      then return False
      else do
        isomorphism <- readProcess isoChecker
                                   [dfaFile] (T.unpack dfaText)
        let firstLine =  head . lines $ isomorphism
        return $ firstLine == "The two DFAs are isomorphic."

checkEquivalence :: T.Text -> FilePath -> FilePath -> FilePath -> IO Bool
checkEquivalence dfaText dfaFile binPath isoChecker
  = do
      isoCheckPerms <- getPermissions isoChecker
      unless (readable isoCheckPerms && executable isoCheckPerms) $
          exitPermissions isoChecker
      isDfa <- checkDfa dfaText binPath
      if not isDfa
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
