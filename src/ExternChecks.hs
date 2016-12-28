module ExternChecks where

import Control.Monad (unless)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Parser.Dfa (doParseDfa)

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

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

checkDfa :: T.Text -> Bool
checkDfa dfaText
  = isRight $ doParseDfa dfaText

-- TODO: A lot
checkIsomorphism :: T.Text -> FilePath -> FilePath -> FilePath -> IO Bool
checkIsomorphism dfaText dfaFile binPath isoChecker
  = if not $ checkDfa dfaText
    then do
      isoCheckPerms <- getPermissions isoChecker
      unless (readable isoCheckPerms && executable isoCheckPerms) $
          exitPermissions isoChecker
      isomorphism <- readProcess isoChecker
                                 [dfaFile] (T.unpack dfaText)
      let firstLine =  head . lines $ isomorphism
      return $ firstLine == "The two DFAs are isomorphic."
    else return False

checkEquivalence :: T.Text -> FilePath -> FilePath -> FilePath -> IO Bool
checkEquivalence dfaText dfaFile binPath isoChecker
  = if not $ checkDfa dfaText
    then do
      isoCheckPerms <- getPermissions isoChecker
      unless (readable isoCheckPerms && executable isoCheckPerms) $
          exitPermissions isoChecker
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
    else return False
