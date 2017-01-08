module ExternChecks (exitPermissions) where

import           System.Exit (ExitCode (ExitFailure), exitWith)

exitPermissions :: FilePath -> IO ()
exitPermissions path
  = do
      putStrLn $ "Bad permissions on file: " ++ path
      exitWith (ExitFailure 1)
