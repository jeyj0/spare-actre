module Main where

import System.Exit (die)
import qualified Library as Lib
import System.Environment (getArgs)
import qualified Data.Text as T
import Filesystem.Path.CurrentOS (fromText)
import Filesystem (isDirectory)

main :: IO ()
main = do
  args <- getArgs

  case args of
    [] ->
      die "Please provide a path to your notes directory."

    (path:args) -> do
      let dirPath = toPath path
      isDirectory' <- isDirectory dirPath

      if isDirectory' then do
        prompts <- Lib.collectPromptsFromDirectory dirPath
        sequence $ map (putStrLn . show) prompts
        return ()
      else
        die $ "Not a valid directory: " ++ path

toPath s = fromText $ T.pack s

