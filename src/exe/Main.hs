module Main where

import qualified Library as Lib
import System.Environment (getArgs)

notesDirectory :: FilePath
notesDirectory = "/home/jeyj0/org/roam/notes"

main :: IO ()
main = do
  args <- getArgs
  main' args

main' :: [String] -> IO ()
main' [] = putStrLn "Please provide a path to your notes directory."
main' (path:args) = Lib.main path

