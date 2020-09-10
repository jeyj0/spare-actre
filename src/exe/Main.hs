{-# LANGUAGE DeriveGeneric #-}
module Main where

import qualified MainCopy as M
import Dhall
  ( FromDhall
  , Natural
  , input
  , auto
  , Generic
  )
import System.Exit (die)
import qualified Library as Lib
import System.Environment (getArgs)
import qualified Data.Text as T
import Filesystem.Path.CurrentOS
  ( fromText
  , toText
  , (</>)
  )
import Filesystem (isDirectory)

main :: IO ()
main = do
  args <- getArgs

  case args of
    [] ->
      die "Please provide a path to your notes directory."

    (path:args) -> do
      let dirPath = M.toPath path
      isDirectory' <- isDirectory dirPath

      if isDirectory' then do
        prompts <- Lib.collectPromptsFromDirectory dirPath

        let filePath = dirPath </> (fromText M.storageFileName)

        simplePath <- case toText filePath of
          Right p ->
            return p
          Left p ->
            die $ "Unexpected error reading file path: " ++ T.unpack p

        dhallData <- input auto simplePath

        let _data = M.convertToData dhallData
        let prompts' = M._prompts _data

        let newData = M.mergeFileAndOrgPrompts prompts _data
        putStrLn "########## newData:"
        putStrLn $ show newData

        M.questionLoop M.intervals filePath newData
      else
        die $ "Not a valid directory: " ++ path

