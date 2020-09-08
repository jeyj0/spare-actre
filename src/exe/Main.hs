{-# LANGUAGE DeriveGeneric #-}
module Main where

import Dhall
  ( FromDhall
  , Natural
  , input
  , auto
  , Generic
  )
import Chrono.TimeStamp
import System.Exit (die)
import qualified Library as Lib
import System.Environment (getArgs)
import qualified Data.Text as T
import Filesystem.Path.CurrentOS
  ( fromText
  , toText
  , (</>)
  )
import Filesystem
  ( isDirectory
  , isFile
  , readTextFile
  , IOMode (..)
  )

storageFileName :: T.Text
storageFileName = T.pack "learning-data.dhall"

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

        let filePath = dirPath </> (fromText storageFileName)
        saveDataFile filePath prompts []

        return ()
      else
        die $ "Not a valid directory: " ++ path

data Data = Data
  { _prompts :: [Lib.Prompt]
  } deriving (Show)

data DhallData = DhallData
  { prompts :: [DhallPrompt]
  } deriving (Generic, Show)
instance FromDhall DhallData

data DhallPrompt = DhallPrompt
  { _id :: String
  , question :: String
  , answer :: String
  , reviews :: [DhallReview]
  } deriving (Generic, Show)
instance FromDhall DhallPrompt

data DhallReview = DhallReview
  { time :: String
  , wasKnown :: Bool
  } deriving (Generic, Show)
instance FromDhall DhallReview

saveDataFile filePath prompts reviews = do
  simplePath <- case toText filePath of
    Right p ->
      return p
    Left p ->
      die $ "Unexpected error reading file path: " ++ T.unpack p

  dhallData <- input auto simplePath
  print (dhallData :: DhallData)

  let _data = convertToData dhallData
  let prompts' = _prompts _data

  putStrLn "########## From File:"
  sequence $ map (putStrLn . show) prompts'
  putStrLn "########## From Org:"
  sequence $ map (putStrLn . show) prompts

  return ()

toPath s = fromText $ T.pack s

convertToData :: DhallData -> Data
convertToData dhallData =
  let
    ps :: [Lib.Prompt]
    ps = Lib.for (prompts dhallData) $ \p ->
      let
        rs :: [Lib.Review]
        rs = Lib.for (reviews p) $ \r ->
          Lib.Review
            { Lib.time = read $ time r
            , Lib.wasKnown = wasKnown r
            }
      in
      Lib.Prompt
        { Lib._id = T.pack $ _id p
        , Lib.question = T.pack $ question p
        , Lib.answer = T.pack $ answer p
        , Lib.reviews = rs
        }
  in
  Data { _prompts = ps }

convertToDhallData :: Data -> DhallData
convertToDhallData data' =
  let
    ps :: [DhallPrompt]
    ps = Lib.for (_prompts data') $ \p ->
      let
        rs :: [DhallReview]
        rs = Lib.for (Lib.reviews p) $ \r ->
          DhallReview
            { time = show $ Lib.time r
            , wasKnown = Lib.wasKnown r
            }
      in
      DhallPrompt
        { _id = T.unpack $ Lib._id p
        , question = T.unpack $ Lib.question p
        , answer = T.unpack $ Lib.answer p
        , reviews = rs
        }
  in
  DhallData { prompts = ps }
