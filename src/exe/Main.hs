{-# LANGUAGE DeriveGeneric #-}
module Main where

import qualified Data.HashMap.Strict as HM
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

data Prompt = Prompt
  { hQuestion :: T.Text
  , hAnswer :: T.Text
  , hReviews :: [Lib.Review]
  , hActive :: Bool
  } deriving (Show, Eq)

data Data = Data
  { _prompts :: HM.HashMap Lib.PromptId Prompt
  } deriving (Show)

data DhallData = DhallData
  { prompts :: HM.HashMap String DhallPrompt
  } deriving (Generic, Show)
instance FromDhall DhallData

data DhallPrompt = DhallPrompt
  { question :: String
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

  let _data = convertToData dhallData
  let prompts' = _prompts _data

  putStrLn "########## From File:"
  sequence $ HM.map (putStrLn . show) prompts'
  putStrLn "########## From Org:"
  sequence $ map (putStrLn . show) prompts

  let newData = mergeFileAndOrgPrompts prompts _data
  putStrLn "########## newData:"
  putStrLn $ show newData

  return ()

toPath s = fromText $ T.pack s

convertToData :: DhallData -> Data
convertToData dhallData =
  let
    fn
      :: String
      -> DhallPrompt
      -> HM.HashMap Lib.PromptId Prompt
      -> HM.HashMap Lib.PromptId Prompt
    fn key p hashMap =
      HM.insert (T.pack key) prompt hashMap
      where
        prompt :: Prompt
        prompt =
          let
            rs :: [Lib.Review]
            rs = Lib.for (reviews p) $ \r ->
              Lib.Review
                { Lib.time = read $ time r
                , Lib.wasKnown = wasKnown r
                }
          in
          Prompt
            { hQuestion = T.pack $ question p
            , hAnswer = T.pack $ answer p
            , hReviews = rs
            , hActive = False
            }

    ps :: HM.HashMap Lib.PromptId Prompt
    ps = HM.foldrWithKey fn HM.empty $ prompts dhallData
  in
  Data { _prompts = ps }

mergeFileAndOrgPrompts :: [Lib.OrgPrompt] -> Data -> Data
mergeFileAndOrgPrompts orgPrompts data' =
  let
    foldFn
      :: HM.HashMap Lib.PromptId Prompt
      -> Lib.OrgPrompt
      -> HM.HashMap Lib.PromptId Prompt
    foldFn promptMap orgPrompt =
      let
        orgPromptId = Lib.orgPromptId orgPrompt

        prompt = Prompt
          { hQuestion = Lib.orgQuestion orgPrompt
          , hAnswer = Lib.orgAnswer orgPrompt
          , hReviews = []
          , hActive = True
          }

        insertFn :: Prompt -> Prompt -> Prompt
        insertFn orgPrompt' dhallPrompt = Prompt
          { hQuestion = hQuestion orgPrompt'
          , hAnswer = hAnswer orgPrompt'
          , hReviews = hReviews dhallPrompt
          , hActive = True
          }
      in
      HM.insertWith insertFn orgPromptId prompt promptMap

    mergedPrompts :: HM.HashMap Lib.PromptId Prompt
    mergedPrompts = foldl
      foldFn
      (HM.map (\prompt ->
        Prompt
          { hQuestion = hQuestion prompt
          , hAnswer = hAnswer prompt
          , hReviews = hReviews prompt
          , hActive = False
          }) $ _prompts data')
      orgPrompts
  in
  Data { _prompts = mergedPrompts }

convertToDhallData :: Data -> DhallData
convertToDhallData data' =
  let
    fn
      :: Lib.PromptId
      -> Prompt
      -> HM.HashMap String DhallPrompt
      -> HM.HashMap String DhallPrompt
    fn key prompt hashMap =
      HM.insert (T.unpack key) dhallPrompt hashMap
      where
        dhallPrompt :: DhallPrompt
        dhallPrompt =
          let
            rs :: [DhallReview]
            rs = Lib.for (hReviews prompt) $ \r ->
              DhallReview
                { time = show $ Lib.time r
                , wasKnown = Lib.wasKnown r
                }
          in
          DhallPrompt
            { question = T.unpack $ hQuestion prompt
            , answer = T.unpack $ hAnswer prompt
            , reviews = rs
            }

    ps :: HM.HashMap String DhallPrompt
    ps = HM.foldrWithKey fn HM.empty $ _prompts data'
  in
  DhallData { prompts = ps }
