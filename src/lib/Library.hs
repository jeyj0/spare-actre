module Library where

import Crypto.Hash (hashWith, SHA256 (..))
import Data.ByteString (ByteString)
import Data.ByteArray.Encoding (convertToBase, Base (Base64))
import Data.Text.Encoding
  ( encodeUtf8
  , decodeUtf8
  )
import Chrono.TimeStamp (TimeStamp)
import Control.Monad (filterM)
import System.Directory
  ( getDirectoryContents
  )
import Filesystem
  ( isFile
  , listDirectory
  , readTextFile
  )
import Filesystem.Path.CurrentOS (extension)
import qualified Data.Text as T

for :: [a] -> (a -> b) -> [b]
for l f = map f l

hash :: T.Text -> T.Text
hash t =
  T.init $ T.tail $ T.pack $
    show ((convertToBase Base64 $ hashWith SHA256 $ encodeUtf8 t) :: ByteString)

type PromptId = T.Text

data OrgPrompt = OrgPrompt
  { orgPromptId :: PromptId
  , orgQuestion :: T.Text
  , orgAnswer :: T.Text
  } deriving (Show, Eq)

data Prompt = Prompt
  { _id :: PromptId
  , question :: T.Text
  , answer :: T.Text
  , reviews :: [Review]
  } deriving (Show, Eq)

data Review = Review
  { time :: TimeStamp
  , wasKnown :: Bool
  } deriving (Show, Eq)

promptsFromFileContents :: [T.Text] -> [OrgPrompt]
promptsFromFileContents [] = []
promptsFromFileContents (file:fileContents) =
  promptsFromFile file ++ promptsFromFileContents fileContents
  where
    promptsFromFile :: T.Text -> [OrgPrompt]
    promptsFromFile file =
      let
        lines' = T.lines file
      in
        promptsFromLines [] lines'

    promptsFromLines :: [OrgPrompt] -> [T.Text] -> [OrgPrompt]
    promptsFromLines prompts [] = prompts
    promptsFromLines prompts (line:lines) =
      if T.isPrefixOf (T.pack "#+begin_prompt") (T.toLower line) then
        let
          (prompt, lines') = finishPrompt lines

          prompts' :: [OrgPrompt]
          prompts' = case prompt of
            Nothing ->
              prompts
            Just p ->
              p:prompts
        in
        promptsFromLines prompts' lines'
      else
        promptsFromLines prompts lines

    finishPrompt :: [T.Text] -> (Maybe OrgPrompt, [T.Text])
    finishPrompt [] = (Nothing, [])
    finishPrompt lines =
      let
        questionFromLines :: [T.Text] -> Maybe (T.Text, [T.Text])
        questionFromLines [] = Nothing
        questionFromLines lines' =
          let
            questionFromLines' :: T.Text -> [T.Text] -> Maybe (T.Text, [T.Text])
            questionFromLines' q [] = Nothing
            questionFromLines' q (line:lines'') =
              if T.isPrefixOf (T.pack "-----") line then
                Just (q, lines'')
              else
                questionFromLines' (T.append line q) lines''
          in
          questionFromLines' (T.pack "") lines'

        answerFromLines :: [T.Text] -> Maybe (T.Text, [T.Text])
        answerFromLines [] = Nothing
        answerFromLines lines' =
          let
            answerFromLines' :: T.Text -> [T.Text] -> Maybe (T.Text, [T.Text])
            answerFromLines' a [] = Nothing
            answerFromLines' a (line:lines'') =
              if T.isPrefixOf (T.pack "#+end_prompt") (T.toLower line) then
                Just (a, lines'')
              else
                answerFromLines' (T.append line a) lines''
          in
          answerFromLines' (T.pack "") lines'
      in
      case questionFromLines lines of
        Nothing ->
          (Nothing, [])
        Just (question, lines') ->
          case answerFromLines lines' of
            Nothing ->
              (Nothing, [])
            Just (answer, lines'') ->
              let
                _id = hash question

                prompt = OrgPrompt
                  { orgPromptId = _id
                  , orgAnswer = answer
                  , orgQuestion = question
                  }
              in
              (Just prompt, lines'')

collectPromptsFromDirectory dirPath = do
  directoryContents <- listDirectory dirPath
  filePaths <- filterM isFile $
    filter (\p -> extension p == (Just $ T.pack "org")) directoryContents
  fileContents <- sequence $ map readTextFile filePaths

  return $ promptsFromFileContents fileContents

parseDataFile :: T.Text -> ([Prompt], [Review])
parseDataFile dataFile = parseDataFile' (T.lines dataFile) ([], [])

parseDataFile' :: [T.Text] -> ([Prompt], [Review]) -> ([Prompt], [Review])
parseDataFile' [] r = r
parseDataFile' (line:lines) (prompts, reviews) =
  if T.isPrefixOf (T.pack "p") line then
    let
      prompt :: Maybe Prompt
      prompt = parsePrompt $ T.tail line
    in
    case prompt of
      Nothing ->
        parseDataFile' lines (prompts, reviews)
      Just p ->
        parseDataFile' lines (p:prompts, reviews)
  else if T.isPrefixOf (T.pack "r") line then
    let
      review :: Maybe Review
      review = parseReview $ T.tail line
    in
    case review of
      Nothing ->
        parseDataFile' lines (prompts, reviews)
      Just r ->
        parseDataFile' lines (prompts, r:reviews)
  else
    parseDataFile' lines (prompts, reviews)

parsePrompt :: T.Text -> Maybe Prompt
parsePrompt = undefined

parseReview :: T.Text -> Maybe Review
parseReview = undefined

