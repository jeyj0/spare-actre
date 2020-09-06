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

data Prompt = Prompt
  { question :: T.Text
  , answer :: T.Text
  , _id :: PromptId
  }
  deriving (Show, Eq)

data Review = Review
  { promptId :: PromptId
  , time :: TimeStamp
  , wasKnown :: Bool
  }
  deriving (Show, Eq)

promptsFromFileContents :: [T.Text] -> [Prompt]
promptsFromFileContents [] = []
promptsFromFileContents (file:fileContents) =
  promptsFromFile file ++ promptsFromFileContents fileContents
  where
    promptsFromFile :: T.Text -> [Prompt]
    promptsFromFile file =
      let
        lines' = T.lines file
      in
        promptsFromLines [] lines'

    promptsFromLines :: [Prompt] -> [T.Text] -> [Prompt]
    promptsFromLines prompts [] = prompts
    promptsFromLines prompts (line:lines) =
      if T.isPrefixOf (T.pack "#+begin_prompt") (T.toLower line) then
        let
          (prompt, lines') = finishPrompt lines

          prompts' :: [Prompt]
          prompts' = case prompt of
            Nothing ->
              prompts
            Just p ->
              p:prompts
        in
        promptsFromLines prompts' lines'
      else
        promptsFromLines prompts lines

    finishPrompt :: [T.Text] -> (Maybe Prompt, [T.Text])
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

                prompt = Prompt
                  { question = question
                  , answer = answer
                  , _id = _id
                  }
              in
              (Just prompt, lines'')

collectPromptsFromDirectory dirPath = do
  directoryContents <- listDirectory dirPath
  filePaths <- filterM isFile $
    filter (\p -> extension p == (Just $ T.pack "org")) directoryContents
  fileContents <- sequence $ map readTextFile filePaths

  return $ promptsFromFileContents fileContents

