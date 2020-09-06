module Library where

import Control.Monad (filterM)
import System.Directory
  ( getDirectoryContents
  )
import Filesystem
  ( isFile
  , isDirectory
  , listDirectory
  , readTextFile
  )
import Filesystem.Path.CurrentOS
  ( fromText
  , extension
  )
import qualified Data.Text as T

for :: [a] -> (a -> b) -> [b]
for l f = map f l

toPath s = fromText $ T.pack s

data Prompt = Prompt
  { question :: T.Text
  , answer :: T.Text
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
                prompt = Prompt { question = question, answer = answer }
              in
              (Just prompt, lines'')

collectPromptsFromDirectory dirPath = do
  directoryContents <- listDirectory dirPath
  filePaths <- filterM isFile $
    filter (\p -> extension p == (Just $ T.pack "org")) directoryContents
  fileContents <- sequence $ map readTextFile filePaths

  let prompts = promptsFromFileContents fileContents

  sequence $ map (putStrLn . show) prompts

  return ()

main :: String -> IO ()
main notesDirectory = do
  let dirPath = toPath notesDirectory
  isDirectory' <- isDirectory dirPath

  if isDirectory' then
    collectPromptsFromDirectory dirPath
  else
    putStrLn $ "Not a valid directory: " ++ notesDirectory
