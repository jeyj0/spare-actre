{-# LANGUAGE DeriveGeneric #-}
module MainCopy where

import Test.Hspec
import Test.QuickCheck

import Data.Int (Int64)
import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Function ((&))
import Data.List (intercalate)
import qualified Data.HashMap.Strict as HM
import Dhall
  ( FromDhall
  , Natural
  , input
  , auto
  , Generic
  )
import Chrono.TimeStamp
import System.IO (getLine, getChar)
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

intervals :: [Interval]
intervals = map ((*) $ 1000000000 * 60 * 60) $ -- convert from hours to nanoseconds
  [ 1, 2, 4, 8, 16 ] -- hours
  ++ (map ((*) 24)
  [ 1, 2, 4, 16 ] ++ -- days
  (map ((*) 30)
  [ 1, 2, 6, 12 ])) -- months (aka. 30 days)

wasKnownLoop :: IO Bool
wasKnownLoop = do
  putStrLn "Did you know the answer? [y/n]"
  userInput <- getChar

  case userInput of
    'y' -> do
      putStrLn "Good for you!"
      return True
    'n' -> do
      putStrLn "You'll surely do next time."
      return False
    _ -> do
      putStrLn "Sorry, I didn't understand you."
      wasKnownLoop

filterInactivePrompts :: HM.HashMap Lib.PromptId Prompt -> HM.HashMap Lib.PromptId Prompt
filterInactivePrompts allPrompts =
  HM.foldrWithKey insertIfActive HM.empty allPrompts
    where
      insertIfActive
        :: Lib.PromptId
        -> Prompt
        -> HM.HashMap Lib.PromptId Prompt
        -> HM.HashMap Lib.PromptId Prompt
      insertIfActive promptId prompt activePrompts =
        if hActive prompt then
          HM.insert promptId prompt activePrompts
        else
          activePrompts

showAnswerLoop :: IO ()
showAnswerLoop = do
  putStrLn "Type 'answer' to see the answer."
  i <- getLine

  case i of
    "answer" ->
      return ()
    _ ->
      showAnswerLoop

type Interval = Int64
type ScheduledTime = Interval

getScheduledTime :: [Interval] -> [Lib.Review] -> ScheduledTime
getScheduledTime [] _ = undefined -- this is a config problem
getScheduledTime _ [] = 0 -- schedule as early as possible
getScheduledTime intervals reviews =
  let
    sortedReviews = sortBy (comparing Lib.time) reviews

    wasLastReviewKnown = Lib.wasKnown $ head sortedReviews

    timeOfLastReview = unTimeStamp $ Lib.time $ head reviews
  in
  timeOfLastReview + case reviews of
    (Lib.Review { Lib.wasKnown = False }):_ ->
      head intervals -- wasn't known; start learning from beginning
    (Lib.Review { Lib.wasKnown = True }):[] ->
      if length intervals >= 2 then
        head $ tail intervals
      else
        head intervals
    (Lib.Review { Lib.wasKnown = True, Lib.time = t1 }):
      (Lib.Review { Lib.time = t2 }):[] ->
      let
        diff = (unTimeStamp t1) - (unTimeStamp t2)
      in
      case filter (\interval -> interval > diff) intervals of
        [] ->
          last intervals
        i:_ ->
          i

schedulePrompts :: [Interval] -> HM.HashMap Lib.PromptId Prompt -> HM.HashMap Lib.PromptId (ScheduledTime, Prompt)
schedulePrompts intervals promptsMap =
  HM.map
    (\prompt -> (getScheduledTime intervals $ hReviews prompt, prompt))
    promptsMap

keepScheduledInPastAndNow
  :: TimeStamp
  -> HM.HashMap k (ScheduledTime, a)
  -> HM.HashMap k (ScheduledTime, a)
keepScheduledInPastAndNow currentTime map =
  HM.filter
    (\(t, _a) -> t <= unTimeStamp currentTime)
    map

sortByScheduledTimeAsc :: [(v1, ScheduledTime, v2)] -> [(v1, ScheduledTime, v2)]
sortByScheduledTimeAsc list =
  sortBy (comparing snd') list
  where
    snd' (a, b, c) = b

-- questionLoop :: FilePath -> Data -> IO ()
questionLoop intervals dataFilePath state = do
  t <- getCurrentTimeNanoseconds

  let prompts =
        state
        & _prompts
        & filterInactivePrompts
        & schedulePrompts intervals
        & keepScheduledInPastAndNow t
        & HM.toList
        & map (\(i, (t, p)) -> (i, t, p))
        & sortByScheduledTimeAsc

  if length prompts <= 0 then
    putStrLn "You're done for now! Good job! :)"
  else do
    let (questionPromptId, _, questionPrompt) = head prompts

    putStrLn "\n"
    putStrLn $ T.unpack $ hQuestion questionPrompt

    showAnswer <- showAnswerLoop

    putStrLn "\n"
    putStrLn $ T.unpack $ hAnswer questionPrompt

    wasKnown <- wasKnownLoop

    review <- do
      t <- getCurrentTimeNanoseconds
      return $ Lib.Review t wasKnown

    let newPrompts = addReviewToPrompt questionPromptId review $ _prompts state
    let newState = Data newPrompts

    -- TODO save new state to file

    questionLoop intervals dataFilePath newState

    where
      addReviewToPrompt
        :: Lib.PromptId
        -> Lib.Review
        -> HM.HashMap Lib.PromptId Prompt
        -> HM.HashMap Lib.PromptId Prompt
      addReviewToPrompt promptId review promptMap =
        HM.adjust
          (\p -> Prompt
            { hQuestion = hQuestion p
            , hAnswer = hAnswer p
            , hActive = hActive p
            , hReviews = review:(hReviews p)
            })
          promptId promptMap

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

-- saveDataFile :: FilePath -> Data -> IO ()
saveDataFile filePath state = do
  putStrLn "########## fileOutput:"
  putStrLn $ convertToDhallFileContent $ convertToDhallData state

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

dhallFileContentPrefix :: String
dhallFileContentPrefix = unlines
  [ "let Review : Type ="
    , "{ time : Text"
    , ", wasKnown : Bool"
    , "}"
  , "let Prompt : Type ="
    , "{ question : Text"
    , ", answer : Text"
    , ", reviews : List Review"
    , "}"
  , "let MapEntry : Type ="
    , "{ mapKey : Text"
    , ", mapValue : Prompt"
    , "}"
  , "in"
  , "{ prompts = ["
  ]

dhallFileContentSuffix :: String
dhallFileContentSuffix = "] : List MapEntry}"

convertToDhallFileContent :: DhallData -> String
convertToDhallFileContent dhallData =
  dhallFileContentPrefix ++ dhallFileContentMain ++ dhallFileContentSuffix
  where
    dataPrompts = prompts dhallData

    dhallFileContentMain :: String
    dhallFileContentMain = intercalate "," $ HM.foldrWithKey mapWithPrompt [] dataPrompts

    mapWithPrompt :: String -> DhallPrompt -> [String] -> [String]
    mapWithPrompt key p fileContentPrompts =
      promptString:fileContentPrompts
      where
        promptString :: String
        promptString =
          "{ mapKey = \"" ++ key ++ "\"" ++
          ", mapValue = { question = \"" ++ question p ++ "\"" ++
            ", answer = \"" ++ answer p ++ "\"" ++
            ", reviews = [" ++
              (intercalate "," $ map reviewAsString $ reviews p) ++
            "] : List Review}}"
          where
            reviewAsString :: DhallReview -> String
            reviewAsString dhallReview =
              "{ time = \"" ++ time dhallReview ++ "\"" ++
              ", wasKnown = " ++ (show $ wasKnown dhallReview) ++
              "}"

tests = do
  describe "getScheduledTime" $ do
    let intervals = [1, 2, 3, 4, 5]
    let reviewTime :: TimeStamp
        reviewTime = read "2020-01-01T00:00:00.000000000Z"

    it "prioritizes prompts without reviews" $ do
      -- when
      let scheduledTime = getScheduledTime intervals []

      -- then
      scheduledTime `shouldBe` 0

    it "schedules prompt with failed review very soon" $ do
      -- given
      let review = Lib.Review
            { Lib.wasKnown = False
            , Lib.time = reviewTime
            }

      -- when
      let scheduledTime = getScheduledTime intervals [review]

      -- then
      scheduledTime `shouldBe` 1 + unTimeStamp reviewTime

    it "schedules prompt with one successful review relatively soon" $ do
      -- given
      let review = Lib.Review
            { Lib.wasKnown = True
            , Lib.time = reviewTime
            }

      -- when
      let scheduledTime = getScheduledTime intervals [review]

      -- then
      scheduledTime `shouldBe` 2 + unTimeStamp reviewTime

    it "schedules prompt with one successful review with the only available interval" $ do
      -- given
      let intervals = [7]
      let review = Lib.Review
            { Lib.wasKnown = True
            , Lib.time = reviewTime
            }

      -- when
      let scheduledTime = getScheduledTime intervals [review]

      -- then
      scheduledTime `shouldBe` (head intervals) + unTimeStamp reviewTime

    it "schedules prompt after success depending on diff to review before (fail) that" $ do
      -- given
      let intervals = [1, 2, 4, 8, 16, 32, 64]
      let review1 = Lib.Review
            { Lib.wasKnown = True
            , Lib.time = reviewTime
            }
      let review2 = Lib.Review
            { Lib.wasKnown = False
            , Lib.time = reviewTime - 5
            }

      -- when
      let scheduledTime = getScheduledTime intervals [review1, review2]

      -- then
      scheduledTime `shouldBe` 8 + unTimeStamp reviewTime

    it "schedules prompt after success depending on diff to review before (success) that" $ do
      -- given
      let intervals = [1, 2, 4, 8, 16, 32, 64]
      let review1 = Lib.Review
            { Lib.wasKnown = True
            , Lib.time = reviewTime
            }
      let review2 = Lib.Review
            { Lib.wasKnown = True
            , Lib.time = reviewTime - 4
            }

      -- when
      let scheduledTime = getScheduledTime intervals [review1, review2]

      -- then
      scheduledTime `shouldBe` 8 + unTimeStamp reviewTime

  describe "keepScheduledInPast" $ do
    it "filters future things; keeps current and past" $ do
      -- given
      let currTime :: TimeStamp
          currTime = read "2020-01-01T00:00:00.000000000Z"

      let unfilteredMap :: HM.HashMap Integer (ScheduledTime, Integer)
          unfilteredMap =
            HM.empty
            & HM.insert 1 (unTimeStamp currTime - 2, 1)
            & HM.insert 2 (unTimeStamp currTime - 1, 2)
            & HM.insert 3 (unTimeStamp currTime, 3)
            & HM.insert 4 (unTimeStamp currTime + 1, 4)
            & HM.insert 5 (unTimeStamp currTime + 2, 5)

      -- when
      let filteredMap = keepScheduledInPastAndNow currTime unfilteredMap

      -- then
      HM.size filteredMap `shouldBe` 3
      HM.member 1 filteredMap `shouldBe` True
      HM.member 2 filteredMap `shouldBe` True
      HM.member 3 filteredMap `shouldBe` True
      HM.member 4 filteredMap `shouldNotBe` True
      HM.member 5 filteredMap `shouldNotBe` True

  describe "sortByScheduledTimeAsc" $ do
    it "should sort the list ascending" $ do
      -- given
      let unsortedList =
            [ ("second", 1, "rand1")
            , ("first", 0, "rand2")
            , ("third", 6, "rand3")
            ]

      -- when
      let sortedList = sortByScheduledTimeAsc unsortedList

      -- then
      sortedList `shouldBe`
        [ ("first", 0, "rand2")
        , ("second", 1, "rand1")
        , ("third", 6, "rand3")
        ]

  describe "filterInactivePrompts" $ do
    it "filters inactive prompts" $ do
      -- given
      let prompt1 = Prompt
            { hQuestion = T.pack "q1"
            , hAnswer = T.pack "q1"
            , hActive = True
            , hReviews = []
            }
      let prompt2 = Prompt
            { hQuestion = T.pack "q2"
            , hAnswer = T.pack "q2"
            , hActive = False
            , hReviews = []
            }
      let prompt3 = Prompt
            { hQuestion = T.pack "q3"
            , hAnswer = T.pack "q3"
            , hActive = True
            , hReviews = []
            }
      let prompt4 = Prompt
            { hQuestion = T.pack "q4"
            , hAnswer = T.pack "q4"
            , hActive = False
            , hReviews = []
            }
      let prompt5 = Prompt
            { hQuestion = T.pack "q5"
            , hAnswer = T.pack "q5"
            , hActive = False
            , hReviews = []
            }
      let prompt6 = Prompt
            { hQuestion = T.pack "q6"
            , hAnswer = T.pack "q6"
            , hActive = True
            , hReviews = []
            }
      let promptsMap = HM.empty
            & HM.insert (T.pack "key1") prompt1
            & HM.insert (T.pack "key2") prompt2
            & HM.insert (T.pack "key3") prompt3
            & HM.insert (T.pack "key4") prompt4
            & HM.insert (T.pack "key5") prompt5
            & HM.insert (T.pack "key6") prompt6

      -- when
      let filteredMap = filterInactivePrompts promptsMap

      -- then
      HM.size filteredMap `shouldBe` 3
      HM.member (T.pack "key1") filteredMap `shouldBe` True
      HM.member (T.pack "key3") filteredMap `shouldBe` True
      HM.member (T.pack "key6") filteredMap `shouldBe` True
      HM.member (T.pack "key2") filteredMap `shouldNotBe` True
      HM.member (T.pack "key4") filteredMap `shouldNotBe` True
      HM.member (T.pack "key5") filteredMap `shouldNotBe` True
