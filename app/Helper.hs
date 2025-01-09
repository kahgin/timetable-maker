module Helper where

import UI
import System.IO
import Data.Char
import Data.Time
import Control.Monad (mplus)
import Data.Map (Map)
import qualified Data.Map as Map

-- Data structure for TimeRange
data TimeRange = TimeRange { startTime :: TimeOfDay, endTime :: TimeOfDay }

instance Show TimeRange where
    show (TimeRange start end) = formatTime defaultTimeLocale "%I:%M%p" start ++ " - " ++ formatTime defaultTimeLocale "%I:%M%p" end

-- Day names
dayNames :: Map String String
dayNames = Map.fromList
  [ ("mon", "Monday")
  , ("monday", "Monday")
  , ("tue", "Tuesday")
  , ("tuesday", "Tuesday")
  , ("wed", "Wednesday")
  , ("wednesday", "Wednesday")
  , ("thu", "Thursday")
  , ("thursday", "Thursday")
  , ("fri", "Friday")
  , ("friday", "Friday")
  , ("sat", "Saturday")
  , ("saturday", "Saturday")
  , ("sun", "Sunday")
  , ("sunday", "Sunday")
  ]

-- Validate Day
validateDay :: IO String
validateDay = do
    printExample "Enter day (e.g. Monday, Tue)."
    putStr "Day: "
    hFlush stdout
    input <- getLine
    let normalizedInput = map toLower input
    case Map.lookup normalizedInput dayNames of
        Just day -> return day
        Nothing -> do
            printError "Invalid day."
            validateDay 

-- Validate Time
validateTime :: IO TimeRange
validateTime = do
    printExample "Enter time in 12-hour format (e.g. 9am-11am, 9.30am-10.30pm)."
    putStr "Time: "
    hFlush stdout
    timeInput <- getLine
    case parseTimeRange timeInput of
        Just timeRange -> return timeRange
        Nothing -> do
            printError "Invalid time format.\n"
            validateTime

-- Function to parse a time string into TimeOfDay, supporting multiple formats
parseTime :: String -> Maybe TimeOfDay
parseTime timeString =
    parseWithFormat "%l:%M%p" timeString `mplus`
    parseWithFormat "%l.%M%p" timeString `mplus`
    parseWithFormat "%l%p" timeString

-- Helper function to try parsing with a given format
parseWithFormat :: String -> String -> Maybe TimeOfDay
parseWithFormat format timeString =
    case parseTimeM True defaultTimeLocale format timeString of
        Just t -> Just t  -- Extract the parsed TimeOfDay directly
        _ -> Nothing

-- Function to parse TimeRange string into TimeRange data structure
parseTimeRange :: String -> Maybe TimeRange
parseTimeRange time = do
    let (start, end) = break (== '-') time
    startTime <- parseTime start
    endTime <- parseTime (tail end)
    return $ TimeRange startTime endTime
