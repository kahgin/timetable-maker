module Helper where

import UI
import System.IO
import Data.Char
import Data.Time
import Control.Monad (mplus)

-- Data structure for TimeRange
data TimeRange = TimeRange { startTime :: TimeOfDay, endTime :: TimeOfDay } deriving Show

-- Validate Day
validateDay :: IO String
validateDay = do
    putStr "Day: "
    hFlush stdout
    dayInput <- getLine
    let normalizedDay = map toLower dayInput
    if isValidDay normalizedDay
        then return normalizedDay
        else do
            printError "Invalid day.\n"
            validateDay

-- Helper to check if the day is valid
isValidDay :: String -> Bool
isValidDay day = day `elem` dayNames

-- List of valid day names
dayNames :: [String]
dayNames = ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", 
            "mon", "tue", "wed", "thu", "fri", "sat", "sun"]

-- Validate Time
validateTime :: IO TimeRange
validateTime = do
    putStrLn "Enter time in 12-hour format (e.g. 9am-11am, 9.30am-10.30pm)"
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
