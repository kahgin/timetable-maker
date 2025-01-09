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

dayMap :: Map.Map String DayOfWeek
dayMap = Map.fromList
    [ ("mon", Monday),     ("monday", Monday)
    , ("tue", Tuesday),    ("tuesday", Tuesday)
    , ("wed", Wednesday),  ("wednesday", Wednesday)
    , ("thu", Thursday),   ("thursday", Thursday)
    , ("fri", Friday),     ("friday", Friday)
    , ("sat", Saturday),   ("saturday", Saturday)
    , ("sun", Sunday),     ("sunday", Sunday)
    ]

-- Validate Day
validateDay :: IO DayOfWeek
validateDay = do
    printExample "Enter day (e.g. Monday, Tue)."
    putStr "Day: "
    hFlush stdout
    input <- getLine
    let normalizedInput = map toLower input
    case Map.lookup normalizedInput dayMap of
        Just day -> return day
        Nothing -> do
            printError "Invalid day.\n"
            validateDay 

-- Validate Time
validateTime :: IO TimeRange
validateTime = do
    printExample "Enter time in 12-hour format (e.g. 9am-11am, 9.30am-10.30pm)."
    putStr "Time: "
    hFlush stdout
    timeInput <- getLine
    case parseTimeRange timeInput of
        Just timeRange 
            | isValidTimeOrder timeRange -> return timeRange 
            | otherwise -> do 
                printError "End time must be after start time.\n"
                validateTime
        Nothing -> do
            printError "Invalid time format.\n"
            validateTime

-- Helper to check if end time is after start time
isValidTimeOrder :: TimeRange -> Bool
isValidTimeOrder (TimeRange start end) =
    timeToMinutes start < timeToMinutes end
  where
    timeToMinutes t = todHour t * 60 + todMin t

-- Function to parse a time string into TimeOfDay
parseTime :: String -> Maybe TimeOfDay
parseTime timeString =
    parseWithFormat "%l:%M%p" timeString `mplus` -- 9:00am
    parseWithFormat "%l.%M%p" timeString `mplus` -- 9.00am
    parseWithFormat "%l%p" timeString            -- 9am

-- Function to try parsing with a given format
parseWithFormat :: String -> String -> Maybe TimeOfDay
parseWithFormat format timeString =
    case parseTimeM True defaultTimeLocale format timeString of
        Just t -> Just t
        _ -> Nothing

-- Function to parse TimeRange string into TimeRange data structure
parseTimeRange :: String -> Maybe TimeRange
parseTimeRange time = do
    let (start, end) = break (== '-') time
    startTime <- parseTime start
    endTime <- parseTime (tail end)
    return $ TimeRange startTime endTime
