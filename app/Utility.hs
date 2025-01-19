{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Utility where

import UI
import System.IO (hFlush, stdout)
import Data.Char (toLower)
import Data.Time
import Control.Monad (mplus)
import qualified Data.Map as Map
import Data.Aeson
import GHC.Generics

data TimeRange = TimeRange { startTime :: TimeOfDay, endTime :: TimeOfDay } deriving (Ord, Eq, Generic, FromJSON, ToJSON)

instance Show TimeRange where
    show (TimeRange start end) = formatTime defaultTimeLocale "%I:%M%p" start <> " - " <> formatTime defaultTimeLocale "%I:%M%p" end

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

-- Get user input with prompt message
getInput :: String -> IO String
getInput prompt = putStr "\n" >> putStr prompt >> hFlush stdout >> getLine

-- Confirm an action with a message
confirmAction :: String -> IO Bool
confirmAction message = do
    printMessage $ message <> " (y/n): "
    response <- getLine
    return $ response `elem` ["y", "Y", "yes", "Yes"]

-- Validate name to ensure it is not empty and does not already exist in the list
validateName :: String -> Map.Map String a -> IO String
validateName typeName existingMap = do
    newName <- getInput (typeName <> " name: ")
    if null newName then do
        printError "Name cannot be empty."
        validateName typeName existingMap
    else if any (\existingName -> map toLower existingName == map toLower newName) (Map.keys existingMap) then do
        printError "Name already exists."
        validateName typeName existingMap
    else
        return newName

-- Validate day
validateDay :: IO DayOfWeek
validateDay =
    printMessage "Enter day (e.g. Monday, Tue)." >>
    getInput "Day: " >>= \input ->
    case Map.lookup (map toLower input) dayMap of
        Just day -> return day
        Nothing -> printError "Invalid day." >> validateDay 

-- Validate a time range
validateTime :: IO TimeRange
validateTime =
    printMessage "Enter time in 12-hour format (e.g. 9am-11am, 9.30am-10.30pm)." >>
    getInput "Time: " >>= \timeInput ->
    case parseTimeRange timeInput of
        Just timeRange
            | isValidTimeOrder timeRange -> return timeRange 
            | otherwise -> printError "End time must be after start time." >> validateTime
        Nothing ->
            printError "Invalid time format." >> validateTime

parseTimeRange :: String -> Maybe TimeRange
parseTimeRange time =
    let (start, end) = break (== '-') time
    in TimeRange <$> parseTime start <*> parseTime (tail end)

-- Check if end time is after start time
isValidTimeOrder :: TimeRange -> Bool
isValidTimeOrder (TimeRange start end) = timeToMinutes start < timeToMinutes end 

-- Parse a time string into TimeOfDay with a given format
parseTime :: String -> Maybe TimeOfDay
parseTime timeString =
    parseTimeM True defaultTimeLocale "%l:%M%p" timeString `mplus` -- 9:00am
    parseTimeM True defaultTimeLocale "%l.%M%p" timeString `mplus` -- 9.00am
    parseTimeM True defaultTimeLocale "%l%p" timeString            -- 9am

-- Check if two time ranges overlap
isOverlap :: TimeRange -> TimeRange -> Bool
isOverlap (TimeRange start1 end1) (TimeRange start2 end2) = timeToMinutes start1 < timeToMinutes end2 && timeToMinutes start2 < timeToMinutes end1
        
timeToMinutes :: TimeOfDay -> Int
timeToMinutes t = todHour t * 60 + todMin t

-- Select an item (either subject or timetable)
selectItem :: String -> [String]-> IO (Maybe Int)
selectItem header items =
    printHeader header >>
    mapM_ (\(i, item) -> putStrLn $ show (i::Int) <> ". " <> item) (zip [1..] items) >>
    printExit >>
    getInput "Select option: " >>= \choice ->
    case choice of
        "" -> return Nothing
        n  -> case reads n of
            [(num, "")] | num > 0 && num <= length items -> return $ Just (num - 1)
            _ -> printError "Invalid choice." >> selectItem header items

-- Pad a string to the right
padRight :: Int -> String -> String
padRight width str =
    let truncated = if length str > width then take width str else str
    in truncated <> replicate (width - length truncated) ' '

-- Split a list into groups of n elements
groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)
