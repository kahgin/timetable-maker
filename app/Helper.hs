{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Helper where

import UI
import System.IO (hFlush, stdout)
import Data.Char (toLower)
import Data.Time
import Control.Monad (mplus, liftM2)
import qualified Data.Map as Map
import Data.Aeson
import GHC.Generics
import Text.Read (readMaybe)

data TimeRange = TimeRange { startTime :: TimeOfDay, endTime :: TimeOfDay } deriving (Generic, FromJSON, ToJSON)

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

-- Validate name to ensure it is not empty and does not already exist in the list
validateName :: String -> [a] -> (a -> String) -> IO String
validateName typeName list toString =
    getInput (typeName <> " name: ") >>= \newName ->
    if null newName then
        printError "Name cannot be empty." >>
        validateName typeName list toString
    else if any (\x -> toString x == newName) list then
        printError "Name already exists." >>
        validateName typeName list toString
    else
        return newName

-- Validate day
validateDay :: IO DayOfWeek
validateDay =
    printMessage "Enter day (e.g. Monday, Tue)." >>
    getInput "Day: " >>= \input ->
    let normalizedInput = map toLower input
    in case Map.lookup normalizedInput dayMap of
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

-- Check if end time is after start time
isValidTimeOrder :: TimeRange -> Bool
isValidTimeOrder (TimeRange start end) =
    timeToMinutes start < timeToMinutes end 
    where timeToMinutes t = todHour t * 60 + todMin t

-- Parse a time string into TimeOfDay
parseTime :: String -> Maybe TimeOfDay
parseTime timeString =
    parseWithFormat "%l:%M%p" timeString `mplus` -- 9:00am
    parseWithFormat "%l.%M%p" timeString `mplus` -- 9.00am
    parseWithFormat "%l%p" timeString            -- 9am

-- Parse time string into TimeOfDay with a given format
parseWithFormat :: String -> String -> Maybe TimeOfDay
parseWithFormat format timeString = parseTimeM True defaultTimeLocale format timeString

-- Parse TimeRange string into TimeRange data structure
parseTimeRange :: String -> Maybe TimeRange
parseTimeRange time =
    let (start, end) = break (== '-') time
    in liftM2 TimeRange (parseTime start) (parseTime (tail end))

-- Check if two time ranges overlap
isOverlap :: TimeRange -> TimeRange -> Bool
isOverlap (TimeRange start1 end1) (TimeRange start2 end2) =
        timeToMinutes start1 < timeToMinutes end2 && timeToMinutes start2 < timeToMinutes end1
    where
        timeToMinutes t = todHour t * 60 + todMin t

-- Replace an element in a list at a given index
replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 newVal (_:xs) = newVal : xs
replaceAt n newVal (x:xs) = x : replaceAt (n - 1) newVal xs

-- Select an item (either subject or timetable)
selectItem :: String -> [a] -> (a -> String) -> IO (Maybe Int)
selectItem header items name =
    printHeader header >>
    mapM_ (\(i, item) -> putStrLn $ show i <> ". " <> name item) (zip [1..] items) >>
    printExit >>
    getInput "Select option: " >>= \choice ->
    case choice of
        "" -> return Nothing
        _  -> case readMaybe choice of
            Just n | n >= 1 && n <= length items -> return (Just (n - 1))
            _ -> printError "Invalid choice." >> selectItem header items name
