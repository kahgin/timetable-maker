module View where

import Timetable
import UI
import Helper
import Data.Maybe
import qualified Data.Map as Map
import Data.Time (DayOfWeek(..), formatTime, defaultTimeLocale, TimeOfDay(..))
import Data.List (foldl')
import Control.Monad (forM_)
import Data.Char (toUpper)
import Debug.Trace

-- Group lessons by day
groupByDay :: Timetable -> Map.Map DayOfWeek [(String, String, String, String)] -- [Day, (Time, Subject, Venue, Lecturer)]
groupByDay timetable = 
    foldl' insertLesson Map.empty allLessons 
    where
    allLessons = 
        [ (day lesson, (show (time lesson), subjectName subject, venue lesson, lecturer lesson)) 
        | subject <- subjects timetable, lesson <- lessons subject]
    insertLesson acc (day, lessonInfo) = Map.alter (Just . maybe [lessonInfo] (lessonInfo :)) day acc

-- Output timetable
outputTimetable :: TimetableList -> IO ()
outputTimetable timetables = do
    idx <- selectItem "Print a Timetable" timetables timetableName
    case idx of
        Nothing -> return ()
        Just idx -> do
            printTimetable (timetables !! idx) >> return ()

-- Print timetable
printTimetable :: Timetable -> IO ()
printTimetable timetable = do
    let groupedLessons = groupByDay timetable
        days = [Monday, Tuesday, Wednesday, Thursday, Friday]
        startTime = TimeOfDay 8 0 0  -- 8:00 AM
        endTime = TimeOfDay 18 0 0   -- 6:00 PM
        times = generateTimeSlots startTime endTime
        width = (length times + 1) * columnWidth
        line = replicate width '='
        header = padRight columnWidth "Day" ++ "|" ++ 
                concatMap (\time -> padRight (columnWidth-1) time ++ "|") times

    putStrLn line
    putStrLn header
    putStrLn line
    forM_ days $ \day -> do
        let lessons = Map.findWithDefault [] day groupedLessons
        printDayRow times day lessons
    putStrLn line

-- Generate time slots dynamically
generateTimeSlots :: TimeOfDay -> TimeOfDay -> [String]
generateTimeSlots start end =
    let slots = takeWhile (<= end) $ iterate (addHour) start
    in map formatTimeOfDay slots
    where 
        addHour time = TimeOfDay h' m' s'
            where
                h' = (todHour time + 1) `mod` 24
                m' = todMin time
                s' = todSec time

getLessonTimeSlots :: TimeRange -> [String]
getLessonTimeSlots (TimeRange start end) =
    let duration = diffMinutes end start
        diffMinutes (TimeOfDay h1 m1 _) (TimeOfDay h2 m2 _) = (h1 - h2) * 60 + (m1 - m2)
        numSlots = duration `div` 60  -- Number of hour slots
        slots = take (fromIntegral numSlots) $ iterate (addHour) start
    in map formatTimeOfDay slots
    where 
        addHour time = TimeOfDay h' m' s'
            where
                h' = (todHour time + 1) `mod` 24
                m' = todMin time
                s' = todSec time

formatTimeOfDay :: TimeOfDay -> String
formatTimeOfDay time = 
    let formatted = formatTime defaultTimeLocale "%I:%M%p" time
        withLeadingZero = if head formatted == '1' || head formatted == '2' 
                         then formatted 
                         else '0' : tail formatted
    in map toUpper withLeadingZero

printDayRow :: [String] -> DayOfWeek -> [(String, String, String, String)] -> IO ()
printDayRow times day lessons = do
    let width = (length times + 1) * columnWidth
    
    -- Get wrapped subject names for each time slot
    let subjectLines = map (\time -> 
            case findLessonByTime time lessons of
                Just (_, subj, _, _) -> wrapText (columnWidth-1) subj
                Nothing -> [""]
            ) times
    
    -- Get maximum number of lines needed for any subject
    let maxLines = maximum $ map length subjectLines
    
    -- Print subject lines
    forM_ [0..(maxLines-1)] $ \lineNum -> do
        -- Print day name only for first line
        if lineNum == 0
            then putStr $ padRight columnWidth (show day) ++ "|"
            else putStr $ padRight columnWidth "" ++ "|"
        
        -- Print each subject's current line
        forM_ subjectLines $ \subj -> 
            if lineNum < length subj
                then putStr $ padRight (columnWidth-1) (subj !! lineNum) ++ "|"
                else putStr $ padRight (columnWidth-1) "" ++ "|"
        putStrLn ""
    
    -- Print venue row
    putStr $ padRight columnWidth "" ++ "|"
    forM_ times $ \time -> do
        let lesson = findLessonByTime time lessons
        case lesson of
            Just (_, _, venue, _) -> putStr $ padRight (columnWidth-1) (if null venue then "" else venue) ++ "|"
            Nothing -> putStr $ padRight (columnWidth-1) "" ++ "|"
    putStrLn ""
    
    -- Print lecturer row
    putStr $ padRight columnWidth "" ++ "|"
    forM_ times $ \time -> do
        let lesson = findLessonByTime time lessons
        case lesson of
            Just (_, _, _, lecturer) -> putStr $ padRight (columnWidth-1) (if null lecturer then "" else lecturer) ++ "|"
            Nothing -> putStr $ padRight (columnWidth-1) "" ++ "|"
    putStrLn ""
    putStrLn $ replicate width '-'

columnWidth :: Int
columnWidth = 15

wrapText :: Int -> String -> [String]
wrapText width text = 
    let words' = words text
    in go words' ""
    where
        go [] acc = if null acc then [] else [acc]
        go (w:ws) acc
            | null acc = go ws w
            | length acc + 1 + length w <= width = go ws (acc ++ " " ++ w)
            | otherwise = acc : go (w:ws) ""

padRight :: Int -> String -> String
padRight width string = 
    let truncated = if length string > width 
                   then take width string 
                   else string
    in truncated ++ replicate (width - length truncated) ' '

findLessonByTime :: String -> [(String, String, String, String)] -> Maybe (String, String, String, String)
findLessonByTime timeStr lessons =
    case filter matchesTime lessons of
        (lesson:_) -> Just lesson
        [] -> Nothing
  where
    matchesTime (timeRangeStr, _, _, _) =
        case parseTimeRange timeRangeStr of
            Just timeRange -> timeStr `elem` getLessonTimeSlots timeRange
            Nothing -> False


