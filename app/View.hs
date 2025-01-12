module View where

import Timetable
import UI
import Helper
import Data.Maybe
import qualified Data.Map as Map
import Data.Time (DayOfWeek(..))
import Data.List (foldl')
import Control.Monad (forM_)

-- Group lessons by day
groupByDay :: Timetable -> Map.Map DayOfWeek [(String, String, String)]
groupByDay timetable = 
    foldl' insertLesson Map.empty allLessons 
    where
    allLessons = 
        [ (day lesson, (subjectName subject, venue lesson, lecturer lesson)) 
        | subject <- subjects timetable, lesson <- lessons subject]
    insertLesson acc (day, subjectLesson) = Map.alter (Just . maybe [subjectLesson] (subjectLesson :)) day acc

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
        times = [ "08:00AM", "09:00AM", "10:00AM", "11:00AM", "12:00PM", "01:00PM", 
                  "02:00PM", "03:00PM", "04:00PM", "05:00PM", "06:00PM" ]
        width = (length times + 1)*10
        line = replicate width '='
        header = "Day      |" ++ concatMap (\time -> replicate 1 ' ' ++ time ++ replicate 1 ' ' ++ "|") times

    -- Print header row with time slots
    putStrLn line
    putStrLn header
    putStrLn line
    mapM_ (printDayRow times) (Map.toList groupedLessons)
    putStrLn line

printDayRow :: [String] -> (DayOfWeek, [(String, String, String)]) -> IO ()
printDayRow times (day, lessons) = do

    let dayColumn = padRight columnWidth (show day)  -- Pad the "Day" column
        lessonRows = createLessonRows lessons  -- Format the lessons for this day
    mapM_ putStrLn (dayColumn : lessonRows)  -- Print the day row and its lessons
    where
        -- Format the lessons for this day
        createLessonRows :: [(String, String, String)] -> [String]
        createLessonRows lessons = concatMap formatLessonRow lessons

        -- Define the function to print each day's row with wrapped subject text
        formatLessonRow :: (String, String, String) -> [String]
        formatLessonRow (subject, venue, lecturer) =
            let wrappedSubject = wrapText columnWidth subject  -- Wrap the subject to fit the column width
            in  wrappedSubject ++
                [ padRight columnWidth ""
                , padRight columnWidth ""
                , padRight columnWidth venue    -- Second row: Venue
                , padRight columnWidth lecturer -- Third row: Lecturer
                ]

columnWidth :: Int
columnWidth = 10

wrapText :: Int -> String -> [String]
wrapText width text = go text
    where
        go [] = []
        go t  = let (line, rest) = splitAt width t
                in line : go (dropWhile (== ' ') rest)  -- Remove leading spaces from the next part


padRight :: Int -> String -> String
padRight width string = string ++ replicate (width - length string) ' '

