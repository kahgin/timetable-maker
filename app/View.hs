module View where

import Timetable
import UI
import Utility
import qualified Data.Map as Map
import Data.Time
import Control.Monad (forM_)
import Data.Char (toUpper)

columnWidth :: Int
columnWidth = 15

viewTimetableMenu :: TimetableDB -> IO ()
viewTimetableMenu db =
    if Map.null db
        then printError "No timetables available."
        else printHeader "View Timetable" >>
             mapM_ (\(name, _) -> putStrLn $ "- " <> name) (Map.toList db) >>
             getInput "Enter timetable name: " >>= \name ->
                case Map.lookup name db of
                    Nothing -> printError "Timetable not found."
                    Just subjects -> printTimetable name subjects

displayLessons :: LessonMap -> IO ()
displayLessons lessons =
    let formatLesson (timeSlot, (venue, lecturer)) =
            let (day, timeRange) = timeSlot
            in [ "Day: " <> show day
               , "Time: " <> show timeRange
               , "Venue: " <> venue
               , "Lecturer: " <> lecturer
               ]
        border = replicate 60 '='
    in putStrLn "" >>
       putStrLn border >>
       forM_ (Map.toList lessons) (\lesson -> 
           mapM_ putStrLn (formatLesson lesson) >>
           putStrLn (replicate 60 '-')) >>
       putStrLn border >>
       putStrLn ""

printTimetable :: String -> SubjectMap -> IO ()
printTimetable name subjects = 
    clearScreen >>
    printHeader name >>
    let timeSlots = generateTimeSlots (TimeOfDay 8 0 0) (TimeOfDay 19 0 0)
        days = [Monday .. Friday]
        width = (length timeSlots + 1) * columnWidth
        header = padRight columnWidth "Day" <> "|" <> 
                concatMap (\t -> padRight (columnWidth-1) t <> "|") timeSlots
        line = replicate width '='
    in putStrLn line >>
       putStrLn header >>
       putStrLn line >>
       forM_ days (\day -> 
           printDayRow timeSlots day (getLessonsForDay day subjects)) >>
       putStrLn line

getLessonsForDay :: DayOfWeek -> SubjectMap -> [(String, TimeSlot, LessonDetails)]
getLessonsForDay day subjects =
    [ (sName, timeSlot, details)
    | (sName, lessons) <- Map.toList subjects
    , (timeSlot@(d, _), details) <- Map.toList lessons
    , d == day
    ]

generateTimeSlots :: TimeOfDay -> TimeOfDay -> [String]
generateTimeSlots start end =
    let formattedEnd = formatTime defaultTimeLocale "%R" end
    in takeWhile (\time -> time < formattedEnd) $
       map (formatTime defaultTimeLocale "%R") $
       iterate addHours start
    where addHours time = time { todHour = (todHour time + 1) `mod` 24 }

formatTimeOfDay :: TimeOfDay -> String
formatTimeOfDay time =
    let formatted = formatTime defaultTimeLocale "%I:%M%p" time
        withLeadingZero = if head formatted == '1' || head formatted == '2'
                         then formatted
                         else '0' : tail formatted
    in map toUpper withLeadingZero

printDayRow :: [String] -> DayOfWeek -> [(String, TimeSlot, LessonDetails)] -> IO ()
printDayRow timeSlots day lessons = do
    let subjectLines = map (formatTimeSlotContent lessons) timeSlots
        maxLines = maximum $ map length subjectLines
        
    forM_ [0..(maxLines-1)] $ \lineNum -> do
        if lineNum == 0
            then putStr $ padRight columnWidth (show day) <> "|"
            else putStr $ padRight columnWidth "" <> "|"
        
        forM_ subjectLines $ \content ->
            putStr $ padRight (columnWidth-1) (if lineNum < length content 
                                             then content !! lineNum 
                                             else "") <> "|"
        putStrLn ""

formatTimeSlotContent :: [(String, TimeSlot, LessonDetails)] -> String -> [String]
formatTimeSlotContent lessons slot =
    case findLessonInTimeSlot slot lessons of
        Nothing -> [""]
        Just (subject, _, (venue, lecturer)) ->
            filter (not . null) [subject, venue, lecturer]

findLessonInTimeSlot :: String -> [(String, TimeSlot, LessonDetails)] -> Maybe (String, TimeSlot, LessonDetails)
findLessonInTimeSlot timeStr lessons =
    let containsTime (_, (_, TimeRange start end), _) =
            timeStr `elem` generateTimeSlots start end
    in case filter containsTime lessons of
           (lesson:_) -> Just lesson
           [] -> Nothing

padRight :: Int -> String -> String
padRight width str =
    let truncated = if length str > width then take width str else str
    in truncated <> replicate (width - length truncated) ' '
    