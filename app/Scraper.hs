module Scraper (
    importTimetableFromHTML,
) where

import Timetable
import UI
import Utility
import qualified Text.HTML.Scalpel as Scalpel
import Data.Char (toLower)
import Data.Time (DayOfWeek(..))
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Map as Map

-- Helper data type just for scraping
data LessonInfo = LessonInfo {
    lessonDay :: DayOfWeek,
    lessonTime :: String,
    lessonSubject :: String,
    lessonVenue :: String,
    lessonLecturer :: String
} deriving (Show)

-- Extract lesson info from an HTML block
scrapeLessonInfo :: Scalpel.Scraper String LessonInfo
scrapeLessonInfo = do
    -- Extract day
    dayStr <- Scalpel.text (Scalpel.tagSelector "strong")
    let day = fromMaybe Monday $ Map.lookup (map toLower $ takeWhile (/= ',') dayStr) dayMap

    -- Extract time, subject, venue, and lecturer
    info <- Scalpel.texts (Scalpel.tagSelector "span")
    let timeInfo = filter (isPrefixOf "Time : ") info
    let venueInfo = filter (isPrefixOf "Venue : ") info
    let lecturerInfo = filter (isPrefixOf "Lecturer : ") info
    
    -- Extract subject name
    subjectName <- Scalpel.text (Scalpel.tagSelector "strong")
    
    case (timeInfo, venueInfo, lecturerInfo) of
        ((t:_), (v:_), (l:_)) -> return $ LessonInfo {
            lessonDay = day,
            lessonTime = drop (length "Time : ") t,
            lessonSubject = dropWhile (== ' ') $ dropWhile (/= '-') $ drop (length "(L/P/T) ") subjectName,
            lessonVenue = drop (length "Venue : ") v,
            lessonLecturer = drop (length "Lecturer : ") l
        }
        _ -> fail "Could not parse lesson info"
    where
        isPrefixOf prefix str = prefix == take (length prefix) str

-- Convert LessonInfo to subject and lesson data using existing types
lessonInfoToTimetableData :: LessonInfo -> Maybe (String, TimeSlot, LessonDetails)
lessonInfoToTimetableData info = do
    timeRange <- parseTimeRange $ lessonTime info
    return (
        lessonSubject info,
        (lessonDay info, timeRange),
        (lessonVenue info, lessonLecturer info)
        )

-- Parse HTML file and create timetable using existing types
parseHTMLToTimetable :: FilePath -> IO TimetableDB
parseHTMLToTimetable filepath = do
    content <- readFile filepath
    
    -- Extract timetable name from file path (remove extension)
    let timetableName = takeWhile (/= '.') filepath
    
    -- Scrape all lesson blocks
    let lessons = Scalpel.scrapeStringLike content $ 
            Scalpel.chroots (Scalpel.tagSelector "div") scrapeLessonInfo
    case lessons of
        Nothing -> do
            printError "Failed to parse HTML content"
            return Map.empty
        Just lessonInfos -> do
            -- Convert lesson info to timetable format
            let timetableData = catMaybes $ map lessonInfoToTimetableData lessonInfos
            
            -- Group by subject to create SubjectMap
            let subjectMap = foldr insertLesson Map.empty timetableData
            
            -- Create TimetableDB with the single timetable
            return $ Map.singleton timetableName subjectMap
            where
                insertLesson (subject, timeSlot, details) acc =
                    Map.insertWith (Map.union) subject (Map.singleton timeSlot details) acc

-- Import timetable from HTML file
importTimetableFromHTML :: FilePath -> IO TimetableDB
importTimetableFromHTML filepath = do
    printMessage "Importing Timetable from HTML"
    newDb <- parseHTMLToTimetable filepath
    case Map.null newDb of
        True -> do
            printError "Failed to import timetable"
            return newDb
        False -> do
            printSuccess $ "Timetable '" <> takeWhile (/= '.') filepath <> "' imported successfully."
            return newDb
