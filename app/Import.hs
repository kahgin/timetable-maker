module Import where

import Timetable
import Helper
import UI
import Data.Char (isSpace)
import Data.Time (DayOfWeek(..), TimeOfDay(..))
import Data.ByteString.Lazy as B
import Text.Html
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Text.HTML.TagSoup
import System.FilePath (takeFileName)
import Data.List (groupBy, sortOn)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Control.Monad (guard)

importTimetable :: [Timetable] -> IO [Timetable]
importTimetable timetables = do
    let htmlFilePath = "Sem5 timetable.html"
        timetableName = Prelude.takeWhile (/= '.') (takeFileName htmlFilePath)

    htmlContent <- B.readFile htmlFilePath
    case extractTimetableFromHtml (TLE.decodeUtf8 htmlContent) timetableName of
        Just timetable -> do
            putStrLn "Successfully parsed timetable"
            displaySubjectsWithLessons (subjects timetable)
            let newTimetableList = timetables ++ [timetable]
            return newTimetableList
        Nothing -> 
            printError "Error parsing HTML!" >> return timetables

extractTimetableFromHtml :: TL.Text -> String -> Maybe Timetable
extractTimetableFromHtml htmlContent timetableName = do
    let tags = parseTags $ TL.unpack htmlContent
        table = findTimetableTable tags
    case table of
        Just tableTags -> do
            let rows = partitionTags tableTags
                (headerRow:contentRows) = rows
                days = extractDays headerRow
                lessons = Prelude.concatMap (extractLessonsFromRow days) contentRows
            return $ Timetable {
                timetableName = timetableName,
                subjects = groupLessonsIntoSubjects lessons
            }
        Nothing -> Nothing

findTimetableTable :: [Tag String] -> Maybe [Tag String]
findTimetableTable tags = 
    let tables = sections (~== "<table>") tags
    in listToMaybe tables

partitionTags :: [Tag String] -> [[Tag String]]
partitionTags = sections (~== "<tr>")

-- Extract days from header row
extractDays :: [Tag String] -> [DayOfWeek]
extractDays tags = mapMaybe parseDayOfWeek (partitionTags tags)
    where
        parseDayOfWeek tags = case innerText tags of
            "Monday"    -> Just Monday
            "Tuesday"   -> Just Tuesday
            "Wednesday" -> Just Wednesday
            "Thursday"  -> Just Thursday
            "Friday"    -> Just Friday
            _           ->  Nothing

-- Extract lessons from a row
extractLessonsFromRow :: [DayOfWeek] -> [Tag String] -> [Lesson]
extractLessonsFromRow days row = 
    let cells = sections (~== "<td>") row
        timeSlot = extractTimeRange $ Prelude.head cells
        lessonCells = Prelude.zip days (Prelude.tail cells)
    in mapMaybe (extractLessonFromCell timeSlot) lessonCells

-- Extract a single lesson from a cell
extractLessonFromCell :: TimeRange -> (DayOfWeek, [Tag String]) -> Maybe Lesson
extractLessonFromCell timeRange (day, cell) = do
    let content = innerText cell
    guard $ not $ Prelude.null content
    return Lesson {
        day = day,
        time = timeRange,
        venue = extractVenue content,
        lecturer = extractLecturer content
    }

-- Helper functions for text extraction
extractTimeRange :: [Tag String] -> TimeRange
extractTimeRange tags = 
    let text = innerText tags
    in fromMaybe defaultTimeRange $ parseTimeRange text
    where
        defaultTimeRange = TimeRange (TimeOfDay 0 0 0) (TimeOfDay 0 0 0)

extractVenue :: String -> String
extractVenue content = 
    fromMaybe "" $ extractFieldValue "Venue :" content

extractLecturer :: String -> String
extractLecturer content = 
    fromMaybe "" $ extractFieldValue "Lecturer :" content

-- Group lessons into subjects
groupLessonsIntoSubjects :: [Lesson] -> [Subject]
groupLessonsIntoSubjects lessons =
    let grouped = Data.List.groupBy (\a b -> getSubjectName a == getSubjectName b) $
                 sortOn getSubjectName lessons
    in Prelude.map createSubject grouped
    where
        getSubjectName lesson = extractSubjectName $ venue lesson
        
        createSubject lessonGroup = Subject {
            subjectName = getSubjectName (Prelude.head lessonGroup),
            lessons = lessonGroup
        }

-- Helper function to extract subject name from room string
extractSubjectName :: String -> String
extractSubjectName roomStr = 
    let parts = words roomStr
    in if not (Prelude.null parts) then Prelude.head parts else "Unknown Subject"

-- Helper function to extract field value
extractFieldValue :: String -> String -> Maybe String
extractFieldValue prefix content = 
    let trim = Prelude.dropWhile isSpace . Prelude.reverse . Prelude.dropWhile isSpace . Prelude.reverse
    in case Prelude.break (Import.isPrefixOf prefix) (lines content) of
        (before, line:_) -> Just $ Prelude.drop (Prelude.length prefix) $ trim line
        _ -> Nothing

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && Import.isPrefixOf xs ys
