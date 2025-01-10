{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Timetable where

import UI
import Helper
import System.IO
import Data.Time
import Data.Aeson
import GHC.Generics
import Control.Monad
import Text.Read (readMaybe)
import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy as B

data Timetable = Timetable {
    timetableName :: String,
    subjects :: [Subject]
    } deriving (Show)

data Subject = Subject { 
    subjectName :: String,
    lessons :: [Lesson]
    } deriving (Show)

data Lesson = Lesson {
    day :: DayOfWeek,
    time :: TimeRange,
    room :: String,
    lecturer :: String
    } deriving (Show)

-- Create a new timetable
createTimetable :: IO Timetable
createTimetable = do
    clearScreen
    printHeader "Create a new timetable"
    putStr "Timetable name: "
    hFlush stdout
    timetableName <- getLine

    let newTimetable = Timetable { timetableName = timetableName, subjects = [] }
    newTimetable
    manageTimetable newTimetable

-- Manage timetable
manageTimetable :: Timetable -> IO Timetable
manageTimetable timetable = do
    clearScreen
    printHeader (timetableName timetable)
    putStrLn "1. Edit Timetable Name"
    putStrLn "2. Add a subject"
    putStrLn "3. Edit a subject"
    putStrLn "4. Delete a subject"
    printExit

    putStr "Select option (1-3): "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> do
            putStr "Timetable name: "
            hFlush stdout
            newName <- getLine
            manageTimetable (timetable { timetableName = newName })
        "2" -> createSubject                        >> manageTimetable timetable
        "3" -> editSubject                          >> manageTimetable timetable
        "4" -> deleteSubject                        >> manageTimetable timetable
        ""  -> return timetable
        _   -> printError "\nInvalid choice!\n\n"   >> manageTimetable timetable

-- Edit a timetable
editTimetable :: [Timetable] -> IO [Timetable]
editTimetable timetables = do
    clearScreen
    printHeader "Edit timetable"

    if null timetables then do
        putStrLn "No timetables available."
        return timetables
    else do
        putStrLn "Select a timetable to edit:"
        mapM_ (\(i, timetable) -> putStrLn $ show i ++ ". " ++ timetableName timetable) (zip [1..] timetables)
        putStr "Select option: "
        hFlush stdout
        choice <- getLine
        case readMaybe choice of
            Just n | n >= 1 && n <= length timetables -> manageTimetable (timetables !! (n - 1)) >>= \updatedTimetable ->
                return $ replaceAt (n - 1) updatedTimetable timetables
            _ -> do
                printError "Invalid choice."
                return TimetableStore

deleteSubject :: [Subject] -> IO [Subject]
deleteSubject subjects = do
    clearScreen
    printHeader "Delete Subject"
    mapM_ (\(i, s) -> putStrLn $ show i ++ ". " ++ subjectName s) (zip [1..] subjects)
    putStr "Select subject to delete (or Enter to cancel): "
    hFlush stdout
    choice <- getLine
    case readMaybe choice of
        Just n | n >= 1 && n <= length subjects -> do
            let updatedSubjects = take (n - 1) subjects ++ drop n subjects
            return updatedSubjects
        _ -> return subjects

-- Replace the timetable at index n with a new timetable
replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 newVal (_:xs) = newVal : xs
replaceAt n newVal (x:xs) = x : replaceAt (n - 1) newVal xs

-- Delete a timetable
deleteTimetable :: [Timetable] -> IO [Timetable]
deleteTimetable timetables = do
    clearScreen
    printHeader "Delete Timetable"
    mapM_ (\(i, t) -> putStrLn $ show i ++ ". " ++ timetableName t) (zip [1..] timetables)
    putStr "Select timetable to delete (or Enter to cancel): "
    hFlush stdout
    choice <- getLine
    case readMaybe choice of
        Just n | n >= 1 && n <= length timetables -> do
            let updatedTimetables = TimetableStore { timetables = take (n - 1) timetables ++ drop n timetables }
            saveTimetables updatedTimetables
            return updatedTimetables
        _ -> return timetables

-- Create a new subject
createSubject :: IO Subject
createSubject = do
    clearScreen
    printHeader "Add a subject"
    putStr "Subject name: "
    hFlush stdout
    subjectName <- getLine

    let newSubject = Subject { subjectName = subjectName, lessons = [] }
    manageSubject newSubject

-- Manage subject   
manageSubject :: Subject -> IO Subject
manageSubject subject = do
    clearScreen
    printHeader (subjectName subject)
    unless (null $ lessons subject) $ displayLessons (lessons subject)

    putStrLn "1. Edit subject name"
    putStrLn "2. Add a lesson"
    putStrLn "3. Edit a lesson"
    putStrLn "4. Delete a lesson"
    printExit

    putStr "Select option (1-3): "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> do
            putStr "Subject name: "
            hFlush stdout
            newName <- getLine
            manageSubject (subject { subjectName = newName })
        "2" -> do
            newLesson <- createLesson
            let updatedSubject = subject { lessons = lessons subject ++ [newLesson] }
            manageSubject updatedSubject
        "3" -> editLesson                           >> manageSubject subject
        "4" -> deleteLesson                         >> manageSubject subject
        ""  -> return subject
        _   -> printError "\nInvalid choice!\n\n"   >> manageSubject subject

editSubject :: [Subject] -> IO Subject
editSubject subjects = do
    clearScreen
    printHeader "Edit subject"

    if null subjects then do
        putStrLn "No subjects available."
        return subjects
    else do
        putStrLn "Select a subject to edit:"
        mapM_ (\(i, subject) -> putStrLn $ show i ++ ". " ++ subjectName subject) (zip [1..] subjects)
        putStr "Select option: "
        hFlush stdout
        choice <- getLine
        case readMaybe choice of
            Just n | n >= 1 && n <= length subjects -> manageSubject (subjects !! (n - 1)) >>= \updatedSubject ->
                return $ replaceAt (n - 1) updatedSubject subjects
            _ -> do
                putStrLn "Invalid choice."
                return subjects

-- Display lessons of a subject
displayLessons :: [Lesson] -> IO ()
displayLessons lessons = do
    let border = replicate 30 '='
    putStrLn border
    mapM_ printLessonWithBorder (zip lessons [1..])
    putStrLn border
    where
        printLessonWithBorder (lesson, index) = do
            putStrLn $ "Day: " ++ show (day lesson)
            putStrLn $ "Time: " ++ show (time lesson)
            putStrLn $ "Room: " ++ room lesson
            putStrLn $ "Lecturer: " ++ lecturer lesson
            if index == length lessons then return () else putStrLn $ replicate 30 '-'

-- Create a new lesson
createLesson :: IO Lesson
createLesson = do
    clearScreen
    printHeader "Add a lesson"

    day <- validateDay

    time <- validateTime

    putStr "Room (optional): "
    hFlush stdout
    room <- getLine

    putStr "Lecturer (optional): "
    hFlush stdout
    lecturer <- getLine

    return Lesson { day = day, time = time, room = room, lecturer = lecturer }

editLesson :: [Lesson] -> IO Lesson
editLesson lessons = do
    clearScreen
    printHeader "Edit lesson"

    if null lessons then do
        putStrLn "No lessons available."
        return lessons
    else do
        putStrLn "Select a lesson to edit:"
        mapM_ (\(i, lesson) -> putStrLn $ show i ++ ". " ++ show (day lesson) ++ " " ++ show (time lesson)) (zip [1..] lessons)
        putStr "Select option: "
        hFlush stdout
        choice <- getLine
        case readMaybe choice of
            Just n | n >= 1 && n <= length lessons -> 
                manageLesson (lessons !! (n - 1)) >>= \updatedLesson ->
                return $ replaceAt (n - 1) updatedLesson lessons
            _ -> do
                putStrLn "Invalid choice."
                return lessons

deleteLesson :: [Lesson] -> IO [Lesson]
deleteLesson lessons = do
    clearScreen
    printHeader "Delete Lesson"
    mapM_ (\(i, l) -> putStrLn $ show i ++ ". " ++ show (day l) ++ " " ++ show (time l)) (zip [1..] lessons)
    putStr "Select lesson to delete (or Enter to cancel): "
    hFlush stdout
    choice <- getLine
    case readMaybe choice of
        Just n | n >= 1 && n <= length lessons -> do
            let updatedLessons = take (n - 1) lessons ++ drop n lessons
            return updatedLessons
        _ -> return lessons

-- Save timetable to file
data TimetableStore = TimetableStore {
    timetables :: [Timetable]
} deriving (Show, Generic, FromJSON, ToJSON)

instance FromJSON Timetable
instance ToJSON Timetable

saveTimetables :: TimetableStore -> IO ()
saveTimetables ts = do
    B.writeFile "timetables.json" (encode ts)

loadTimetables :: IO (Maybe TimetableStore)
loadTimetables = do
    fileExists <- doesFileExist "timetables.json"
    if fileExists
        then do
            content <- B.readFile "timetables.json"
            return $ decode content
        else return Nothing
