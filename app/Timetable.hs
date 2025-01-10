{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

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
    } deriving (Show, Generic, FromJSON, ToJSON)

data Subject = Subject { 
    subjectName :: String,
    lessons :: [Lesson]
    } deriving (Show, Generic, FromJSON, ToJSON)

data Lesson = Lesson {
    day :: DayOfWeek,
    time :: TimeRange,
    room :: String,
    lecturer :: String
    } deriving (Show, Generic, FromJSON, ToJSON)

-- Create a new timetable
createTimetable :: IO Timetable
createTimetable = do
    clearScreen
    printHeader "Create a new timetable"
    putStr "Timetable name: "
    hFlush stdout
    timetableName <- getLine

    let newTimetable = Timetable { timetableName = timetableName, subjects = [] }
    manageTimetable newTimetable

-- Manage timetable
manageTimetable :: Timetable -> IO Timetable
manageTimetable timetable = do
    -- clearScreen
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
        "2" -> do
            createSubject
            manageTimetable timetable
        "3" -> do
            editSubject (subjects timetable)
            manageTimetable timetable
        "4" -> do
            deleteSubject (subjects timetable)
            manageTimetable timetable
        ""  -> return timetable
        _   -> printError "\nInvalid choice!\n\n"   >> manageTimetable timetable

-- Edit a timetable
editTimetable :: [Timetable] -> IO [Timetable]
editTimetable timetables = do
    -- clearScreen
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
                editTimetable timetables


-- Delete a timetable
deleteTimetable :: [Timetable] -> IO [Timetable]
deleteTimetable timetables = do
    -- clearScreen
    printHeader "Delete Timetable"
    mapM_ (\(i, t) -> putStrLn $ show i ++ ". " ++ timetableName t) (zip [1..] timetables)
    putStr "Select timetable to delete (or Enter to cancel): "
    hFlush stdout
    choice <- getLine
    case readMaybe choice of
        Just n | n >= 1 && n <= length timetables -> do
            let updatedTimetables = take (n - 1) timetables ++ drop (n + 1) timetables 
            saveTimetables (TimetableList updatedTimetables)
            return updatedTimetables

        "" -> return timetables
        _  -> printError "Invalid input" >> deleteTimetable timetables

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
manageSubject :: Subject -> IO ()
manageSubject subject = do
    clearScreen
    printHeader (subjectName subject)
    unless (null $ lessons subject) $ displayLessons (lessons subject)

    putStrLn "1. Edit subject name"
    putStrLn "2. Add a lesson"
    putStrLn "3. Edit a lesson"
    putStrLn "4. Delete a lesson"
    printExit

    putStr "Select option (1-4): "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> do
            putStr "Subject name: "
            hFlush stdout
            newName <- getLine
            updateSubject $ subject { subjectName = newName }
            
        "2" -> do
            newLesson <- createLesson
            updateSubject $ subject { lessons = lessons subject ++ [newLesson] }

        "3" -> do
            updatedLessons <- editLesson (lessons subject)
            updateSubject $ subject { lessons = updatedLessons }
            
        "4" -> do
            updatedLessons <- deleteLesson (lessons subject)
            updateSubject $ subject { lessons = updatedLessons }
            
        ""  -> return ()
        _   -> printError "\nInvalid choice!\n\n"   >> manageSubject subject

    where
        updateSubject updatedSubject = do
            let updatedTimetables = updateSubjectsList updatedSubject
            saveTimetables updatedTimetables
            manageSubject updatedSubject

updateSubjectsList :: Subject -> [Timetable]
updateSubjectsList updatedSubject = 
        map updateTimetable timetables
    where
        updateTimetable timetable = 
            timetable { subjects = map replaceSubject (subjects timetable) }
        replaceSubject subj
            | subjectName subj == subjectName updatedSubject = updatedSubject
            | otherwise = subj

editSubject :: [Subject] -> IO ()
editSubject subjects = do
    -- clearScreen
    printHeader "Edit subject"

    if null subjects then do
        putStrLn "No subjects available."
    else do
        putStrLn "Select a subject to edit:"
        mapM_ (\(i, subject) -> putStrLn $ show i ++ ". " ++ subjectName subject) (zip [1..] subjects)
        putStr "Select option: "
        hFlush stdout
        choice <- getLine
        case readMaybe choice of
            Just n | n >= 1 && n <= length subjects -> manageSubject (subjects !! (n - 1))
            _ -> printError "Invalid choice."


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
            -- putStrLn $ "Room: " ++ room lesson
            -- putStrLn $ "Lecturer: " ++ lecturer lesson
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

editLesson :: [Lesson] -> IO [Lesson]
editLesson lessons = do
    -- clearScreen
    printHeader "Edit lesson"

    if null lessons then do
        putStrLn "No lessons available."
        return lessons
    else do
        mapM_ (\(i, lesson) -> putStrLn $ show i ++ ". " ++ show (day lesson) ++ " " ++ show (time lesson)) (zip [1..] lessons)
        selectedLessonIndex <- getValidLessonIndex lessons
        let selectedLesson = lessons !! (selectedLessonIndex - 1) 

        printHeader $ show (day selectedLesson) ++ " " ++ show (time selectedLesson) 
        putStrLn "1. Day"
        putStrLn "2. Time"
        putStrLn "3. Room"
        putStrLn "4. Lecturer"
        putStrLn "5. Cancel" 

        putStr "Select option: "
        hFlush stdout
        choice <- getLine

        case readMaybe choice of
            Just 1 -> do 
                putStr "Enter new day: "
                hFlush stdout
                newDay <- validateDay
                return $ replaceAt (selectedLessonIndex - 1) (selectedLesson { day = newDay }) lessons
            Just 2 -> do 
                putStr "Enter new time: "
                hFlush stdout
                newTime <- validateTime
                return $ replaceAt (selectedLessonIndex - 1) (selectedLesson { time = newTime }) lessons
            Just 3 -> do 
                putStr "Enter new room: "
                hFlush stdout
                newRoom <- getLine
                return $ replaceAt (selectedLessonIndex - 1) (selectedLesson { room = newRoom }) lessons
            Just 4 -> do 
                putStr "Enter new lecturer: "
                hFlush stdout
                newLecturer <- getLine
                return $ replaceAt (selectedLessonIndex - 1) (selectedLesson { lecturer = newLecturer }) lessons
            Just 5 -> return lessons -- Cancel editing
            _ -> do
                printError "Invalid choice."
                editLesson lessons

getValidLessonIndex :: [Lesson] -> IO Int
getValidLessonIndex lessons = do
    putStr "Select a lesson to edit: "
    hFlush stdout
    choice <- getLine
    case readMaybe choice of
        Just n | n >= 1 && n <= length lessons -> return n
        _ -> do
            printError "Invalid choice."
            getValidLessonIndex lessons

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
type TimetableList = [Timetable]

saveTimetables :: TimetableList -> IO ()
saveTimetables ts = B.writeFile "timetables.json" (encode ts)

loadTimetables :: IO (Maybe TimetableList)
loadTimetables = do
    fileExists <- doesFileExist "timetables.json"
    if fileExists
        then do
            content <- B.readFile "timetables.json"
            return $ decode content
        else return Nothing
