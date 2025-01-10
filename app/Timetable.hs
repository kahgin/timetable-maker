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
import Data.List (findIndex)

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
createTimetable :: [Timetable] -> IO Timetable
createTimetable timetables = do
    -- clearScreen
    printHeader "Add a new timetable"
    timetableName <- validateName "Timetable" (map timetableName timetables) id

    let newTimetable = Timetable { timetableName = timetableName, subjects = [] }
    manageTimetable newTimetable timetables

-- Manage timetable
manageTimetable :: Timetable -> [Timetable] -> IO Timetable
manageTimetable timetable timetables = do
    -- clearScreen
    printHeader (timetableName timetable)
    putStrLn "1. Edit timetable name"
    putStrLn "2. Add a subject"
    putStrLn "3. Edit a subject"
    putStrLn "4. Delete a subject"
    printExit

    putStr "Select option (1-4): "
    hFlush stdout
    choice <- getLine

    case choice of
        "1" -> do
            printHeader "Edit timetable name"
            newName <- validateName "Timetable" (map timetableName timetables) id
            let updatedTimetable = timetable { timetableName = newName }
            saveTimetables $ updateTimetableInList updatedTimetable timetables
            manageTimetable (timetable { timetableName = newName }) timetables
        "2" -> do
            newSubject <- createSubject timetable timetables
            let updatedTimetable = timetable { subjects = subjects timetable ++ [newSubject] }
            saveTimetables $ updateTimetableInList updatedTimetable timetables
            manageTimetable (timetable { subjects = subjects timetable ++ [newSubject] }) timetables
        "3" -> do
            if null (subjects timetable) then do
                putStrLn "No subjects available."
                manageTimetable timetable timetables
            else do
                updatedSubjects <- editSubject timetable timetables
                let updatedTimetable = timetable { subjects = updatedSubjects }
                saveTimetables $ updateTimetableInList updatedTimetable timetables
                manageTimetable (timetable { subjects = updatedSubjects }) timetables
        "4" -> do
            updatedSubjects <- deleteSubject (subjects timetable)
            let updatedTimetable = timetable { subjects = updatedSubjects }
            saveTimetables $ updateTimetableInList updatedTimetable timetables
            manageTimetable (timetable { subjects = updatedSubjects }) timetables
        ""  -> do
            saveTimetables $ updateTimetableInList timetable timetables
            return timetable
        _   -> printError "\nInvalid choice!\n\n" >> manageTimetable timetable timetables

updateTimetableInList :: Timetable -> [Timetable] -> [Timetable]
updateTimetableInList updatedTimetable [] = [updatedTimetable]
updateTimetableInList updatedTimetable timetables = 
    case findIndex (\t -> timetableName t == timetableName updatedTimetable) timetables of
        Just idx -> take idx timetables ++ [updatedTimetable] ++ drop (idx + 1) timetables
        Nothing -> timetables ++ [updatedTimetable]
        
-- Edit a timetable
editTimetable :: [Timetable] -> IO [Timetable]
editTimetable timetables = do
    -- clearScreen
    printHeader "Edit timetable"

    if null timetables then do
        printError "\nNo timetables available.\n\n"
        return timetables
    else do
        mapM_ (\(i, timetable) -> putStrLn $ show i ++ ". " ++ timetableName timetable) (zip [1..] timetables)
        printExit
        
        putStr "Select a timetable to edit: "
        hFlush stdout
        choice <- getLine

        case choice of
            "" -> return timetables
            _  -> case readMaybe choice of
                    Just n | n >= 1 && n <= length timetables -> manageTimetable (timetables !! (n - 1)) timetables >>= \updatedTimetable ->
                        return $ replaceAt (n - 1) updatedTimetable timetables
                    _ -> printError "Invalid choice." >> editTimetable timetables

-- Delete a timetable
deleteTimetable :: [Timetable] -> IO [Timetable]
deleteTimetable timetables = do
    -- clearScreen
    printHeader "Delete Timetable"
    mapM_ (\(i, t) -> putStrLn $ show i ++ ". " ++ timetableName t) (zip [1..] timetables)
    printExit

    putStr "Select timetable to delete: "
    hFlush stdout
    choice <- getLine
    
    case choice of
        "" -> return timetables
        _  -> case readMaybe choice of
            Just n | n >= 1 && n <= length timetables -> do
                let updatedTimetables = take (n - 1) timetables ++ drop n timetables
                saveTimetables updatedTimetables
                return updatedTimetables

            _  -> printError "Invalid choice." >> deleteTimetable timetables

-- Create a new subject
createSubject :: Timetable -> [Timetable] -> IO Subject
createSubject timetable timetables = do
    -- clearScreen
    printHeader "Add a subject"
    subjectName <- validateName "Subject" (map subjectName (subjects timetable)) id

    let newSubject = Subject { subjectName = subjectName, lessons = [] }
    manageSubject newSubject timetable timetables

-- Manage subject   
manageSubject :: Subject -> Timetable -> [Timetable] -> IO Subject
manageSubject subject timetable timetables = do
    -- clearScreen
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
            newName <- validateName "Subject" (map subjectName (subjects timetable)) id
            let updatedSubject = subject { subjectName = newName }
            updateSubjectInTimetable updatedSubject timetable timetables
            
        "2" -> do
            newLesson <- createLesson
            let updatedSubject = subject { lessons = lessons subject ++ [newLesson] }
            updateSubjectInTimetable updatedSubject timetable timetables

        "3" -> do
            updatedLessons <- editLesson (lessons subject)
            let updatedSubject = subject { lessons = updatedLessons }
            updateSubjectInTimetable updatedSubject timetable timetables
            
        "4" -> do
            updatedLessons <- deleteLesson (lessons subject)
            let updatedSubject = subject { lessons = updatedLessons }
            updateSubjectInTimetable updatedSubject timetable timetables
            
        ""  -> return subject

        _   -> printError "\nInvalid choice!\n\n"   >> manageSubject subject timetable timetables

    where
        updateSubjectInTimetable :: Subject -> Timetable -> [Timetable] -> IO Subject
        updateSubjectInTimetable updatedSubject currentTimetable allTimetables = do
            let updatedSubjects = replaceSubjectInList updatedSubject (subjects currentTimetable)
            let updatedTimetable = currentTimetable { subjects = updatedSubjects }
            let updatedTimetables = map (\t -> if timetableName t == timetableName currentTimetable 
                                              then updatedTimetable 
                                              else t) allTimetables
            manageSubject updatedSubject updatedTimetable updatedTimetables

-- Replace a subject in a list of subjects
replaceSubjectInList :: Subject -> [Subject] -> [Subject]
replaceSubjectInList newSubject subjects = 
    zipWith (\oldSubject index -> if subjectName oldSubject == subjectName newSubject then newSubject else oldSubject) subjects [0..] 

editSubject :: Timetable -> [Timetable] -> IO [Subject]
editSubject timetable timetables = do
    -- clearScreen
    printHeader "Edit subject"
    let subjectList = subjects timetable
    mapM_ (\(i, subject) -> putStrLn $ show i ++ ". " ++ subjectName subject) (zip [1..] subjectList)
    printExit

    putStr "Select a subject to edit:"
    hFlush stdout
    choice <- getLine
    if null choice then
        return subjectList
    else
        case readMaybe choice of
            Just n | n >= 1 && n <= length subjectList -> do
                updatedSubject <- manageSubject (subjectList !! (n - 1)) timetable timetables
                let updatedSubjects = replaceSubjectInList updatedSubject subjectList
                return updatedSubjects
            _ -> do
                printError "Invalid choice."
                editSubject timetable timetables


deleteSubject :: [Subject] -> IO [Subject]
deleteSubject subjects = do
    -- clearScreen
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
            putStrLn $ "Room: " ++ room lesson
            putStrLn $ "Lecturer: " ++ lecturer lesson
            if index == length lessons then return () else putStrLn $ replicate 30 '-'
            
-- Create a new lesson
createLesson :: IO Lesson
createLesson = do
    -- clearScreen
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
        printError "\nNo lessons available.\n\n"
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
                newDay <- validateDay
                return $ replaceAt (selectedLessonIndex - 1) (selectedLesson { day = newDay }) lessons
            Just 2 -> do 
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
            Just 5 -> return lessons
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
    -- clearScreen
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
saveTimetables ts = do
    print ts
    B.writeFile "timetables.json" (encode ts)

loadTimetables :: IO (Maybe TimetableList)
loadTimetables = do
    fileExists <- doesFileExist "timetables.json"
    if fileExists
        then do
            content <- B.readFile "timetables.json"
            return $ decode content
        else return Nothing
