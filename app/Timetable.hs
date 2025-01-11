{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Timetable where

import UI
import Helper
import Data.Time (DayOfWeek(..))
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import GHC.Generics (Generic)
import Control.Monad (unless)
import Text.Read (readMaybe)
import Data.Maybe (listToMaybe)
import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy as B
import Data.List (findIndex)

type TimetableList = [Timetable]

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
    venue :: String,
    lecturer :: String
    } deriving (Show, Generic, FromJSON, ToJSON)

-- Create a new timetable
createTimetable :: [Timetable] -> IO Timetable
createTimetable timetables = do
    printHeader "Create a new Timetable"
    timetableName <- validateName "Timetable" (map timetableName timetables) id
    let newTimetable = Timetable { timetableName = timetableName, subjects = [] }
    manageTimetable newTimetable timetables

-- Manage timetable
manageTimetable :: Timetable -> [Timetable] -> IO Timetable
manageTimetable timetable timetables = do
    printHeader (timetableName timetable)
    unless (null $ subjects timetable) $ displaySubjectsWithLessons (subjects timetable)
    putStrLn "1. Edit timetable name"
    putStrLn "2. Add a subject"
    putStrLn "3. Edit a subject"
    putStrLn "4. Delete a subject"
    printExit
    choice <- getInput "Select option (1-4): "

    case choice of
        "1" -> do
            printHeader "Rename Timetable"
            newName <- validateName "Timetable" (map timetableName timetables) id
            let updatedTimetable = timetable { timetableName = newName }
            manageTimetable (timetable { timetableName = newName }) timetables
        "2" -> do
            newSubject <- createSubject timetable timetables
            let updatedTimetable = timetable { subjects = subjects timetable ++ [newSubject] }
            manageTimetable (timetable { subjects = subjects timetable ++ [newSubject] }) timetables
        "3" -> do
            if null (subjects timetable) then do
                printError "No subjects available."
                manageTimetable timetable timetables
            else do
                updatedSubjects <- editSubject timetable timetables
                let updatedTimetable = timetable { subjects = updatedSubjects }
                manageTimetable (timetable { subjects = updatedSubjects }) timetables
        "4" -> do
            if null (subjects timetable) then do
                printError "No subjects available."
                manageTimetable timetable timetables
            else do
                updatedSubjects <- deleteSubject (subjects timetable)
                let updatedTimetable = timetable { subjects = updatedSubjects }
                manageTimetable (timetable { subjects = updatedSubjects }) timetables
        ""  -> saveTimetables (updateTimetableInList timetable timetables) >> return timetable
        _   -> printError "Invalid choice!" >> manageTimetable timetable timetables

-- Update the list of timetables with the updated timetable
updateTimetableInList :: Timetable -> [Timetable] -> [Timetable]
updateTimetableInList updatedTimetable [] = [updatedTimetable]
updateTimetableInList updatedTimetable timetables = 
    case findIndex (\t -> timetableName t == timetableName updatedTimetable) timetables of
        Just idx -> take idx timetables ++ [updatedTimetable] ++ drop (idx + 1) timetables
        Nothing -> timetables ++ [updatedTimetable]
        
-- Edit a timetable
editTimetable :: [Timetable] -> IO [Timetable]
editTimetable timetables = do
    idx <- selectItem "Edit a Timetable" timetables timetableName
    case idx of
        Nothing -> return timetables
        Just idx -> do
            updatedTimetable <- manageTimetable (timetables !! idx) timetables
            return $ replaceAt idx updatedTimetable timetables

-- Delete a timetable
deleteTimetable :: [Timetable] -> IO [Timetable]
deleteTimetable timetables = do
    idx <- selectItem "Delete a Timetable" timetables timetableName
    case idx of
        Nothing -> return timetables
        Just idx -> do
            let updatedTimetables = take idx timetables ++ drop (idx + 1) timetables
            return updatedTimetables

displaySubjectsWithLessons :: [Subject] -> IO ()
displaySubjectsWithLessons subjects = do
    let border = replicate 30 '='
    mapM_ printSubjectWithLessons (zip subjects [1..])
    where
        printSubjectWithLessons (subject, index) = do
            if index == 1 then putStrLn "" else return ()
            putStrLn $ subjectName subject
            if null (lessons subject) then printError "\nNo lessons available.\n" else displayLessons (lessons subject)

-- Create a new subject
createSubject :: Timetable -> [Timetable] -> IO Subject
createSubject timetable timetables = do
    printHeader "Add a Subject"
    subjectName <- validateName "Subject" (map subjectName (subjects timetable)) id

    let newSubject = Subject { subjectName = subjectName, lessons = [] }
    manageSubject newSubject timetable timetables

-- Manage subject   
manageSubject :: Subject -> Timetable -> [Timetable] -> IO Subject
manageSubject subject timetable timetables = do
    printHeader (subjectName subject)
    unless (null $ lessons subject) $ displayLessons (lessons subject)
    putStrLn "1. Edit subject name"
    putStrLn "2. Add a lesson"
    putStrLn "3. Edit a lesson"
    putStrLn "4. Delete a lesson"
    printExit
    choice <- getInput "Select option (1-4): "

    case choice of
        "1" -> do
            newName <- validateName "Subject" (map subjectName (subjects timetable)) id
            let updatedSubject = subject { subjectName = newName }
            updateSubjectInTimetable updatedSubject timetable timetables
            
        "2" -> do
            newLesson <- createLesson timetable
            case newLesson of
                Just newLesson -> do
                    let updatedSubject = subject { lessons = lessons subject ++ [newLesson] }
                    updateSubjectInTimetable updatedSubject timetable timetables
                Nothing -> manageSubject subject timetable timetables
        "3" -> do
            if null $ lessons subject then do
                printError "No lessons available."
                manageSubject subject timetable timetables
            else do
                updatedLessons <- editLesson (lessons subject)
                let updatedSubject = subject { lessons = updatedLessons }
                updateSubjectInTimetable updatedSubject timetable timetables
        "4" -> do
            if (null $ lessons subject) then do
                printError "No lessons available."
                manageSubject subject timetable timetables
            else do
                updatedLessons <- deleteLesson (lessons subject)
                let updatedSubject = subject { lessons = updatedLessons }
                updateSubjectInTimetable updatedSubject timetable timetables
            
        ""  -> return subject

        _   -> printError "Invalid choice!" >> manageSubject subject timetable timetables

    where
        -- Update the subject in the timetable
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

-- Edit a subject
editSubject :: Timetable -> [Timetable] -> IO [Subject]
editSubject timetable timetables = do
    let subjectList = subjects timetable
    selectedIdx <- selectItem "Edit subject" subjectList subjectName
    case selectedIdx of
        Nothing -> return subjectList
        Just idx -> do
            updatedSubject <- manageSubject (subjectList !! idx) timetable timetables
            let updatedSubjects = replaceAt idx updatedSubject subjectList
            return updatedSubjects

-- Delete a subject
deleteSubject :: [Subject] -> IO [Subject]
deleteSubject subjects = do
    selectedIdx <- selectItem "Delete subject" subjects subjectName
    case selectedIdx of
        Nothing -> return subjects
        Just idx -> return (take idx subjects ++ drop (idx + 1) subjects)

-- Display lessons of a subject
displayLessons :: [Lesson] -> IO ()
displayLessons lessons = do
    let border = replicate 30 '='
    putStrLn ""
    putStrLn border
    mapM_ printLessonWithBorder (zip lessons [1..])
    putStrLn border
    putStrLn ""
    where
        printLessonWithBorder (lesson, index) = do
            putStrLn $ "Day: " ++ show (day lesson)
            putStrLn $ "Time: " ++ show (time lesson)
            putStrLn $ "Venue: " ++ venue lesson
            putStrLn $ "Lecturer: " ++ lecturer lesson
            if index == length lessons then return () else putStrLn $ replicate 30 '-'
            
-- Create a new lesson
createLesson :: Timetable -> IO (Maybe Lesson)
createLesson timetable = do
    printHeader "Add a Lesson"
    day <- validateDay
    time <- validateTime
    let newLesson = Lesson { day = day, time = time, venue = "", lecturer = "" }
    overlappingLesson <- handleOverlappingLesson newLesson timetable
    case overlappingLesson of
        Nothing -> return Nothing
        Just lesson -> do
            venue <- getInput "Venue (optional): "
            lecturer <- getInput "Lecturer (optional): "
            return $ Just lesson { venue = venue, lecturer = lecturer }

-- Check for an overlapping lesson and return the subject name and the lesson
checkOverlappingLesson :: Lesson -> Timetable -> Maybe (Subject, Lesson)
checkOverlappingLesson newLesson timetable =
    listToMaybe [(subject, lesson) | subject <- subjects timetable, lesson <- lessons subject, day lesson == day newLesson, isOverlap (time lesson) (time newLesson)]

-- Handle overlapping lesson
handleOverlappingLesson :: Lesson -> Timetable -> IO (Maybe Lesson)
handleOverlappingLesson newLesson timetable = do
    case checkOverlappingLesson newLesson timetable of
        Nothing -> return $ Just newLesson
        Just (subject, overlappingLesson) -> do
            printError $ "Overlapping lesson found in " ++ subjectName subject ++ " " ++ show (day overlappingLesson) ++ " " ++ show (time overlappingLesson)
            putStrLn "1. Re-enter day & time"
            putStrLn "2. Stop adding current lesson"
            choice <- getInput "Select option: "
            case choice of
                "1" -> do
                    day <- validateDay
                    time <- validateTime
                    let newLesson = Lesson { day = day, time = time, venue = "", lecturer = "" } 
                    handleOverlappingLesson newLesson timetable
                "2" -> return Nothing
                _ -> printError "Invalid choice." >> handleOverlappingLesson newLesson timetable

-- Edit a lesson
editLesson :: [Lesson] -> IO [Lesson]
editLesson lessons = do
    let lessonToString lesson = show (day lesson) ++ " " ++ show (time lesson)
    selectedLesson <- selectItem "Edit lesson" lessons lessonToString
    case selectedLesson of
        Nothing -> return lessons
        Just idx -> do
            let selectedLesson = lessons !! idx
            printHeader $ show (day selectedLesson) ++ " " ++ show (time selectedLesson) 
            putStrLn "1. Day"
            putStrLn "2. Time"
            putStrLn "3. Venue"
            putStrLn "4. Lecturer"
            putStrLn "5. Cancel" 
            choice <- getInput "Select option: "

            case readMaybe choice of
                Just 1 -> do
                    newDay <- validateDay
                    return $ replaceAt idx (selectedLesson { day = newDay }) lessons
                Just 2 -> do
                    newTime <- validateTime
                    return $ replaceAt idx (selectedLesson { time = newTime }) lessons
                Just 3 -> do
                    newVenue <- getInput "Enter new venue: "
                    return $ replaceAt idx (selectedLesson { venue = newVenue }) lessons
                Just 4 -> do
                    newLecturer <- getInput "Enter new lecturer: "
                    return $ replaceAt idx (selectedLesson { lecturer = newLecturer }) lessons
                Just 5 -> return lessons
                _ -> do
                    printError "Invalid choice."
                    editLesson lessons

-- Delete a lesson
deleteLesson :: [Lesson] -> IO [Lesson]
deleteLesson lessons = do
    let lessonToString lesson = show (day lesson) ++ " " ++ show (time lesson)
    selectedLesson <- selectItem "Delete lesson" lessons lessonToString
    case selectedLesson of
        Nothing -> return lessons
        Just idx -> do
            let updatedLessons = take idx lessons ++ drop (idx + 1) lessons
            return updatedLessons

-- Save timetables to json file
saveTimetables :: TimetableList -> IO ()
saveTimetables ts = B.writeFile "timetables.json" (encode ts)

-- Load timetables from json file
loadTimetables :: IO (Maybe TimetableList)
loadTimetables = do
    fileExists <- doesFileExist "timetables.json"
    if fileExists
        then do
            content <- B.readFile "timetables.json"
            return $ decode content
        else return Nothing
