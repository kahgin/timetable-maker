{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Timetable where

import UI
import Helper
import Data.Time (DayOfWeek(..))
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import GHC.Generics (Generic)
import Control.Monad (unless, when)
import Text.Read (readMaybe)
import Data.Maybe (maybe, listToMaybe)
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
createTimetable :: TimetableList -> IO Timetable
createTimetable timetables =
    printHeader "Create a new Timetable" >>
    validateName "Timetable" (map timetableName timetables) id >>= \timetableName ->
    let newTimetable = Timetable { timetableName = timetableName, subjects = [] }
    in printSuccess "Timetable created successfully." >> manageTimetable newTimetable timetables

-- Display a menu for user to either edit timetable name or manage subjects (add, edit, delete) in the timetable
manageTimetable :: Timetable -> TimetableList -> IO Timetable
manageTimetable timetable timetables =
    printHeader (timetableName timetable) >>
    unless (null $ subjects timetable) (displaySubjectsWithLessons (subjects timetable)) >>
    putStrLn "1. Edit timetable name" >>
    putStrLn "2. Add a subject" >>
    putStrLn "3. Edit a subject" >>
    putStrLn "4. Delete a subject" >>
    printExit >>
    getInput "Select option (1-4): " >>= \choice ->
    case choice of
        "1" ->
            printHeader "Edit Timetable Name" >>
            validateName "Timetable" (map timetableName timetables) id >>= \newName ->
            let updatedTimetable = timetable { timetableName = newName }
            in manageTimetable (timetable { timetableName = newName }) timetables
        "2" ->
            createSubject timetable >>= \newSubject ->
            let updatedTimetable = timetable { subjects = subjects timetable <> [newSubject] }
            in manageTimetable (timetable { subjects = subjects timetable <> [newSubject] }) timetables
        "3" ->
            if null (subjects timetable) then
                printError "No subjects available." >>
                manageTimetable timetable timetables
            else
                editSubject timetable >>= \updatedSubjects ->
                let updatedTimetable = timetable { subjects = updatedSubjects }
                in manageTimetable (timetable { subjects = updatedSubjects }) timetables
        "4" ->
            if null (subjects timetable) then
                printError "No subjects available." >>
                manageTimetable timetable timetables
            else
                deleteSubject (subjects timetable) >>= \updatedSubjects ->
                let updatedTimetable = timetable { subjects = updatedSubjects }
                in manageTimetable (timetable { subjects = updatedSubjects }) timetables
        ""  -> saveTimetables (updateTimetableInList timetable timetables) >> return timetable
        _   -> printError "Invalid choice!" >> manageTimetable timetable timetables

-- Update the list of timetables with the updated timetable
updateTimetableInList :: Timetable -> TimetableList -> TimetableList
updateTimetableInList updatedTimetable [] = [updatedTimetable]
updateTimetableInList updatedTimetable timetables = 
    case findIndex (\t -> timetableName t == timetableName updatedTimetable) timetables of
        Just idx -> take idx timetables <> [updatedTimetable] <> drop (idx + 1) timetables
        Nothing -> timetables <> [updatedTimetable]
        
-- Display a menu for user to select a timetable to edit
editTimetable :: TimetableList -> IO TimetableList
editTimetable timetables =
    selectItem "Edit a Timetable" timetables timetableName >>= \idx ->
    case idx of
        Nothing -> return timetables
        Just idx ->
            manageTimetable (timetables !! idx) timetables >>= \updatedTimetable ->
            return $ replaceAt idx updatedTimetable timetables

-- Display a menu for user to select a timetable to delete
deleteTimetable :: TimetableList -> IO TimetableList
deleteTimetable timetables =
    selectItem "Delete a Timetable" timetables timetableName >>= \idx ->
    case idx of
        Nothing -> return timetables
        Just idx ->
            let updatedTimetables = take idx timetables <> drop (idx + 1) timetables
            in printSuccess "Timetable deleted successfully." >> return updatedTimetables
            
-- Display all subjects with their corresponding lessons in a timetable
displaySubjectsWithLessons :: [Subject] -> IO ()
displaySubjectsWithLessons subjects =
    let border = replicate 30 '=' 
    in mapM_ printSubjectWithLessons (zip subjects [1..])
    where
        printSubjectWithLessons (subject, index) =
            (when (index == 1) (putStrLn "")) >>
            putStrLn (subjectName subject) >>
            if null (lessons subject) then printError "\nNo lessons available.\n" else displayLessons (lessons subject)
            
-- Create a new subject
createSubject :: Timetable -> IO Subject
createSubject timetable = 
    printHeader "Add a Subject" >> 
    (validateName "Subject" (map subjectName (subjects timetable)) id >>= \subjectName -> 
        let newSubject = Subject { subjectName = subjectName, lessons = [] } 
        in printSuccess "Subject added successfully." >> manageSubject newSubject timetable)

-- Display a menu for user to either edit subject name or manage lessons (add, edit, delete) in the subject
manageSubject :: Subject -> Timetable -> IO Subject
manageSubject subject timetable = 
    printHeader (subjectName subject) >> 
    unless (null $ lessons subject) (displayLessons (lessons subject)) >> 
    putStrLn "1. Edit subject name" >> 
    putStrLn "2. Add a lesson" >> 
    putStrLn "3. Edit a lesson" >> 
    putStrLn "4. Delete a lesson" >> 
    printExit >>
    getInput "Select option (1-4): " >>= \choice ->
        case choice of
            "1" -> 
                printHeader "Edit Subject Name" >> 
                validateName "Subject" (map subjectName (subjects timetable)) id >>= \newName -> 
                    let updatedSubject = subject { subjectName = newName }
                    in printSuccess "Subject name updated successfully." >>
                        updateSubjectInTimetable updatedSubject timetable

            "2" -> 
                createLesson (lessons subject) timetable >>= \updatedLessons -> 
                    let updatedSubject = subject { lessons = updatedLessons }
                    in updateSubjectInTimetable updatedSubject timetable

            "3" -> 
                if null $ lessons subject then
                    printError "No lessons available." >>
                    manageSubject subject timetable
                else
                    editLesson (lessons subject) timetable >>= \updatedLessons -> 
                        let updatedSubject = subject { lessons = updatedLessons }
                        in updateSubjectInTimetable updatedSubject timetable

            "4" -> 
                if null $ lessons subject then
                    printError "No lessons available." >>
                    manageSubject subject timetable
                else
                    deleteLesson (lessons subject) >>= \updatedLessons -> 
                        let updatedSubject = subject { lessons = updatedLessons }
                        in updateSubjectInTimetable updatedSubject timetable

            "" -> return subject

            _ -> printError "Invalid choice!" >> manageSubject subject timetable

    where
        -- Update the subject in the timetable
        updateSubjectInTimetable :: Subject -> Timetable -> IO Subject
        updateSubjectInTimetable updatedSubject currentTimetable = 
            let updatedSubjects = replaceSubjectInList updatedSubject (subjects currentTimetable)
                updatedTimetable = currentTimetable { subjects = updatedSubjects }
            in manageSubject updatedSubject updatedTimetable

        -- Replace a subject in a list of subjects
        replaceSubjectInList :: Subject -> [Subject] -> [Subject]
        replaceSubjectInList newSubject subjects = 
            zipWith (\oldSubject index -> if subjectName oldSubject == subjectName newSubject then newSubject else oldSubject) subjects [0..]

-- Display a menu for user to select a subject to edit
editSubject :: Timetable -> IO [Subject]
editSubject timetable =
    let subjectList = subjects timetable
    in selectItem "Edit subject" subjectList subjectName >>= \selectedIdx ->
    case selectedIdx of
        Nothing -> return subjectList
        Just idx ->
            manageSubject (subjectList !! idx) timetable >>= \updatedSubject ->
            let updatedSubjects = replaceAt idx updatedSubject subjectList
            in return updatedSubjects

-- Display a menu for user to select a subject to delete
deleteSubject :: [Subject] -> IO [Subject]
deleteSubject subjects =
    selectItem "Delete subject" subjects subjectName >>= \selectedIdx ->
    case selectedIdx of
        Nothing -> return subjects
        Just idx ->
            printSuccess "Subject deleted successfully." >>
            return (take idx subjects <> drop (idx + 1) subjects)

-- Display all lessons of a subject
displayLessons :: [Lesson] -> IO ()
displayLessons lessons =
    let border = replicate 60 '='
    in  putStrLn "" >>
        putStrLn border >>
        mapM_ printLessonWithBorder (zip lessons [1..]) >>
        putStrLn border >>
        putStrLn ""
    where
        printLessonWithBorder (lesson, index) =
            putStrLn ("Day: " <> show (day lesson)) >>
            putStrLn ("Time: " <> show (time lesson)) >>
            putStrLn ("Venue: " <> venue lesson) >>
            putStrLn ("Lecturer: " <> lecturer lesson) >>
            (if index == length lessons then return () else putStrLn $ replicate 60 '-')
            
-- Create a new lesson
createLesson :: [Lesson] -> Timetable -> IO [Lesson]
createLesson lessons timetable =
    printHeader "Add a Lesson" >>
    validateDay >>= \day ->
    validateTime >>= \time ->
    let newLesson = Lesson { day = day, time = time, venue = "", lecturer = "" }
    in handleOverlapLesson newLesson timetable >>= \overlappingLesson ->
    case overlappingLesson of
        Nothing -> return lessons
        Just lesson ->
            getInput "Venue (optional): " >>= \venue ->
            getInput "Lecturer (optional): " >>= \lecturer ->
            printSuccess "Lesson added successfully." >>
            return (lessons <> [lesson { venue = venue, lecturer = lecturer }])

-- Check if overlapping lesson exist and return the subject name and the lesson if found
checkOverlappingLesson :: Lesson -> Timetable -> Maybe (Subject, Lesson)
checkOverlappingLesson newLesson timetable =
    listToMaybe [(subject, lesson) | subject <- subjects timetable, lesson <- lessons subject, day lesson == day newLesson, isOverlap (time lesson) (time newLesson)]

-- Display a menu for user to handle overlapping lesson (re-enter day & time or stop adding / editing)
handleOverlapLesson :: Lesson -> Timetable -> IO (Maybe Lesson)
handleOverlapLesson newLesson timetable =
    case checkOverlappingLesson newLesson timetable of
        Nothing -> return $ Just newLesson
        Just (subject, overlappingLesson) ->
            printError ("Overlapping lesson found in " <> subjectName subject <> " " <> show (day overlappingLesson) <> " " <> show (time overlappingLesson)) >>
            putStrLn "1. Re-enter day & time" >>
            putStrLn "2. Stop adding / editing" >>
            getInput "Select option: " >>= \choice ->
            case choice of
                "1" -> 
                    validateDay >>= \day ->
                    validateTime >>= \time ->
                    let newLesson = Lesson { day = day, time = time, venue = "", lecturer = "" }
                    in handleOverlapLesson newLesson timetable
                "2" -> return Nothing
                _   -> printError "Invalid choice." >> handleOverlapLesson newLesson timetable

-- Display a menu for user to select a lesson to edit, then prompt the user to choose which field to edit
editLesson :: [Lesson] -> Timetable -> IO [Lesson]
editLesson lessons timetable =
    let lessonToString lesson = show (day lesson) <> " " <> show (time lesson)
    in selectItem "Edit lesson" lessons lessonToString >>= \selectedLesson ->
    case selectedLesson of
        Nothing -> return lessons
        Just idx -> 
            let selectedLesson = lessons !! idx
            in  printHeader (show (day selectedLesson) <> " " <> show (time selectedLesson)) >>
                putStrLn "1. Day" >>
                putStrLn "2. Time" >>
                putStrLn "3. Venue" >>
                putStrLn "4. Lecturer" >>
                putStrLn "5. Cancel" >>
                getInput "Select option: " >>= \choice ->
                case readMaybe choice of
                    Just 1 ->
                        printMessage ("Current day: " <> show (day selectedLesson)) >>
                        validateDay >>= \newDay ->
                        let tempLesson = selectedLesson { day = newDay }
                        in handleOverlapLesson tempLesson timetable >>= \updatedLesson ->
                        case updatedLesson of
                            Nothing -> editLesson lessons timetable
                            Just updatedLesson ->
                                printSuccess "Lesson updated successfully." >>
                                return (replaceAt idx updatedLesson lessons)
                    Just 2 ->
                        printMessage ("Current time: " <> show (time selectedLesson)) >>
                        validateTime >>= \newTime ->
                        let tempLesson = selectedLesson { time = newTime }
                        in handleOverlapLesson tempLesson timetable >>= \updatedLesson ->
                        case updatedLesson of
                            Nothing -> editLesson lessons timetable
                            Just updatedLesson ->
                                printSuccess "Lesson updated successfully." >>
                                return (replaceAt idx updatedLesson lessons)
                    Just 3 ->
                        printMessage ("Current venue: " <> venue selectedLesson) >>
                        getInput "Enter new venue: " >>= \newVenue ->
                        printSuccess "Lesson updated successfully." >>
                        return (replaceAt idx (selectedLesson { venue = newVenue }) lessons)
                    Just 4 ->
                        printMessage ("Current lecturer: " <> lecturer selectedLesson) >>
                        getInput "Enter new lecturer: " >>= \newLecturer ->
                        printSuccess "Lesson updated successfully." >>
                        return (replaceAt idx (selectedLesson { lecturer = newLecturer }) lessons)
                    Just 5 -> return lessons
                    _ -> printError "Invalid choice." >> editLesson lessons timetable

-- Display a menu for user to select a lesson to delete
deleteLesson :: [Lesson] -> IO [Lesson]
deleteLesson lessons =
    let lessonToString lesson = show (day lesson) <> " " <> show (time lesson)
    in selectItem "Delete lesson" lessons lessonToString >>= \selectedLesson ->
    case selectedLesson of
        Nothing -> return lessons
        Just idx ->
            let updatedLessons = take idx lessons <> drop (idx + 1) lessons
            in printSuccess "Lesson deleted successfully." >> return updatedLessons

-- Save timetables to json file
saveTimetables :: TimetableList -> IO ()
saveTimetables ts = B.writeFile "timetables.json" (encode ts)

-- Load timetables from json file
loadTimetables :: IO (Maybe TimetableList)
loadTimetables =
    doesFileExist "timetables.json" >>= \fileExists ->
    if fileExists
        then
            fmap decode (B.readFile "timetables.json")
        else return Nothing
