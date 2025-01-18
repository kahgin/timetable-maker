{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Timetable where

import UI
import Utility
import Data.Time (DayOfWeek(..))
import Data.Aeson (encode, decode)
import Control.Monad (forM_, unless, when)
import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust)

type TimetableDB = Map.Map String SubjectMap        -- timetableName -> subjects
type SubjectMap = Map.Map String LessonMap          -- subjectName -> lessons
type LessonMap = Map.Map TimeSlot LessonDetails     -- (day,time) -> details
type TimeSlot = (DayOfWeek, TimeRange) 
type LessonDetails = (String, String)               -- (venue, lecturer)

-- Create a new timetable interactively
createTimetable :: TimetableDB -> IO TimetableDB
createTimetable db = do
    printHeader "Create a new Timetable"
    name <- validateName "Timetable" db
    let newDb = Map.insert name Map.empty db
    printSuccess "Timetable created successfully."
    return newDb

-- Display a menu for user to either edit timetable name or manage subjects (add, edit, delete) in the timetable
manageTimetableMenu :: String -> TimetableDB -> IO TimetableDB
manageTimetableMenu tName db = do
    printHeader tName
    case Map.lookup tName db of
        Just subjects -> 
            unless (Map.null subjects) $ do
                forM_ (Map.toList subjects) $ \(sName, lessons) -> do
                    putStrLn $ "Subject: " <> sName
                    if Map.null lessons
                        then printMessage "\nNo lessons available.\n"
                        else displayLessons lessons
        Nothing -> putStr ""

    putStrLn "1. Edit timetable name"
    putStrLn "2. Add a subject"
    putStrLn "3. Edit a subject"
    putStrLn "4. Delete a subject"
    printExit
    
    choice <- getInput "Select option (1-4): "
    case choice of
        "1" -> do
            newDb <- editTimetableName tName db
            let newName = head [name | (name, subjects) <- Map.toList newDb, subjects == (fromJust $ Map.lookup tName db)]
            manageTimetableMenu newName newDb
            
        "2" -> do
            newDb <- createSubject tName db
            case [name | (name, _) <- Map.toList (fromJust $ Map.lookup tName newDb), 
                        name `notElem` map fst (Map.toList (fromMaybe Map.empty $ Map.lookup tName db))] of
                (newSubject:_) -> do
                    newerDb <- manageSubjectMenu tName newSubject newDb
                    manageTimetableMenu tName newerDb
                _ -> manageTimetableMenu tName newDb
            
        "3" -> case Map.lookup tName db of
            Nothing -> do
                printError "Timetable not found."
                return db
            Just subjects -> 
                if Map.null subjects
                    then do
                        printError "No subjects available."
                        manageTimetableMenu tName db
                    else do
                        newDb <- editSubjectMenu tName db
                        manageTimetableMenu tName newDb
                        
        "4" -> case Map.lookup tName db of
            Nothing -> do
                printError "Timetable not found."
                return db
            Just subjects -> 
                if Map.null subjects
                    then do
                        printError "No subjects available."
                        manageTimetableMenu tName db
                    else do
                        newDb <- deleteSubjectMenu tName db
                        manageTimetableMenu tName newDb
                        
        ""  -> return db
        
        _   -> do
            printError "Invalid choice!"
            manageTimetableMenu tName db

-- Display functions
displaySubjectsWithLessons :: SubjectMap -> IO ()
displaySubjectsWithLessons subjects = do
    let border = replicate 30 '='
    putStrLn ""
    putStrLn border
    forM_ (Map.toList subjects) $ \(subjectName, lessons) -> do
        putStrLn $ "Subject: " <> subjectName
        if Map.null lessons 
            then printError "No lessons available.\n"
            else displayLessons lessons
        putStrLn border

displayLessons :: Map.Map TimeSlot LessonDetails -> IO ()
displayLessons lessons = do
    let border = replicate 60 '='
        lessonsList = Map.toList lessons
    putStrLn ""
    putStrLn border
    forM_ (zip [1..] lessonsList) $ \(index, ((day, timeRange), (venue, lecturer))) -> do
        putStrLn $ "Day: " <> show day
        putStrLn $ "Time: " <> show timeRange
        putStrLn $ "Venue: " <> venue
        putStrLn $ "Lecturer: " <> lecturer
        when (index < length lessonsList) $ putStrLn $ replicate 60 '-'
    putStrLn border
    putStrLn ""

-- Display a menu for user to select a timetable to edit
editTimetableMenu :: TimetableDB -> IO TimetableDB
editTimetableMenu db = do
    printHeader "Edit a Timetable"
    mapM_ (\(name, _) -> putStrLn $ "- " <> name) (Map.toList db)
    tName <- getInput "Enter timetable name: "
    case Map.lookup tName db of
        Nothing -> do
            printError "Timetable not found."
            return db
        Just _ -> manageTimetableMenu tName db

-- Display a menu for user to select a timetable to delete
deleteTimetable :: String -> TimetableDB -> IO TimetableDB
deleteTimetable name db = do
    case Map.lookup name db of
        Nothing -> do
            printError "Timetable not found."
            return db
        Just _ -> do
            confirmed <- confirmAction $ "Are you sure you want to delete timetable '" <> name <> "'?"
            if confirmed
                then do
                    let newDb = Map.delete name db
                    printSuccess "Timetable deleted successfully."
                    return newDb
                else do
                    printMessage "Deletion cancelled."
                    return db

-- Create a new subject interactively
createSubject :: String -> TimetableDB -> IO TimetableDB
createSubject tName db = do
    case Map.lookup tName db of
        Nothing -> do
            printError "Timetable not found."
            return db
        Just subjects -> do
            printHeader "Add a Subject"
            sName <- validateName "Subject" subjects
            let newSubjects = Map.insert sName Map.empty subjects
                newDb = Map.insert tName newSubjects db
            printSuccess "Subject created successfully."
            return newDb

-- Display a menu for user to either edit subject name or manage lessons (add, edit, delete) in the subject
manageSubjectMenu :: String -> String -> TimetableDB -> IO TimetableDB
manageSubjectMenu tName sName db = do
    printHeader sName

    case Map.lookup tName db >>= Map.lookup sName of
        Just lessons -> unless (Map.null lessons) $ displayLessons lessons
        Nothing -> putStr ""

    putStrLn "1. Edit subject name"
    putStrLn "2. Add a lesson"
    putStrLn "3. Edit a lesson"
    putStrLn "4. Delete a lesson"
    printExit

    choice <- getInput "Select option (1-4): "
    case choice of
        "1" -> do
            newDb <- editSubjectName tName sName db
            manageTimetableMenu tName newDb
            
        "2" -> do
            newDb <- createLesson tName sName db
            manageSubjectMenu tName sName newDb
            
        "3" -> do
            newDb <- handleLessonOperation editLesson tName sName "Edit" db
            manageSubjectMenu tName sName newDb
            
        "4" -> do
            newDb <- handleLessonOperation deleteLesson tName sName "Delete" db
            manageSubjectMenu tName sName newDb
            
        ""  -> manageTimetableMenu tName db
        
        _ -> do
            printError "Invalid choice!"
            manageSubjectMenu tName sName db

handleLessonOperation :: (String -> String -> TimeSlot -> TimetableDB -> IO TimetableDB) -> String -> String -> String -> TimetableDB -> IO TimetableDB
handleLessonOperation operation tName sName opType db = do
    printHeader $ opType <> " Lesson"
    case Map.lookup tName db >>= Map.lookup sName of
        Nothing -> do
            printError "Subject not found."
            return db
        Just lessons -> 
            if Map.null lessons
                then do
                    printError "No lessons available."
                    return db
                else do
                    displayLessons lessons
                    day <- validateDay
                    timeRange <- validateTime
                    let timeSlot = (day, timeRange)
                    operation tName sName timeSlot db

-- Edit timetable name
editTimetableName :: String -> TimetableDB -> IO TimetableDB
editTimetableName oldName db = do
    case Map.lookup oldName db of
        Nothing -> do
            printError "Timetable not found."
            return db
        Just subjects -> do
            printHeader "Edit Timetable Name"
            newName <- validateName "New timetable" (Map.delete oldName db)
            let newDb = Map.insert newName subjects $ Map.delete oldName db
            printSuccess "Timetable renamed successfully."
            return newDb

-- Edit subject name
editSubjectName :: String -> String -> TimetableDB -> IO TimetableDB
editSubjectName tName oldName db = do
    case Map.lookup tName db of
        Nothing -> do
            printError "Timetable not found."
            return db
        Just subjects -> case Map.lookup oldName subjects of
            Nothing -> do
                printError "Subject not found."
                return db
            Just lessons -> do
                printHeader "Edit Subject Name"
                newName <- validateName "New subject" (Map.delete oldName subjects)
                let newSubjects = Map.insert newName lessons $ Map.delete oldName subjects
                    newDb = Map.insert tName newSubjects db
                printSuccess "Subject renamed successfully."
                return newDb

-- Display a menu for user to select a subject to edit
editSubjectMenu :: String -> TimetableDB -> IO TimetableDB
editSubjectMenu tName db = do
    printHeader "Edit Subject"
    case Map.lookup tName db of
        Nothing -> do
            printError "Timetable not found."
            return db
        Just subjects -> do
            mapM_ (\(name, _) -> putStrLn $ "- " <> name) (Map.toList subjects)
            sName <- getInput "Enter subject name: "
            case Map.lookup sName subjects of
                Nothing -> do
                    printError "Subject not found."
                    return db
                Just _ -> manageSubjectMenu tName sName db

deleteSubjectMenu :: String -> TimetableDB -> IO TimetableDB
deleteSubjectMenu tName db = do
    printHeader "Delete a Subject"
    case Map.lookup tName db of
        Nothing -> do
            printError "Timetable not found."
            return db
        Just subjects -> do
            mapM_ (\(name, _) -> putStrLn $ "- " <> name) (Map.toList subjects)
            sName <- getInput "Enter subject name to delete: "
            deleteSubject tName sName db

-- Display a menu for user to select a subject to delete
deleteSubject :: String -> String -> TimetableDB -> IO TimetableDB
deleteSubject tName sName db = do
    case Map.lookup tName db of
        Nothing -> do
            printError "Timetable not found."
            return db
        Just subjects -> case Map.lookup sName subjects of
            Nothing -> do
                printError "Subject not found."
                return db
            Just _ -> do
                confirmed <- confirmAction $ "Are you sure you want to delete subject '" <> sName <> "'?"
                if confirmed
                    then do
                        let newSubjects = Map.delete sName subjects
                            newDb = Map.insert tName newSubjects db
                        printSuccess "Subject deleted successfully."
                        return newDb
                    else do
                        printMessage "Deletion cancelled."
                        return db

createLesson :: String -> String -> TimetableDB -> IO TimetableDB
createLesson tName sName db = do
    case Map.lookup tName db of
        Nothing -> do
            printError "Timetable not found."
            return db
        Just subjects -> do
            printHeader "Add a Lesson"
            day <- validateDay
            timeRange <- validateTime
            let timeSlot = (day, timeRange)
            case findTimeConflict timeSlot subjects of
                Just (conflictSubject, (conflictDay, conflictTime)) -> do
                    printError $ "Time slot conflicts with existing lesson in " <> conflictSubject <> " at " <> show conflictDay <> " " <> show conflictTime
                    printMessage "Would you like to:"
                    putStrLn "1. Try a different time"
                    putStrLn "2. Cancel adding lesson"
                    choice <- getInput "Select option (1-2): "
                    case choice of
                        "1" -> createLesson tName sName db
                        _   -> return db
                Nothing -> do
                    venue <- getInput "Venue (optional): "
                    lecturer <- getInput "Lecturer (optional): "
                    let details = (venue, lecturer)
                        currentLessons = fromMaybe Map.empty $ Map.lookup sName subjects
                        newLessons = Map.insert timeSlot details currentLessons
                        newSubjects = Map.insert sName newLessons subjects
                        newDb = Map.insert tName newSubjects db
                    printSuccess "Lesson created successfully."
                    return newDb

editLesson :: String -> String -> TimeSlot -> TimetableDB -> IO TimetableDB
editLesson tName sName oldTimeSlot db = do
    case Map.lookup tName db >>= Map.lookup sName of
        Nothing -> do
            printError "Timetable or subject not found."
            return db
        Just lessons -> case Map.lookup oldTimeSlot lessons of
            Nothing -> do
                printError "Lesson not found."
                return db
            Just (venue, lecturer) -> do
                printHeader "Edit Lesson"
                printMessage "Enter new details (press Enter to keep current value, '-' to clear):"
                
                -- Edit time slot
                printMessage $ "Current day: " <> show (fst oldTimeSlot)
                printMessage $ "Current time: " <> show (snd oldTimeSlot)
                printMessage "Do you want to change the time slot? (y/n): "
                changeTime <- getLine
                
                -- Handle time slot changes in a separate do block
                newTimeSlot <- do
                    if changeTime `elem` ["y", "Y", "yes", "Yes"]
                        then do
                            day <- validateDay
                            timeRange <- validateTime
                            let newSlot = (day, timeRange)
                                subjects = fromJust $ Map.lookup tName db
                                lessonsWithoutCurrent = Map.adjust (Map.delete oldTimeSlot) sName subjects
                            case findTimeConflict newSlot lessonsWithoutCurrent of
                                Just (conflictSubject, (_, conflictTime)) -> do
                                    printError $ "This time slot conflicts with existing lesson in " <> conflictSubject <> " at " <> show conflictTime
                                    printMessage "\nWould you like to:"
                                    putStrLn "1. Try a different time"
                                    putStrLn "2. Keep current time"
                                    choice <- getInput "Select option (1-2): "
                                    if choice == "1"
                                        then do
                                            mNewSlot <- lessonMenu tName sName db
                                            case mNewSlot of
                                                Just newSlot' -> return newSlot'
                                                Nothing -> return oldTimeSlot
                                        else return oldTimeSlot
                                Nothing -> return newSlot
                        else return oldTimeSlot
                
                -- Edit venue and lecturer
                newVenue <- getInput $ "Venue (current: " <> venue <> "): "
                newLecturer <- getInput $ "Lecturer (current: " <> lecturer <> "): "
                
                let updatedDetails = 
                        ( if null newVenue then venue 
                          else if newVenue == "-" then "" 
                          else newVenue
                        , if null newLecturer then lecturer 
                          else if newLecturer == "-" then "" 
                          else newLecturer
                        )
                    
                    -- If time slot changed, delete old and insert new
                    newLessons = if oldTimeSlot /= newTimeSlot
                                then Map.delete oldTimeSlot . Map.insert newTimeSlot updatedDetails $ lessons
                                else Map.insert newTimeSlot updatedDetails lessons
                                
                    newDb = Map.adjust (Map.adjust (const newLessons) sName) tName db
                
                printSuccess "Lesson updated successfully."
                return newDb

-- Check for time conflicts across all subjects
findTimeConflict :: TimeSlot -> SubjectMap -> Maybe (String, TimeSlot)
findTimeConflict (day, timeRange) subjects =
    let conflicts = [(sName, (d, tr)) | (sName, lessons) <- Map.toList subjects, (timeSlot@(d, tr), _) <- Map.toList lessons, d == day && isOverlap timeRange tr]
    in case conflicts of
        [] -> Nothing
        (conflict:_) -> Just conflict

-- Display a menu for user to select a lesson to delete
deleteLesson :: String -> String -> TimeSlot -> TimetableDB -> IO TimetableDB
deleteLesson tName sName timeSlot db = do
    case Map.lookup tName db >>= Map.lookup sName of
        Nothing -> do
            printError "Timetable or subject not found."
            return db
        Just lessons -> case Map.lookup timeSlot lessons of
            Nothing -> do
                printError "Lesson not found."
                return db
            Just _ -> do
                confirmed <- confirmAction "Are you sure you want to delete this lesson?"
                if confirmed
                    then do
                        let newLessons = Map.delete timeSlot lessons
                            newDb = Map.adjust (Map.adjust (const newLessons) sName) tName db
                        printSuccess "Lesson deleted successfully."
                        return newDb
                    else do
                        printMessage "Deletion cancelled."
                        return db

getAllLessonsForDay :: DayOfWeek -> SubjectMap -> LessonMap
getAllLessonsForDay day subjects = 
    Map.unionsWith (\_ b -> b) 
        [Map.filterWithKey (\(d, _) _ -> d == day) lessons 
         | (_, lessons) <- Map.toList subjects]

-- Save timetables to json file
saveTimetableDB :: TimetableDB -> IO ()
saveTimetableDB timetableDB = B.writeFile "timetables.json" . encode $ timetableDB

-- Load timetables from json file
loadTimetableDB :: IO TimetableDB
loadTimetableDB =
    doesFileExist "timetables.json" >>= \fileExists ->
        if fileExists 
            then fmap (fromMaybe Map.empty . decode) (B.readFile "timetables.json") 
            else return Map.empty

-- Get lesson time slot from user
lessonMenu :: String -> String -> TimetableDB -> IO (Maybe TimeSlot)
lessonMenu tName sName db = do
    case Map.lookup tName db >>= Map.lookup sName of
        Nothing -> do
            printError "Subject not found."
            return Nothing
        Just lessons -> do
            displaySubjectLessons tName sName db
            day <- validateDay
            timeRange <- validateTime
            let timeSlot = (day, timeRange)
            return $ Just timeSlot

displaySubjectLessons :: String -> String -> TimetableDB -> IO ()
displaySubjectLessons tName sName db = do
    case Map.lookup tName db >>= Map.lookup sName of
        Nothing -> printError "Timetable or subject not found."
        Just lessons -> do
            printHeader $ tName <> " - " <> sName
            if Map.null lessons
                then printMessage "No lessons found."
                else mapM_ displayLesson (Map.toList lessons)
    where
        displayLesson ((day, timeRange), (venue, lecturer)) = do
            putStrLn $ "Day: " <> show day
            putStrLn $ "Time: " <> show timeRange
            putStrLn $ "Venue: " <> venue
            putStrLn $ "Lecturer: " <> lecturer
            putStrLn (replicate 40 '-')

deleteTimetableMenu :: TimetableDB -> IO TimetableDB
deleteTimetableMenu db = do
    printHeader "Delete a Timetable"
    mapM_ (\(name, _) -> putStrLn $ "- " <> name) (Map.toList db)
    tName <- getInput "Enter timetable name to delete: "
    deleteTimetable tName db
