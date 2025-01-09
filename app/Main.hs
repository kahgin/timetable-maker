module Main where

import UI
import Helper
import File
import System.IO
import Data.Time
import Data.Aeson
import GHC.Generics

data Timetable = Timetable {
    timetableName :: String,
    subjects :: [Subject]
    } deriving (Show, Generic, ToJSON, FromJSON)

instance FromJSON Timetable
instance ToJSON Timetable

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

createTimetable :: IO Timetable
createTimetable = do
    clearScreen
    printHeader "Create a new timetable"
    putStr "Timetable name: "
    hFlush stdout
    timetableName <- getLine

    let newTimetable = Timetable { timetableName = timetableName, subjects = [] }
    manageTimetable newTimetable


manageTimetable :: Timetable -> IO Timetable
manageTimetable timetable = do
    clearScreen
    printHeader (timetableName timetable)
    putStrLn "1. Add a subject"
    putStrLn "2. Edit a subject"
    putStrLn "3. Delete a subject"
    printExit

    putStr "Select option (1-3): "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> createSubject                        >> manageTimetable timetable
        "2" -> putStrLn "editSubject"               >> manageTimetable timetable
        "3" -> putStrLn "deleteSubject"             >> manageTimetable timetable
        ""  -> return timetable
        _   -> printError "\nInvalid choice!\n\n"   >> manageTimetable timetable

editTimetable :: IO ()
editTimetable = do
    clearScreen
    printHeader "Edit a timetable"

createSubject :: IO Subject
createSubject = do
    clearScreen
    printHeader "Add a subject"
    putStr "Subject name: "
    hFlush stdout
    subjectName <- getLine

    let newSubject = Subject { subjectName = subjectName, lessons = [] }
    manageSubject newSubject
      
manageSubject :: Subject -> IO Subject
manageSubject subject = do
    clearScreen
    printHeader (subjectName subject)
    if not (null (lessons subject))
        then displayLessons (lessons subject)
        else return ()
    putStrLn "1. Add a lesson"
    putStrLn "2. Edit a lesson"
    putStrLn "3. Delete a lesson"
    printExit

    putStr "Select option (1-3): "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> do
            newLesson <- createLesson
            let updatedSubject = subject { lessons = lessons subject ++ [newLesson] }
            manageSubject updatedSubject
        "2" -> putStrLn "editLesson"                >> manageSubject subject
        "3" -> displayLessons (lessons subject)     >> manageSubject subject
        ""  -> return subject
        _   -> printError "\nInvalid choice!\n\n"   >> manageSubject subject

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

editSubject :: IO ()
editSubject = do
    putStr "Select option: "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> putStrLn "editSubjectName"         >> editSubject
        "2" -> putStrLn "editLesson"              >> editSubject
        ""  -> return ()
        _   -> printError "\nInvalid choice!\n\n" >> editSubject

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

mainMenu :: IO ()
mainMenu = do
    -- clearScreen
    printHeader "Welcome to Timetable Creator!"
    putStrLn "1. Create a new timetable"
    putStrLn "2. Edit a timetable"
    putStrLn "3. Import a timetable"
    putStrLn "4. Export a timetable"
    printExit

    putStr "Select option (1-4): "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> createTimetable                      >> mainMenu
        "2" -> editTimetable                        >> mainMenu
        "3" -> putStrLn "importTimetable"           >> mainMenu
        "4" -> putStrLn "exportTimetable"           >> mainMenu
        ""  -> putStrLn "Goodbye!"
        _   -> printError "\nInvalid choice!\n\n"   >> mainMenu

main :: IO ()
main = mainMenu