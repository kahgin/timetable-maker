module Main where

import UI
import Helper
import System.IO

data Timetable = Timetable {
    timetableName :: String,
    subjects :: [Subject]
    } deriving (Show)

data Subject = Subject { 
    subjectName :: String,
    color :: String,
    description :: Maybe String,
    lessons :: [Lesson]
    } deriving (Show)

data Lesson = Lesson {
    day :: String,
    time :: TimeRange,
    room :: String,
    teacher :: String
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
    putStrLn "3. Display all subjects"
    putStrLn "4. Finish"

    putStr "Select option (1-4): "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> createSubject                        >> manageTimetable timetable
        "2" -> putStrLn "editSubject"               >> manageTimetable timetable
        "3" -> putStrLn "displaySubjects"           >> manageTimetable timetable
        "4" -> return timetable
        _   -> printError "\nInvalid choice!\n\n"   >> manageTimetable timetable

createSubject :: IO Subject
createSubject = do
    clearScreen
    printHeader "Add a subject"
    putStr "Subject name: "
    hFlush stdout
    subjectName <- getLine

    let newSubject = Subject { subjectName = subjectName, color = "", description = Nothing, lessons = [] }
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
    putStrLn "4. Finish"

    putStr "Select option (1-4): "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> do
            newLesson <- createLesson
            let updatedSubject = subject { lessons = lessons subject ++ [newLesson] }
            manageSubject updatedSubject
        "2" -> putStrLn "editLesson"                >> manageSubject subject
        "3" -> displayLessons (lessons subject)     >> manageSubject subject
        "4" -> return subject
        _   -> printError "\nInvalid choice!\n\n"   >> manageSubject subject

displayLessons :: [Lesson] -> IO ()
displayLessons lessons = do
    let border = replicate 40 '='
    putStrLn border
    mapM_ printLesson lessons
    putStrLn border
  where
    printLesson lesson = do
        putStrLn $ "Day: " ++ day lesson
        putStrLn $ "Time: " ++ show (time lesson)
        putStrLn $ "Room: " ++ room lesson
        putStrLn $ "Teacher: " ++ teacher lesson
        putStrLn $ replicate 40 '-'

{-editSubject :: IO ()
editSubject = do
    for i, subject in subjects do
        putStrLn (i. <> subject.subjectName)
    putStr "Select option: "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> putStrLn "editSubjectName"
        "2" -> putStrLn "editSubjectColor"
        "3" -> putStrLn "editSubjectDescription"
        "4" -> putStrLn "finish"
        _   -> printError "\nInvalid choice!\n\n" >> editSubject-}

createLesson :: IO Lesson
createLesson = do
    clearScreen
    printHeader "Add a lesson"

    day <- validateDay

    time <- validateTime

    putStr "Room (optional): "
    hFlush stdout
    room <- getLine

    putStr "Teacher (optional): "
    hFlush stdout
    teacher <- getLine

    return Lesson { day = day, time = time, room = room, teacher = teacher }

mainMenu :: IO ()
mainMenu = do
    -- clearScreen
    printHeader "Welcome to Timetable Creator!"
    putStrLn "1. Create a new timetable"
    putStrLn "2. Edit a timetable"
    putStrLn "3. Import a timetable"
    putStrLn "4. Export a timetable"
    putStrLn "Press [Enter] to Exit\n"

    putStr "Select option (1-5): "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> createTimetable                      >> mainMenu
        "2" -> putStrLn "editTimetable"             >> mainMenu
        "3" -> putStrLn "importTimetable"           >> mainMenu
        "4" -> putStrLn "exportTimetable"           >> mainMenu
        ""  -> putStrLn "Goodbye!"
        _   -> printError "\nInvalid choice!\n\n"   >> mainMenu

main :: IO Lesson
main = createLesson
