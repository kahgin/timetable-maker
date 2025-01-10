module Main where

import UI
import Timetable
import System.IO
import Data.Maybe

mainMenu :: [Timetable] -> IO ()
mainMenu timetables = do
    printHeader "Welcome to Timetable Creator!"
    putStrLn "1. Create new timetable"
    putStrLn "2. Edit timetable"
    putStrLn "3. Delete timetable"
    putStrLn "4. Import timetable"
    putStrLn "5. Export timetable"
    printExit

    putStr "Select option (1-5): "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> do
            newTimetable <- createTimetable timetables
            let newTimetableList = timetables ++ [newTimetable]
            saveTimetables newTimetableList
            mainMenu timetables
        "2" -> do
            if not (null timetables) 
            then do
                updatedTimetables <- editTimetable timetables
                saveTimetables updatedTimetables
                mainMenu updatedTimetables
            else do
                printError "\nNo timetable available."
                mainMenu timetables
        "3" -> do
            updatedTimetable <- deleteTimetable timetables
            saveTimetables updatedTimetable
            mainMenu updatedTimetable
        "4" -> putStrLn "importTimetable"           >> mainMenu timetables
        "5" -> putStrLn "exportTimetable"           >> mainMenu timetables
        ""  -> putStrLn "Goodbye!"
        _   -> printError "\nInvalid choice!"       >> mainMenu timetables

main :: IO ()
main = do
    maybeTimetables <- loadTimetables
    mainMenu (fromMaybe [] maybeTimetables)
