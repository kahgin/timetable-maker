module Main where

import UI
import Helper
import Timetable
import Data.Maybe

mainMenu :: TimetableList -> IO ()
mainMenu timetables = do
    printHeader "Welcome to Timetable Creator!"
    putStrLn "1. Create a new timetable"
    putStrLn "2. Edit a timetable"
    putStrLn "3. Delete a timetable"
    printExit
    choice <- getInput "Select option (1-4): "

    case choice of
        "1" -> do
            newTimetable <- createTimetable timetables
            let newTimetableList = timetables ++ [newTimetable]
            saveTimetables newTimetableList
            mainMenu newTimetableList
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
        ""  -> putStrLn "Goodbye!"
        _   -> printError "\nInvalid choice!" >> mainMenu timetables

main :: IO ()
main = do
    maybeTimetables <- loadTimetables
    mainMenu (fromMaybe [] maybeTimetables)
