module Main where

import UI
import Helper
import Timetable
import View
import Data.Maybe

-- Main menu
mainMenu :: TimetableDB -> IO ()
mainMenu timetables =
    printHeader "Welcome to Timetable Creator!" >>
    putStrLn "1. Create a new timetable" >>
    putStrLn "2. Edit a timetable" >>
    putStrLn "3. Delete a timetable" >>
    putStrLn "4. View a timetable" >>
    printExit >>
    getInput "Select option (1-4): " >>= \choice ->

    case choice of
        "1" ->
            createTimetable timetables >>= \newTimetable ->
            let newTimetableDB = timetables <> [newTimetable]
            in saveTimetables newTimetableDB >> mainMenu newTimetableDB
        "2" ->
            if not (null timetables) then
                editTimetable timetables >>= \updatedTimetables ->
                saveTimetables updatedTimetables >> mainMenu updatedTimetables
            else
                printError "\nNo timetable available." >> mainMenu timetables
        "3" -> deleteTimetable timetables >>= \updatedTimetables ->
               saveTimetables updatedTimetables >> mainMenu updatedTimetables
        "4" -> viewTimetable timetables >> mainMenu timetables
        ""  -> putStrLn "Goodbye!"
        _   -> printError "\nInvalid choice!" >> mainMenu timetables

-- Entry point
main :: IO ()
main =
    clearScreen >>
    loadTimetables >>= \maybeTimetables ->
    mainMenu (fromMaybe [] maybeTimetables)
