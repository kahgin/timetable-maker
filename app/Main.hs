module Main where

import UI
import Timetable
import System.IO
import Data.Maybe

mainMenu :: [Timetable] -> IO ()
mainMenu timetables = do
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
        "1" -> createTimetable timetables           >> mainMenu timetables
        "2" -> editTimetable timetables             >> mainMenu timetables
        "3" -> putStrLn "importTimetable"           >> mainMenu timetables
        "4" -> putStrLn "exportTimetable"           >> mainMenu timetables
        ""  -> putStrLn "Goodbye!"
        _   -> printError "\nInvalid choice!\n\n"   >> mainMenu timetables

main :: IO ()
main = do
    maybeTimetables <- loadTimetables
    mainMenu (fromMaybe [] maybeTimetables)
