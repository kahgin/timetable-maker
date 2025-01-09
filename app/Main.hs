module Main where

import UI
import File
import Timetable
import System.IO

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