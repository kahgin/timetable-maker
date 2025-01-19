module Main where

import UI
import Utility
import Timetable
import View
import Scraper
import qualified Data.Map as Map

--main :: IO ()
--main = do
    -- clearScreen
    --db <- loadTimetableDB
    --mainMenu db

main :: IO ()
main = do
    clearScreen
    db <- loadTimetableDB
    newDb <- importTimetableFromHTML "C:/Users/kahgin/Downloads/Sem5 timetable.html"
    -- Merge with existing database if needed
    let mergedDb = Map.union newDb db
    --saveTimetableDB mergedDb
    mainMenu mergedDb

mainMenu :: TimetableDB -> IO ()
mainMenu db = do
    printHeader "Welcome to Timetable Creator!"
    putStrLn "1. Create a new timetable"
    putStrLn "2. Edit a timetable"
    putStrLn "3. Delete a timetable"
    putStrLn "4. View a timetable"
    printExit
    
    choice <- getInput "Select option (1-4): "
    case choice of
        "1" -> do
            newDb <- createTimetable db
            let newTimetableName = head [name | (name, _) <- Map.toList newDb, not $ name `Map.member` db]
            newDb' <- manageTimetableMenu newTimetableName newDb
            saveTimetableDB newDb'
            mainMenu newDb'
            
        "2" -> do
            if Map.null db
                then do
                    printError "No timetables available."
                    mainMenu db
                else do
                    newDb <- editTimetableMenu db
                    saveTimetableDB newDb
                    mainMenu newDb
                    
        "3" -> do
            if Map.null db
                then do
                    printError "No timetables available."
                    mainMenu db
                else do
                    newDb <- deleteTimetableMenu db
                    saveTimetableDB newDb
                    mainMenu newDb
                    
        "4" -> do
            if Map.null db
                then printError "No timetables available."
                else viewTimetableMenu db
            mainMenu db
            
        ""  -> do
            printMessage "Goodbye!"
            
        _ -> do
            printError "Invalid choice!"
            mainMenu db
