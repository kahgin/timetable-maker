module Main where

import UI
import System.IO

data Subject = Subject { 
    -- must have
    subjectName :: String, 
    color :: String,

    -- optional
    description :: Maybe String,
    lessons :: [Lesson]

    } deriving (Show)

data Lesson = Lesson {
    -- must have
    day :: String,
    time :: String,

    -- optional
    room :: String,
    teacher :: String

    } deriving (Show)

data Timetable = Timetable {
    -- must have
    timetableName :: String,
    palette :: [String],
    start :: String,
    end :: String,

    -- optional
    subjects :: [Subject]

    } deriving (Show)

data Palette = Palette {
    paletteName :: String,
    colors :: [String]

    } deriving (Show)

main :: IO ()
main = do
    printMenu

    putStr "Select option (1-5): "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> putStrLn "createTimetable"
        "2" -> putStrLn "editTimetable"
        "3" -> putStrLn "importTimetable"
        "4" -> putStrLn "exportTimetable"
        "5" -> putStrLn "Goodbye!"
        _   -> printError "\nInvalid choice!\n\n" >> main
    