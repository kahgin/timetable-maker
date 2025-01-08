module Main where

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
    time :: Maybe String,

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
main = putStrLn "Hello, world!"