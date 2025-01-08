module UI where

import System.Console.ANSI

printHeader :: String -> IO ()
printHeader header = do
    let lineLength = length header + 8
        line = replicate lineLength '='

    setSGR [SetColor Foreground Dull Blue]
    putStrLn line
    putStrLn (replicate 4 ' ' <> header <> replicate 4 ' ')
    putStrLn line
    setSGR [Reset]

printMenu :: IO ()
printMenu = do
    printHeader "Welcome to Timetable Creator!"
    putStrLn "1. Create a new timetable"
    putStrLn "2. Edit a timetable"
    putStrLn "3. Import a timetable"
    putStrLn "4. Export a timetable"
    putStrLn "5. Exit"

printError :: String -> IO()
printError message = do
    setSGR [SetColor Foreground Dull Red]
    putStr message
    setSGR [Reset]
