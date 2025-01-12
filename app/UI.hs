module UI where

import System.Console.ANSI

-- Print header
printHeader :: String -> IO ()
printHeader header = do
    let headerWidth = 180
        padding = (headerWidth - length header) `div` 2
        line = replicate headerWidth '='

    setSGR [SetColor Foreground Dull Blue]
    putStrLn ""
    putStrLn line
    putStrLn (replicate padding ' ' <> header <> replicate padding ' ')
    putStrLn line
    setSGR [Reset]

-- Print error message
printError :: String -> IO()
printError message = do
    setSGR [SetColor Foreground Dull Red]
    putStrLn message
    setSGR [Reset]

-- Print example message
printMessage :: String -> IO ()
printMessage message = do
    setSGR [SetColor Foreground Dull Cyan]
    putStrLn message
    setSGR [Reset]

-- Print exit instruction
printExit :: IO ()
printExit = do
    setSGR [SetColor Foreground Dull Blue]
    putStrLn "Press [Enter] to exit."
    setSGR [Reset]

-- Print success message
printSuccess :: String -> IO ()
printSuccess message = do
    setSGR [SetColor Foreground Dull Green]
    putStrLn $ "\n" ++ message
    setSGR [Reset]

-- Clear screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"
