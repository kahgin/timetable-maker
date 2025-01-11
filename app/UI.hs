module UI where

import System.Console.ANSI

-- Print header
printHeader :: String -> IO ()
printHeader header = do
    let lineLength = length header + 8
        line = replicate lineLength '='

    setSGR [SetColor Foreground Dull Blue]
    putStrLn ""
    putStrLn line
    putStrLn (replicate 4 ' ' <> header <> replicate 4 ' ')
    putStrLn line
    setSGR [Reset]

-- Print error message
printError :: String -> IO()
printError message = do
    setSGR [SetColor Foreground Dull Red]
    putStrLn message
    setSGR [Reset]

-- Print example message
printExample :: String -> IO ()
printExample message = do
    setSGR [SetColor Foreground Dull Cyan]
    putStrLn message
    setSGR [Reset]

-- Print exit instruction
printExit :: IO ()
printExit = do
    setSGR [SetColor Foreground Dull Blue]
    putStrLn "Press [Enter] to exit.\n"
    setSGR [Reset]
