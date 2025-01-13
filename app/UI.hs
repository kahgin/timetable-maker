module UI where

import System.Console.ANSI

-- Print header
printHeader :: String -> IO ()
printHeader header =
    let headerWidth = 180
        padding = (headerWidth - length header) `div` 2
        line = replicate headerWidth '='
    in
        setSGR [SetColor Foreground Dull Blue] >>
        putStrLn "" >>
        putStrLn line >>
        putStrLn (replicate padding ' ' <> header <> replicate padding ' ') >>
        putStrLn line >>
        setSGR [Reset]

-- Print error message
printError :: String -> IO()
printError message =
    setSGR [SetColor Foreground Dull Red] >> putStrLn message >> setSGR [Reset]

-- Print example message
printMessage :: String -> IO ()
printMessage message =
    setSGR [SetColor Foreground Dull Cyan] >> putStrLn message >> setSGR [Reset]

-- Print exit instruction
printExit :: IO ()
printExit = setSGR [SetColor Foreground Dull Blue] >> putStrLn "Press [Enter] to exit." >> setSGR [Reset]

-- Print success message
printSuccess :: String -> IO ()
printSuccess message = setSGR [SetColor Foreground Dull Green] >> putStrLn ("\n" <> message) >> setSGR [Reset]

-- Clear screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

confirmAction :: String -> IO Bool
confirmAction message = do
    printMessage $ message <> " (y/n): "
    response <- getLine
    return $ response `elem` ["y", "Y", "yes", "Yes"]
