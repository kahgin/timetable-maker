module UI where

import System.Console.ANSI

palettes :: [(String, [String])]
palettes =
    [ ("Pastel", ["#e4dde3", "#ffe8d6", "#fcd9db", "#dae7e3", "#bee1e6", "#c6def2", "#cddafd"])
    , ("French Tapestry", ["#a3886b", "#dead9c", "#ebdac6", "#9fb6ae", "#fedcaf"])
    , ("Earthy Rainbow", ["#dfd1e0", "#bbdee4", "#c1dad7", "#edddc6", "#dead9c"])
    , ("Wildflower", ["#d4ace0", "#d78393", "#f2aead", "#f7e9de", "#cf89ae"])
    ]

printHeader :: String -> IO ()
printHeader header = do
    let lineLength = length header + 8
        line = replicate lineLength '='

    setSGR [SetColor Foreground Dull Blue]
    putStrLn line
    putStrLn (replicate 4 ' ' <> header <> replicate 4 ' ')
    putStrLn line
    setSGR [Reset]

printError :: String -> IO()
printError message = do
    setSGR [SetColor Foreground Dull Red]
    putStr message
    setSGR [Reset]

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

printUnavailable :: String -> IO ()
printUnavailable message = do
    setSGR [SetColor Foreground Vivid Black]
    putStrLn message
    setSGR [Reset]
