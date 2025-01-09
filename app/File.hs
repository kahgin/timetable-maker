module File where

import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as B
import System.IO (writeFile, readFile)

-- Save timetable to file
saveTimetable :: Timetable -> IO ()
saveTimetable timetable = do
    let json = encode timetable
    writeFile "timetable.json" json

-- Load timetable from file
loadTimetable :: IO (Maybe Timetable)
loadTimetable = do
    json <- readFile "timetable.json"
    return $ decode json
