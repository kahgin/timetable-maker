
module File where

import Timetable
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as B
{-
-- Save timetable to file
saveTimetable :: Timetable -> IO ()
saveTimetable timetable = do
    let json = encode timetable
    B.writeFile "timetable.json" json

-- Load timetable from file
loadTimetable :: IO (Maybe Timetable)
loadTimetable = do
    json <- B.readFile "timetable.json"
    return $ decode json
-}