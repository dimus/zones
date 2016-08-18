module Lib
    ( ui
    , output
    , zone
    , formatZone
    ) where

import qualified Data.Map as M
import System.Environment

ui :: IO ()
ui = do
  (input:_) <- getArgs
  let hr = read input :: Int
  putStrLn $ output hr

output :: Int -> String
output hr = ("Zones for max HR " ++ show hr ++ ".\n\n"
             ++ "1. Active Recovery:   HR (" ++ fzone 1 ++ ")\n"
             ++ "2. Endurance:         HR (" ++ fzone 2 ++ ")\n"
             ++ "3. Tempo:             HR (" ++ fzone 3 ++ ")\n"
             ++ "4. Lactate Threshold: HR (" ++ fzone 4 ++ ")\n"
             ++ "5. VO2:               HR (" ++ fzone 5 ++ ")\n")
  where
    fzone = formatZone hr


formatZone :: Int -> Int -> String
formatZone hr zoneNum =
  let (min, max) = zone hr zoneNum
  in
    case zoneNum of
      1 -> "<= " ++ show max
      5 -> "> " ++ show min
      _ -> show min ++ "-" ++ show max

zone :: Int -> Int -> (Int, Int)
zone hr zoneNum
  | (zoneNum < 1) || (zoneNum > 5) = error "Unsupported zone number"
  | otherwise = (calcZone min + 1, calcZone max)
  where
    zoneList = [0, 0.68, 0.83, 0.94, 1.05, 0]
    min = zoneList !! (zoneNum -1)
    max =  zoneList !! zoneNum
    calcZone a = round $ fromIntegral hr * a


