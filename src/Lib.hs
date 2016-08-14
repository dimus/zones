module Lib
    ( ui
    , output
    , zone
    ) where

import qualified Data.Map as M

ui :: IO ()
ui = do
  putStrLn "Enter max HR"
  input <- getLine
  let hr = read input :: Float
  putStrLn $ output hr

output :: Float -> String
output hr = ("Zones for max HR " ++ show hr ++ ".\n\n"
             ++ "1. Active Recovery:   HR (" ++ zoneHr 1 ++ ")\n"
             ++ "2. Endurance:         HR (" ++ zoneHr 2 ++ ")\n"
             ++ "3. Tempo:             HR (" ++ zoneHr 3 ++ ")\n"
             ++ "4. Lactate Threshold: HR (" ++ zoneHr 4 ++ ")\n"
             ++ "5. VO2:               HR (" ++ zoneHr 5 ++ ")\n")
  where
    zoneHr = zone hr

zone :: Float -> Float -> String
zone hr zoneNum =
  min ++ "-" ++ max
  where
    val = M.lookup zoneNum zones
    res = case val of
      Nothing -> error "Something is wrong"
      Just a -> a
    f x = round $ hr * ((!!) res x)
    min = show $ f 0
    max = show $ f 1

zones :: M.Map Float [Float]
zones = foldr (\(x:xs) -> M.insert x xs) M.empty zonesList

zonesList :: [[Float]]
zonesList = [
               [1, 0.50, 0.68]
             , [2, 0.69, 0.83]
             , [3, 0.84, 0.94]
             , [4, 0.95, 1.05]
             , [5, 1.06, 2]
             ]

