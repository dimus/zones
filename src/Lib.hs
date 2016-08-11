module Lib
    ( ui
    , output
    ) where

import qualified Data.Map as M

ui :: IO ()
ui = do
  putStrLn "Enter max HR"
  -- input <- getLine
  putStrLn $ output "180"
  -- input

output :: String -> String
output input = ("Zones for max HR " ++ show hr ++ ".\n\n"
             ++ "1. Active Recovery:   HR (" ++ zoneHr 1 ++ ")\n"
             ++ "2. Endurance:         HR (" ++ zoneHr 2 ++ ")\n"
             ++ "3. Tempo:             HR (" ++ zoneHr 3 ++ ")\n"
             ++ "4. Lactate Threshold: HR (" ++ zoneHr 4 ++ ")\n"
             ++ "5. VO2:               HR (" ++ zoneHr 5 ++ ")\n")
  where
    hr = read input :: Float
    zoneHr = zone hr

zone :: Float -> Float -> String
zone h zoneNum =
  show (round $ h * (res !! 0)) ++ "-" ++ show(round $ h * (res !! 1))
  where
    val = M.lookup zoneNum zones
    res = case val of
      Nothing -> error "Something is wrong"
      Just a -> a

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

