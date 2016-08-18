module Main where

  import Lib
  import Control.Exception(catch)
  import System.Environment(getProgName)
  import System.IO.Error(isUserError)

  main :: IO ()
  main = ui `catch` handler

  handler :: IOError -> IO ()
  handler e
    | isUserError e = do
      progName <- getProgName
      putStrLn ("Usage: " ++ progName ++ " 180")
    | otherwise = ioError e
