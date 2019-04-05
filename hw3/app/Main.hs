module Main where

import System.Environment (getArgs)
import System.IO (IOMode(..), hGetContents, openFile)

import Lib (processScript)

main :: IO ()
main = do
  programArguments <- getArgs
  case programArguments of
    [] -> putStrLn "Expected file with script"
    scriptFileName:scriptArguments -> do
      scriptFile <- openFile scriptFileName ReadMode
      script <- hGetContents scriptFile
      processScript script scriptArguments
