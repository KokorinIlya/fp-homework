module Main where

import System.Environment (getArgs)
import System.IO (IOMode (..), hGetContents, openFile)

import Interpreter (parseAndProcessScript)

main :: IO ()
main = do
  programArguments <- getArgs
  case programArguments of
    [] -> putStrLn "Expected name of the file with script"
    scriptFileName:_ -> do
      scriptFile <- openFile scriptFileName ReadMode
      script <- hGetContents scriptFile
      parseAndProcessScript script programArguments
