module Interpreter
  ( processScript
  ) where

import Parser (assignmentParser)
import Text.Megaparsec (runParser)

processScript :: String -> [String] -> IO ()
processScript script _ = do
  let s = runParser assignmentParser "" script
  print s
