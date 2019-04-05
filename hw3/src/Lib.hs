module Lib
  ( processScript
  ) where

import Data.Char (isSpace)
import Text.Megaparsec

processScript :: String -> [String] -> IO ()
processScript script _ = do
  let s = runParser commandsParser "" script
  print s

data ParsingError =
  ParsingError
  deriving (Show, Eq, Ord)

variableParser :: Parsec ParsingError String Variable
variableParser =
  Variable <$> do
    firstLetter <- oneOf variableNameSymbolsList
    otherLetters <- variableParserImpl
    return $ firstLetter : otherLetters
  where
    variableParserImpl :: Parsec ParsingError String String
    variableParserImpl = nonEmptyVariableParserImpl <|> return ""
    nonEmptyVariableParserImpl :: Parsec ParsingError String String
    nonEmptyVariableParserImpl = do
      firstLetter <- oneOf variableNameSymbolsList
      tailLetters <- variableParserImpl
      return $ firstLetter : tailLetters
    variableNameSymbolsList :: [Char]
    variableNameSymbolsList = '_' : ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']

singleQuotesParser :: Parsec ParsingError String String
singleQuotesParser = do
  _ <- single '\''
  inner <- singleQuotesInnerParser
  _ <- single '\''
  return inner
  where
    singleQuotesInnerParser :: Parsec ParsingError String String
    singleQuotesInnerParser = (pure (:) <*> satisfy (/= '\'') <*> singleQuotesInnerParser) <|> return []

newtype Variable =
  Variable String
  deriving (Show)

data AssignmentRight
  = SingleQuotes String
  | VariableName Variable
  deriving (Show)

assignedVariableParser :: Parsec ParsingError String Variable
assignedVariableParser = do
  _ <- single '$'
  variableParser

assignmentRightParser :: Parsec ParsingError String AssignmentRight
assignmentRightParser = (SingleQuotes <$> singleQuotesParser) <|> (VariableName <$> assignedVariableParser)

data Assignment =
  Assignment Variable
             AssignmentRight
  deriving (Show)

assignmentParser :: Parsec ParsingError String Assignment
assignmentParser = do
  leftVar <- variableParser
  _ <- single '='
  Assignment leftVar <$> assignmentRightParser

commandEndParser :: Parsec ParsingError String ()
commandEndParser = do
  _ <- oneOf ['\n', ';']
  whitespaceSkipper
  where
    whitespaceSkipper :: Parsec ParsingError String ()
    whitespaceSkipper = oneOrMoreSpacesSkipper <|> return ()
    oneOrMoreSpacesSkipper :: Parsec ParsingError String ()
    oneOrMoreSpacesSkipper = do
      _ <- satisfy isSpace
      whitespaceSkipper

data Command =
  AssignmentCommand Assignment
  deriving (Show)

commandParser :: Parsec ParsingError String Command
commandParser = do
  command <- AssignmentCommand <$> assignmentParser
  commandEndParser
  return command

commandsParser :: Parsec ParsingError String [Command]
commandsParser = nonEmptyCommandsParser <|> return []
  where
    nonEmptyCommandsParser :: Parsec ParsingError String [Command]
    nonEmptyCommandsParser = do
      firstCommand <- commandParser
      otherCommands <- commandsParser
      return $ firstCommand : otherCommands
