module Lib
  ( processScript
  ) where

import Data.Char (isDigit, isSpace)
import Data.Functor ((<$))
import Data.List.NonEmpty (NonEmpty (..))
import Text.Megaparsec

processScript :: String -> [String] -> IO ()
processScript script _ = do
  let s = runParser commandsParser "" script
  print s

data ParsingError =
  ParsingError
  deriving (Show, Eq, Ord)

variableParser :: Parsec ParsingError String Variable
variableParser = Variable <$> nonEmptyVariableParserImpl
  where
    variableParserImpl :: Parsec ParsingError String String
    variableParserImpl = nonEmptyVariableParserImpl <|> return ""
    nonEmptyVariableParserImpl :: Parsec ParsingError String String
    nonEmptyVariableParserImpl = (:) <$> oneOf variableNameSymbolsList <*> variableParserImpl
    variableNameSymbolsList :: [Char]
    variableNameSymbolsList = '_' : ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']

singleQuotesParser :: Parsec ParsingError String String
singleQuotesParser = single '\'' *> singleQuotesInnerParser <* single '\''
  where
    singleQuotesInnerParser :: Parsec ParsingError String String
    singleQuotesInnerParser = (pure (:) <*> satisfy (/= '\'') <*> singleQuotesInnerParser) <|> return []

newtype Variable =
  Variable String
  deriving (Show)

data AssignmentRight
  = SingleQuotes String
  | VariableName Variable
  | DoubleQuotes DoubleQuotesInner
  deriving (Show)

assignedVariableParser :: Parsec ParsingError String Variable
assignedVariableParser = single '$' *> variableParser

assignmentRightParser :: Parsec ParsingError String AssignmentRight
assignmentRightParser =
  (SingleQuotes <$> singleQuotesParser) <|> (VariableName <$> assignedVariableParser) <|>
  (DoubleQuotes <$> doubleQuotesParser) <|>
  (SingleQuotes <$> implicitSingleQuotesParser)
  where
    implicitSingleQuotesParser :: Parsec ParsingError String String
    implicitSingleQuotesParser = nonEmptyImplicitSingleQuotesParser <|> return ""
    nonEmptyImplicitSingleQuotesParser :: Parsec ParsingError String String
    nonEmptyImplicitSingleQuotesParser =
      (:) <$> satisfy (\c -> not (isSpace c || c == ';')) <*> implicitSingleQuotesParser

data Assignment =
  Assignment Variable
             AssignmentRight
  deriving (Show)

assignmentParser :: Parsec ParsingError String Assignment
assignmentParser = do
  leftVar <- variableParser
  _ <- single '='
  Assignment leftVar <$> assignmentRightParser

newtype InnerRead =
  InnerRead (NonEmpty Variable)
  deriving (Show)

readParser :: Parsec ParsingError String InnerRead
readParser = chunk "read" *> (InnerRead <$> oneOrMoreArgumentsParser)
  where
    oneOrMoreArgumentsParser :: Parsec ParsingError String (NonEmpty Variable)
    oneOrMoreArgumentsParser = oneOrMoreSpacesSkipper *> ((:|) <$> variableParser <*> readArgumentsParser)
    readArgumentsParser :: Parsec ParsingError String [Variable]
    readArgumentsParser = (oneOrMoreSpacesSkipper *> ((:) <$> variableParser <*> readArgumentsParser)) <|> return []
    whitespaceSkipper :: Parsec ParsingError String ()
    whitespaceSkipper = oneOrMoreSpacesSkipper <|> return ()
    oneOrMoreSpacesSkipper :: Parsec ParsingError String ()
    oneOrMoreSpacesSkipper = satisfy (\c -> isSpace c && c /= '\n') *> whitespaceSkipper

data InnerEcho = InnerEcho
  { needNewline :: Bool
  , toPrint     :: NonEmpty AssignmentRight
  } deriving (Show)

echoParser :: Parsec ParsingError String InnerEcho
echoParser = chunk "echo" *> oneOrMoreSpacesSkipper *> (InnerEcho <$> parseKey <*> oneOrMoreArgumentsParser)
  where
    parseKey :: Parsec ParsingError String Bool
    parseKey = (False <$ chunk "-n") <|> return True
    oneOrMoreArgumentsParser :: Parsec ParsingError String (NonEmpty AssignmentRight)
    oneOrMoreArgumentsParser = (:|) <$> assignmentRightParser <*> echoArgumentsParser
    echoArgumentsParser :: Parsec ParsingError String [AssignmentRight]
    echoArgumentsParser =
      (oneOrMoreSpacesSkipper *> ((:) <$> assignmentRightParser <*> echoArgumentsParser)) <|> return []
    whitespaceSkipper :: Parsec ParsingError String ()
    whitespaceSkipper = oneOrMoreSpacesSkipper <|> return ()
    oneOrMoreSpacesSkipper :: Parsec ParsingError String ()
    oneOrMoreSpacesSkipper = satisfy (\c -> isSpace c && c /= '\n') *> whitespaceSkipper

newtype InnerExit =
  InnerExit Int
  deriving (Show)

data Sign
  = Plus
  | Minus

exitParser :: Parsec ParsingError String InnerExit
exitParser = do
  _ <- chunk "exit"
  oneOrMoreSpacesSkipper
  InnerExit <$> numberParser
  where
    whitespaceSkipper :: Parsec ParsingError String ()
    whitespaceSkipper = oneOrMoreSpacesSkipper <|> return ()
    oneOrMoreSpacesSkipper :: Parsec ParsingError String ()
    oneOrMoreSpacesSkipper = satisfy (\c -> isSpace c && c /= '\n') *> whitespaceSkipper
    numberParser :: Parsec ParsingError String Int
    numberParser = unsignedNumberParser <|> signedNumberParser
    signedNumberParser :: Parsec ParsingError String Int
    signedNumberParser = do
      sign <- signParser
      number <- unsignedNumberParser
      return $
        case sign of
          Plus  -> number
          Minus -> (-1) * number
    signParser :: Parsec ParsingError String Sign
    signParser = Minus <$ single '-' <|> Plus <$ single '+'
    unsignedNumberParser :: Parsec ParsingError String Int
    unsignedNumberParser = fmap fst unsignedNumberParserImpl
    unsignedNumberParserImpl :: Parsec ParsingError String (Int, Int)
    unsignedNumberParserImpl = do
      curChar <- satisfy isDigit
      let curNum = charToNum curChar
      (numberTail, numberTailPow) <- unsignedNumberParserImpl <|> return (0, 1)
      return (curNum * numberTailPow + numberTail, 10 * numberTailPow)
    charToNum :: Char -> Int
    charToNum '0' = 0
    charToNum '1' = 1
    charToNum '2' = 2
    charToNum '3' = 3
    charToNum '4' = 4
    charToNum '5' = 5
    charToNum '6' = 6
    charToNum '7' = 7
    charToNum '8' = 8
    charToNum _   = 9

newtype InnerCd =
  InnerCd AssignmentRight
  deriving (Show)

cdParser :: Parsec ParsingError String InnerCd
cdParser = chunk "cd" *> oneOrMoreSpacesSkipper *> (InnerCd <$> assignmentRightParser)
  where
    whitespaceSkipper :: Parsec ParsingError String ()
    whitespaceSkipper = oneOrMoreSpacesSkipper <|> return ()
    oneOrMoreSpacesSkipper :: Parsec ParsingError String ()
    oneOrMoreSpacesSkipper = satisfy (\c -> isSpace c && c /= '\n') *> whitespaceSkipper

commandEndParser :: Parsec ParsingError String ()
commandEndParser = nonTerminatingWhitespaceSkipper *> oneOf ['\n', ';'] *> whitespaceSkipper
  where
    whitespaceSkipper :: Parsec ParsingError String ()
    whitespaceSkipper = oneOrMoreSpacesSkipper <|> return ()
    oneOrMoreSpacesSkipper :: Parsec ParsingError String ()
    oneOrMoreSpacesSkipper = satisfy isSpace *> whitespaceSkipper
    nonTerminatingWhitespaceSkipper :: Parsec ParsingError String ()
    nonTerminatingWhitespaceSkipper = oneOrMoreNonTerminatingSpacesSkipper <|> return ()
    oneOrMoreNonTerminatingSpacesSkipper :: Parsec ParsingError String ()
    oneOrMoreNonTerminatingSpacesSkipper = satisfy (\c -> isSpace c && c /= '\n') *> nonTerminatingWhitespaceSkipper

data Command
  = AssignmentCommand Assignment
  | ReadCommand InnerRead
  | EchoCommand InnerEcho
  | PwdCommand
  | ExitCommand InnerExit
  | CdCommand InnerCd
  deriving (Show)

commandParser :: Parsec ParsingError String Command
commandParser =
  ((EchoCommand <$> echoParser) <|> (ReadCommand <$> readParser) <|> (PwdCommand <$ chunk "pwd") <|>
   (ExitCommand <$> exitParser) <|>
   (CdCommand <$> cdParser) <|>
   (AssignmentCommand <$> assignmentParser)) <*
  commandEndParser

commandsParser :: Parsec ParsingError String [Command]
commandsParser = nonEmptyCommandsParser <|> return []
  where
    nonEmptyCommandsParser :: Parsec ParsingError String [Command]
    nonEmptyCommandsParser = (:) <$> commandParser <*> commandsParser

data DoubleQuotesInner
  = End
  | SimpleString String
                 DoubleQuotesInner
  | VariableRef Variable
                DoubleQuotesInner
  deriving (Show)

doubleQuotesParser :: Parsec ParsingError String DoubleQuotesInner
doubleQuotesParser = single '"' *> doubleQuotesInnerParser <* single '"'
  where
    doubleQuotesInnerParser :: Parsec ParsingError String DoubleQuotesInner
    doubleQuotesInnerParser = parseNonEmptyInner <|> return End
    parseNonEmptyInner :: Parsec ParsingError String DoubleQuotesInner
    parseNonEmptyInner = do
      curSymbol <- satisfy (/= '"')
      case curSymbol of
        '$' -> do
          inner <- (VariableRef <$> variableParser) <|> return (SimpleString "$")
          remainder <- doubleQuotesInnerParser
          return $
            case inner remainder of
              SimpleString s1 (SimpleString s2 e) -> SimpleString (s1 ++ s2) e
              x                                   -> x
        _ -> do
          doubleQuotInnerRemainder <- doubleQuotesInnerParser
          return $
            case doubleQuotInnerRemainder of
              SimpleString s remainder -> SimpleString (curSymbol : s) remainder
              notSimpleString          -> SimpleString [curSymbol] notSimpleString
