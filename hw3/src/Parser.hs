module Parser
  ( Parser
  , identifierParser
  , variableParser
  , singleQuotesParser
  , doubleQuotesParser
  , implicitQuotesParser
  , assignmentParser
  , shellCommandParser
  , commandParser
  , nonEmptyCommandSequenceParser
  , whileParser
  , commandSequenceParser
  , ifParser
  , programParser
  ) where

import Data.Char (isSpace)
import Data.Functor ((<$))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Void (Void)
import ProgramStructure (Assignment (..), Command (..), DoubleQuotesInner (..), ElseIf (..),
                         Identifier (..), If (..), ImplicitQuotesInner (..), ShellCommand (..),
                         SingleQuotes (..), Variable (..), While (..))
import Text.Megaparsec (Parsec, anySingle, empty, eof, many, notFollowedBy, oneOf, satisfy, single,
                        some, try, (<|>))
import Text.Megaparsec.Char (digitChar, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

identifierParser :: Parser Identifier
identifierParser = Identifier <$> ((:|) <$> oneOf ('_' : ['a' .. 'z'] ++ ['A' .. 'Z']) <*> identifierEndParser)
  where
    identifierEndParser :: Parser String
    identifierEndParser = many $ try (oneOf ('_' : ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']))

variableParser :: Parser Variable
variableParser =
  try shortNumberParser <|> try longNumberParser <|> try (IdentifiedVariable <$> identifierParser) <|>
  try inlineCallParser
  where
    longNumberParser :: Parser Variable
    longNumberParser = ScriptArgument <$> (single '{' *> decimal <* single '}')
    shortNumberParser :: Parser Variable
    shortNumberParser = ScriptArgument . read . (: []) <$> digitChar
    inlineCallParser :: Parser Variable
    inlineCallParser =
      single '(' *> many (try (satisfy isSpace)) *> (InlineCall <$> commandSequenceParser [single ')']) <* single ')'

singleQuotesParser :: Parser SingleQuotes
singleQuotesParser = single '\'' *> (SingleQuotes <$> singleQuotesInnerParser) <* single '\''
  where
    singleQuotesInnerParser :: Parser String
    singleQuotesInnerParser = many $ try (satisfy (/= '\''))

doubleQuotesParser :: Parser DoubleQuotesInner
doubleQuotesParser = single '\"' *> doubleQuotesInnerParser
  where
    doubleQuotesInnerParser :: Parser DoubleQuotesInner
    doubleQuotesInnerParser = try escapeParser <|> try variableRefParser <|> try symbolParser <|> try parseEnd
    escapeParser :: Parser DoubleQuotesInner
    escapeParser = do
      c <- single '\\' *> (try (single '\\') <|> try (single '$') <|> try (single '"'))
      concatenateCharAndString c
    variableRefParser :: Parser DoubleQuotesInner
    variableRefParser = single '$' *> (DoubleQuotesVariableRef <$> variableParser <*> doubleQuotesInnerParser)
    symbolParser :: Parser DoubleQuotesInner
    symbolParser = satisfy (/= '\"') >>= concatenateCharAndString
    parseEnd :: Parser DoubleQuotesInner
    parseEnd = DoubleQuotesEnd <$ single '\"'
    concatenateCharAndString :: Char -> Parser DoubleQuotesInner
    concatenateCharAndString symbol = do
      remainder <- doubleQuotesInnerParser
      return $
        case remainder of
          DoubleQuotesSimpleString s r -> DoubleQuotesSimpleString (symbol : s) r
          _                            -> DoubleQuotesSimpleString [symbol] remainder

implicitQuotesParser :: Parser ImplicitQuotesInner
implicitQuotesParser =
  try escapeParser <|> try innerSingleQuotesParser <|> try innerDoubleQuotesParser <|> try innerExpressionParser <|>
  try charParser
  where
    implicitQuotesParserImpl :: Parser ImplicitQuotesInner
    implicitQuotesParserImpl =
      try escapeParser <|> try innerSingleQuotesParser <|> try innerDoubleQuotesParser <|> try innerExpressionParser <|>
      try charParser <|>
      return ImplicitQuotesEnd
    escapeParser :: Parser ImplicitQuotesInner
    escapeParser = do
      c <- single '\\' *> anySingle
      concatenateCharAndString c
    innerSingleQuotesParser :: Parser ImplicitQuotesInner
    innerSingleQuotesParser = do
      (SingleQuotes str) <- singleQuotesParser
      concatenateStrings str
    innerDoubleQuotesParser :: Parser ImplicitQuotesInner
    innerDoubleQuotesParser = ImplicitQuotesDoubleQuotes <$> doubleQuotesParser <*> implicitQuotesParserImpl
    innerExpressionParser :: Parser ImplicitQuotesInner
    innerExpressionParser = single '$' *> (ImplicitQuotesVariableRef <$> variableParser <*> implicitQuotesParserImpl)
    charParser :: Parser ImplicitQuotesInner
    charParser = do
      notFollowedBy (satisfy (\c -> c == '(' || c == ')' || isSpace c || c == ';'))
      curChar <- anySingle
      concatenateCharAndString curChar
    concatenateCharAndString :: Char -> Parser ImplicitQuotesInner
    concatenateCharAndString symbol = do
      remainder <- implicitQuotesParserImpl
      return $
        case remainder of
          ImplicitQuotesSimpleString s r -> ImplicitQuotesSimpleString (symbol : s) r
          _                              -> ImplicitQuotesSimpleString [symbol] remainder
    concatenateStrings :: String -> Parser ImplicitQuotesInner
    concatenateStrings cs = do
      remainder <- implicitQuotesParserImpl
      return $
        case remainder of
          ImplicitQuotesSimpleString s r -> ImplicitQuotesSimpleString (cs ++ s) r
          _                              -> ImplicitQuotesSimpleString cs remainder

assignmentParser :: Parser Assignment
assignmentParser = do
  identifier <- identifierParser
  _ <- single '='
  Assignment identifier <$> implicitQuotesParser

shellCommandParser :: Parser ShellCommand
shellCommandParser = ShellCommand <$> ((:|) <$> implicitQuotesParser <*> many (try commandArgumentsParser))
  where
    commandArgumentsParser :: Parser ImplicitQuotesInner
    commandArgumentsParser = spaceSkipper *> implicitQuotesParser
    spaceSkipper :: Parser ()
    spaceSkipper = () <$ some (try $ satisfy (\c -> isSpace c && c /= '\n'))

oneOfParsers :: [Parser a] -> Parser a
oneOfParsers = foldr ((<|>) . try) empty

commandParser :: [Parser a] -> Parser Command
commandParser ps =
  notFollowedBy (oneOfParsers ps) *>
  (try (AssignmentCommand <$> assignmentParser) <|> try (WhileCommand <$> whileParser) <|> try (IfCommand <$> ifParser) <|>
   try (CallCommand <$> shellCommandParser))

nonEmptyCommandSequenceParser :: [Parser a] -> Parser (NonEmpty Command)
nonEmptyCommandSequenceParser ps = (:|) <$> commandParser ps <*> many (try commandListParser) <* commandDelimeterParser
  where
    commandListParser :: Parser Command
    commandListParser = commandDelimeterParser *> commandParser ps
    spaceSkipper :: (Char -> Bool) -> Parser ()
    spaceSkipper predicate = () <$ many (try $ satisfy predicate)
    commandDelimeterParser :: Parser ()
    commandDelimeterParser =
      () <$ (spaceSkipper (\c -> isSpace c && c /= '\n') *> oneOf [';', '\n'] *> spaceSkipper isSpace)

commandSequenceParser :: [Parser a] -> Parser [Command]
commandSequenceParser ps = many (try commandListParser)
  where
    commandListParser :: Parser Command
    commandListParser = commandParser ps <* commandDelimeterParser
    spaceSkipper predicate = () <$ many (try $ satisfy predicate)
    commandDelimeterParser :: Parser ()
    commandDelimeterParser =
      () <$ (spaceSkipper (\c -> isSpace c && c /= '\n') *> oneOf [';', '\n'] *> spaceSkipper isSpace)

whileParser :: Parser While
whileParser = do
  _ <- string "while"
  oneOrMoreSpaceSkipper
  conds <- nonEmptyCommandSequenceParser [string "do" *> satisfy isSpace]
  _ <- string "do"
  oneOrMoreSpaceSkipper
  acts <- commandSequenceParser [string "done" *> satisfy isSpace <|> string "done" *> single ';']
  _ <- string "done"
  return $ While conds acts
  where
    oneOrMoreSpaceSkipper :: Parser ()
    oneOrMoreSpaceSkipper = () <$ some (try $ satisfy isSpace)

ifParser :: Parser If
ifParser = do
  _ <- string "if"
  oneOrMoreSpaceSkipper
  conds <- nonEmptyCommandSequenceParser [string "then" *> satisfy isSpace]
  _ <- string "then"
  oneOrMoreSpaceSkipper
  acts <-
    commandSequenceParser
      [ string "else" *> satisfy isSpace
      , string "elif" *> satisfy isSpace
      , string "fi" *> single ';'
      , string "fi" *> satisfy isSpace
      ]
  elifs <- elifsParser
  parsedElse <- Just <$> elseParser <|> return Nothing
  _ <- string "fi"
  return $ If conds acts elifs parsedElse
  where
    oneOrMoreSpaceSkipper :: Parser ()
    oneOrMoreSpaceSkipper = () <$ some (try $ satisfy isSpace)
    elifsParser :: Parser [ElseIf]
    elifsParser = many (try singleElifParser)
    singleElifParser :: Parser ElseIf
    singleElifParser = do
      _ <- string "elif"
      oneOrMoreSpaceSkipper
      conds <- nonEmptyCommandSequenceParser [string "then" *> satisfy isSpace]
      _ <- string "then"
      oneOrMoreSpaceSkipper
      acts <-
        commandSequenceParser
          [ string "else" *> satisfy isSpace
          , string "elif" *> satisfy isSpace
          , string "fi" *> single ';'
          , string "fi" *> satisfy isSpace
          ]
      return $ ElseIf conds acts
    elseParser :: Parser [Command]
    elseParser = do
      _ <- string "else"
      oneOrMoreSpaceSkipper
      commandSequenceParser [string "fi" *> single ';', string "fi" *> satisfy isSpace]

programParser :: Parser [Command]
programParser = many (try (satisfy isSpace)) *> commandSequenceParser [] <* eof
