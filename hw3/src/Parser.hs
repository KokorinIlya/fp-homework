module Parser
  ( Parser
  , identifierParser
  , variableParser
  , singleQuotesParser
  , concatenateCharAndString
  , concatenateStrings
  , doubleQuotesParser
  , implicitQuotesParser
  , assignmentParser
  ) where

import Data.Char (isSpace)
import Data.Functor ((<$))
import Data.List.NonEmpty (NonEmpty (..))
import ProgramStructure (Assignment (..), FragmentsList (..), Identifier (..), SingleQuotes (..),
                         Variable (..))
import Text.Megaparsec (Parsec, anySingle, many, oneOf, satisfy, single, try, (<|>))
import Text.Megaparsec.Char (digitChar)
import Text.Megaparsec.Char.Lexer (decimal)

data ParsingError =
  ParsingError
  deriving (Show, Eq, Ord)

type Parser = Parsec ParsingError String

identifierParser :: Parser Identifier
identifierParser = Identifier <$> ((:|) <$> oneOf ('_' : ['a' .. 'z'] ++ ['A' .. 'Z']) <*> identifierEndParser)
  where
    identifierEndParser :: Parser String
    identifierEndParser = many $ try (oneOf ('_' : ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']))

variableParser :: Parser Variable
variableParser = try shortNumberParser <|> try longNumberParser <|> try (IdentifiedVariable <$> identifierParser)
  where
    longNumberParser :: Parser Variable
    longNumberParser = ScriptArgument <$> (single '{' *> decimal <* single '}')
    shortNumberParser :: Parser Variable
    shortNumberParser = ScriptArgument . read . (: []) <$> digitChar

singleQuotesParser :: Parser SingleQuotes
singleQuotesParser = single '\'' *> (SingleQuotes <$> singleQuotesInnerParser) <* single '\''
  where
    singleQuotesInnerParser :: Parser String
    singleQuotesInnerParser = many $ try (satisfy (/= '\''))

concatenateCharAndString :: Char -> Parser FragmentsList -> Parser FragmentsList
concatenateCharAndString symbol remainderParser = do
  remainder <- remainderParser
  return $
    case remainder of
      SimpleString s r -> SimpleString (symbol : s) r
      _                -> SimpleString [symbol] remainder

concatenateStrings :: String -> Parser FragmentsList -> Parser FragmentsList
concatenateStrings cs remainderParser = do
  remainder <- remainderParser
  return $
    case remainder of
      SimpleString s r -> SimpleString (cs ++ s) r
      _                -> SimpleString cs remainder

doubleQuotesParser :: Parser FragmentsList
doubleQuotesParser = single '\"' *> doubleQuotesInnerParser
  where
    doubleQuotesInnerParser :: Parser FragmentsList
    doubleQuotesInnerParser = try escapeParser <|> try variableRefParser <|> try symbolParser <|> try parseEnd
    escapeParser :: Parser FragmentsList
    escapeParser = do
      c <- single '\\' *> (try (single '\\') <|> try (single '$') <|> try (single '"'))
      concatenateCharAndString c doubleQuotesInnerParser
    variableRefParser :: Parser FragmentsList
    variableRefParser = single '$' *> (VariableRef <$> variableParser <*> doubleQuotesInnerParser)
    symbolParser :: Parser FragmentsList
    symbolParser = do
      c <- satisfy (/= '\"')
      concatenateCharAndString c doubleQuotesInnerParser
    parseEnd :: Parser FragmentsList
    parseEnd = End <$ single '\"'

implicitQuotesParser :: Parser FragmentsList
implicitQuotesParser =
  try escapeParser <|> try innerSingleQuotesParser <|> try innerDoubleQuotesParser <|> try innerExpressionParser <|>
  try charParser
  where
    escapeParser :: Parser FragmentsList
    escapeParser = do
      c <- single '\\' *> anySingle
      concatenateCharAndString c implicitQuotesParser
    innerSingleQuotesParser :: Parser FragmentsList
    innerSingleQuotesParser = do
      (SingleQuotes str) <- singleQuotesParser
      concatenateStrings str implicitQuotesParser
    innerDoubleQuotesParser :: Parser FragmentsList
    innerDoubleQuotesParser = (<>) <$> doubleQuotesParser <*> implicitQuotesParser
    innerExpressionParser :: Parser FragmentsList
    innerExpressionParser = single '$' *> (VariableRef <$> variableParser <*> implicitQuotesParser)
    charParser :: Parser FragmentsList
    charParser = do
      curChar <- anySingle
      if curChar == '(' || curChar == ')' || isSpace curChar || curChar == ';'
        then return End
        else concatenateCharAndString curChar implicitQuotesParser

assignmentParser :: Parser Assignment
assignmentParser = do
  identifier <- identifierParser
  _ <- single '='
  Assignment identifier <$> implicitQuotesParser
