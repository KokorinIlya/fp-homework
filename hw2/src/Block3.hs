{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Block3
  ( CorrectBracketSequence(..)
  , Sign
  , Parser(..)
  , correctBracketSequenceParser
  , element
  , eof
  , numbersListParser
  , numbersListsParser
  , numberParser
  , ok
  , satisfy
  , stream
  ) where

import Control.Applicative (Alternative (..))
import Data.Char (isDigit, isSpace)
import Data.Functor ((<$))
import Utils (mapFirst)

newtype Parser s a = Parser
  { runParser :: [s] -> Maybe (a, [s])
  }

instance Functor (Parser s) where
  fmap f (Parser parsingFunction) = Parser $ \input -> fmap (mapFirst f) (parsingFunction input)

instance Applicative (Parser s) where
  pure result = Parser $ \input -> Just (result, input)
  (Parser pf) <*> (Parser pa) =
    Parser $ \input ->
      case pf input of
        Nothing -> Nothing
        Just (f, remainder) ->
          case pa remainder of
            Nothing              -> Nothing
            Just (a, streamTail) -> Just (f a, streamTail)

instance Alternative (Parser s) where
  empty = Parser $ const Nothing
  Parser pa <|> Parser pb = Parser $ \input -> pa input <|> pb input

instance Monad (Parser s) where
  Parser pa >>= f =
    Parser $ \input -> do
      (result, remainder) <- pa input
      let (Parser pb) = f result
      pb remainder

ok :: Parser s ()
ok = Parser $ \input -> Just ((), input)

eof :: forall s. Parser s ()
eof = Parser checkStreamIsEmpty
  where
    checkStreamIsEmpty :: [s] -> Maybe ((), [s])
    checkStreamIsEmpty [] = Just ((), [])
    checkStreamIsEmpty _  = Nothing

satisfy :: forall s. (s -> Bool) -> Parser s s
satisfy predicate = Parser checkSymbol
  where
    checkSymbol :: [s] -> Maybe (s, [s])
    checkSymbol [] = Nothing
    checkSymbol (x:xs)
      | predicate x = Just (x, xs)
      | otherwise = Nothing

element :: Eq s => s -> Parser s s
element el = satisfy (== el)

stream ::
     forall a. (Eq a)
  => [a]
  -> Parser a [a]
stream streamToParse = Parser $ \input -> fmap (streamToParse, ) (cutIfPrefix streamToParse input)
  where
    cutIfPrefix :: [a] -> [a] -> Maybe [a]
    cutIfPrefix [] [] = Just []
    cutIfPrefix [] second@(_:_) = Just second
    cutIfPrefix (_:_) [] = Nothing
    cutIfPrefix (x:xs) (y:ys)
      | x == y = cutIfPrefix xs ys
      | otherwise = Nothing

data CorrectBracketSequence
  = Empty -- empty
  | Inner CorrectBracketSequence -- (s)
  | Concatenation CorrectBracketSequence
                  CorrectBracketSequence -- s1 s2

instance Show CorrectBracketSequence where
  show Empty               = ""
  show (Inner inner)       = '(' : (show inner ++ ")")
  show (Concatenation a b) = show a ++ show b

correctBracketSequenceParser :: Parser Char CorrectBracketSequence
correctBracketSequenceParser = bracketParser <* eof
  where
    bracketParser :: Parser Char CorrectBracketSequence
    bracketParser = nonEmptyParser <|> emptyParser
    nonEmptyParser :: Parser Char CorrectBracketSequence
    nonEmptyParser = Concatenation <$> innerParser <*> bracketParser
    innerParser :: Parser Char CorrectBracketSequence
    innerParser = fmap Inner (element '(' *> bracketParser <* element ')')
    emptyParser :: Parser Char CorrectBracketSequence
    emptyParser = Empty <$ ok

data Sign
  = Plus
  | Minus

numberParser ::
     forall t. Num t
  => Parser Char t
numberParser = parseWithoutSign <|> parseWithSign
  where
    parseWithSign :: Parser Char t
    parseWithSign = do
      sign <- parseSign
      number <- parseWithoutSign
      return $
        case sign of
          Plus  -> number
          Minus -> (-1) * number
    parseSign :: Parser Char Sign
    parseSign = Minus <$ element '-' <|> Plus <$ element '+'
    parseWithoutSign :: Parser Char t
    parseWithoutSign = do
      (number, _) <- parseWithoutSignImpl
      return number
    parseWithoutSignImpl :: Parser Char (t, t)
    parseWithoutSignImpl = do
      curChar <- satisfy isDigit
      let curNum = charToNum curChar
      (numberTail, numberTailPow) <- parseWithoutSignImpl <|> parseEnd
      return (curNum * numberTailPow + numberTail, 10 * numberTailPow)
    parseEnd :: Parser Char (t, t)
    parseEnd = (0, 1) <$ ok
    charToNum :: Char -> t
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

skipDelimeters :: Parser Char ()
skipDelimeters = do
  skipWhitespaces
  _ <- element ','
  skipWhitespaces
  where
    skipWhitespaces :: Parser Char ()
    skipWhitespaces = skipSingleOrMoreWhitespaces <|> ok
    skipSingleOrMoreWhitespaces :: Parser Char ()
    skipSingleOrMoreWhitespaces = do
      _ <- satisfy isSpace
      skipWhitespaces

numbersListParser ::
     forall t. (Num t, Ord t)
  => Parser Char [t]
numbersListParser = do
  listLength <- numberParser
  if listLength < 0
    then empty
    else parseListOfKnownLength listLength
  where
    parseListOfKnownLength :: t -> Parser Char [t]
    parseListOfKnownLength len
      | len == 0 = return []
      | otherwise = do
        skipDelimeters
        curElem <- numberParser
        elemsTail <- parseListOfKnownLength (len - 1)
        return $ curElem : elemsTail

numbersListsParser ::
     forall t. (Num t, Ord t)
  => Parser Char [[t]]
numbersListsParser = parseNonEmpty <|> parseNil
  where
    parseNonEmpty :: Parser Char [[t]]
    parseNonEmpty = do
      curList <- numbersListParser
      otherLists <- parseRemainder
      return $ curList : otherLists
    parseRemainder :: Parser Char [[t]]
    parseRemainder = parseNonEmptyRemainder <|> parseNil
    parseNonEmptyRemainder :: Parser Char [[t]]
    parseNonEmptyRemainder = do
      skipDelimeters
      remainderHead <- numbersListParser
      remainderTail <- parseRemainder
      return $ remainderHead : remainderTail
    parseNil :: Parser Char [[t]]
    parseNil = [] <$ eof
