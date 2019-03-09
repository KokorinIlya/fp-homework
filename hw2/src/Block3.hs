{-# LANGUAGE ScopedTypeVariables #-}

module Block3
  ( CorrectBracketSequence(..)
  , Sign
  , Parser(..)
  , correctBracketSequenceParser
  , element
  , eof
  , numberParser
  , ok
  , satisfy
  , stream
  ) where

import Data.Char (isDigit)
import Control.Applicative (Alternative (..))

newtype Parser s a = Parser
  { runParser :: [s] -> Maybe (a, [s])
  }

instance Functor (Parser s) where
  fmap f (Parser parsingFunction) =
    Parser $ \input ->
      case parsingFunction input of
        Nothing                  -> Nothing
        Just (result, remainder) -> Just (f result, remainder)

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
  Parser pa <|> Parser pb =
    Parser $ \input ->
      case pa input of
        Nothing         -> pb input
        result@(Just _) -> result

instance Monad (Parser s) where
  Parser pa >>= f =
    Parser $ \input ->
      case pa input of
        Nothing -> Nothing
        Just (a, remainder) ->
          let (Parser pb) = f a
           in pb remainder

runSequentually :: Parser s a -> Parser s b -> Parser s (a, b)
runSequentually pa pb = do
  a <- pa
  b <- pb
  return (a, b)

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
stream streamToParse =
  Parser $ \input ->
    case cutIfPrefix streamToParse input of
      Just remainder -> Just (streamToParse, remainder)
      Nothing        -> Nothing
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
    nonEmptyParser = fmap (uncurry Concatenation) (runSequentually innerParser bracketParser)

    innerParser :: Parser Char CorrectBracketSequence
    innerParser = fmap Inner (element '(' *> bracketParser <* element ')')

    emptyParser :: Parser Char CorrectBracketSequence
    emptyParser = fmap (const Empty) ok

data Sign = Plus | Minus

numberParser :: forall t. Num t => Parser Char t
numberParser = parseWithoutSign <|> parseWithSign
  where
    parseSign :: Parser Char Sign
    parseSign = fmap (const Minus) (element '-') <|> fmap (const Plus) (element '+')

    parseWithSign :: Parser Char t
    parseWithSign = do
      sign <- parseSign
      number <- parseWithoutSign
      return $ case sign of
        Plus  -> number
        Minus -> (-1) * number

    parseWithoutSign :: Parser Char t
    parseWithoutSign = do
      (number, _) <- parseWithoutSignImpl
      return number

    parseWithoutSignImpl :: Parser Char (t, t)
    parseWithoutSignImpl = do
      curChar <- satisfy isDigit
      let curNum = charToNum curChar
      (numberTail, numberTailPow) <- parseEnd <|> parseWithoutSignImpl
      return (curNum * numberTailPow + numberTail, 10 * numberTailPow)

    parseEnd :: Parser Char (t, t)
    parseEnd = fmap (const (0, 1)) eof

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
    charToNum _ = 9
