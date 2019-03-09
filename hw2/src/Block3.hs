{-# LANGUAGE ScopedTypeVariables #-}

module Block3
  ( Parser(..)
  , element
  , eof
  , ok
  , satisfy
  , stream
  ) where

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
    cutIfPrefix [] []           = Just []
    cutIfPrefix [] second@(_:_) = Just second
    cutIfPrefix (_:_) []        = Nothing
    cutIfPrefix (x:xs) (y:ys)
      | x == y = cutIfPrefix xs ys
      | otherwise = Nothing
