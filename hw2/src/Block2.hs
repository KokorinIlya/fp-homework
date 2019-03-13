module Block2
  ( ArithmeticError(..)
  , Expression(..)
  , MovingAverageState(..)
  , Queue(..)
  , empty
  , eval
  , moving
  , top
  , pop
  , push
  ) where

import Control.Monad.State (State, state, evalState)
import Data.Maybe (fromMaybe)

data Expression
  = Const Int
  | Addition Expression
             Expression
  | Subtraction Expression
                Expression
  | Multiplication Expression
                   Expression
  | Division Expression
             Expression
  | Power Expression
          Expression
  | Negation Expression

data ArithmeticError
  = DivisionByZero
  | NegativePow
  deriving (Show)

instance Eq ArithmeticError where
  DivisionByZero == DivisionByZero = True
  NegativePow == NegativePow       = True
  _ == _                           = False

returnError :: a -> Either a b
returnError = Left

eval :: Expression -> Either ArithmeticError Int
eval (Const x) = Right x
eval (Addition left right) = (+) <$> eval left <*> eval right
eval (Subtraction left right) = (-) <$> eval left <*> eval right
eval (Multiplication left right) = (*) <$> eval left <*> eval right
eval (Division left right) = do
  rightResult <- eval right
  if rightResult == 0
    then returnError DivisionByZero
    else do
      leftResult <- eval left
      return $ leftResult `div` rightResult
eval (Power left right) = do
  rightResult <- eval right
  if rightResult < 0
    then returnError NegativePow
    else do
      leftResult <- eval left
      return $ leftResult ^ rightResult
eval (Negation expr) = fmap negate (eval expr)

data Queue a = Queue
  { headList :: [a]
  , tailList :: [a]
  } deriving (Show)

push :: Queue a -> a -> Queue a
push queue@Queue {tailList = curTailList} x = queue {tailList = x : curTailList}

pop :: Queue a -> Maybe (a, Queue a)
pop queue@Queue {headList = x:xs} = Just (x, queue {headList = xs})
pop Queue {headList = [], tailList = curTail} =
  case reverse curTail of
    []   -> Nothing
    x:xs -> Just (x, Queue {headList = xs, tailList = []})

top :: Queue a -> Maybe a
top Queue {headList = x:_}                          = Just x
top Queue {headList = [], tailList = curTail@(_:_)} = Just $ last curTail
top Queue {headList = [], tailList = []}            = Nothing

empty :: Queue a
empty = Queue {headList = [], tailList = []}

data MovingAverageState = MovingAverageState
  { windowQueue :: Queue Double
  , windowSum   :: Double
  , windowLen   :: Int
  }

moving :: Int -> [Double] -> [Double]
moving _ [] = []
moving windowSize list
  | windowSize <= 0 = error "Cannot compute average for window size <= 0"
  | otherwise =
    let initialState = MovingAverageState {windowQueue = empty, windowSum = 0, windowLen = 0}
     in evalState (processList list) initialState
  where
    processList :: [Double] -> State MovingAverageState [Double]
    processList [] = return []
    processList (x:xs) = do
      processedPoint <- processCurPointState x
      tailPoints <- processList xs
      return $ processedPoint : tailPoints
    processCurPointState :: Double -> State MovingAverageState Double
    processCurPointState x = state $ processCurPoint x
    processCurPoint :: Double -> MovingAverageState -> (Double, MovingAverageState)
    processCurPoint x curState@MovingAverageState {windowQueue = curQueue, windowSum = curSum, windowLen = curLen}
      | curLen < windowSize =
        let newQueue = push curQueue x
            newSum = curSum + x
            newLen = curLen + 1
            newState = MovingAverageState {windowQueue = newQueue, windowSum = newSum, windowLen = newLen}
            answer = newSum / fromIntegral newLen
         in (answer, newState)
      | otherwise =
        let (lastValue, queueWithoutLastValue) = fromMaybe (error "Conract violation, size = 0") (pop curQueue)
            newQueue = push queueWithoutLastValue x
            newSum = curSum - lastValue + x
            newState = curState {windowQueue = newQueue, windowSum = newSum}
            answer = newSum / fromIntegral curLen
         in (answer, newState)
