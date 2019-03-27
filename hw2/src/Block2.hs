module Block2
  ( ArithmeticError(..)
  , Expression(..)
  , MovingAverageState(..)
  , NonEmptyQueue(..)
  , eval
  , moving
  , popAndPush
  , push
  , singleElementQueue
  ) where

import Control.Monad.State (State, evalState, get, put)

import Block1 (NonEmpty (..))

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
  NegativePow == NegativePow = True
  _ == _ = False

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
    else div <$> eval left <*> pure rightResult
eval (Power left right) = do
  rightResult <- eval right
  if rightResult < 0
    then returnError NegativePow
    else (^) <$> eval left <*> pure rightResult
eval (Negation expr) = fmap negate (eval expr)

data NonEmptyQueue a = NonEmptyQueue
  { headList :: NonEmpty a
  , tailList :: [a]
  }

push :: NonEmptyQueue a -> a -> NonEmptyQueue a
push queue@NonEmptyQueue {tailList = curTailList} x = queue {tailList = x : curTailList}

popAndPush :: NonEmptyQueue a -> a -> (a, NonEmptyQueue a)
popAndPush NonEmptyQueue {headList = curHead :| (nextHead:headListTail), tailList = curTailList} x =
  (curHead, NonEmptyQueue {headList = nextHead :| headListTail, tailList = x : curTailList})
popAndPush NonEmptyQueue {headList = curHead :| [], tailList = curTailList} x =
  case reverse curTailList of
    []   -> (curHead, NonEmptyQueue {headList = x :| [], tailList = []})
    y:ys -> (curHead, NonEmptyQueue {headList = y :| ys, tailList = [x]})

singleElementQueue :: a -> NonEmptyQueue a
singleElementQueue x = NonEmptyQueue {headList = x :| [], tailList = []}

data MovingAverageState = MovingAverageState
  { windowQueue :: NonEmptyQueue Double
  , windowSum   :: Double
  , windowLen   :: Int
  }

moving :: Int -> [Double] -> [Double]
moving _ [] = []
moving windowSize (firstPoint:otherPoints)
  | windowSize <= 0 = error "Cannot compute average for window size <= 0"
  | otherwise =
    let initialQueue = singleElementQueue firstPoint
        initialState = MovingAverageState {windowQueue = initialQueue, windowSum = firstPoint, windowLen = 1}
     in firstPoint : evalState (processList otherPoints) initialState
  where
    processList :: [Double] -> State MovingAverageState [Double]
    processList [] = return []
    processList (x:xs) = do
      processedPoint <- processCurPoint x
      tailPoints <- processList xs
      return $ processedPoint : tailPoints

    processCurPoint :: Double -> State MovingAverageState Double
    processCurPoint x = do
      curState@MovingAverageState {windowQueue = curQueue, windowSum = curSum, windowLen = curLen} <- get
      if curLen < windowSize
        then do
          let newQueue = push curQueue x
          let newSum = curSum + x
          let newLen = curLen + 1
          put MovingAverageState {windowQueue = newQueue, windowSum = newSum, windowLen = newLen}
          return $ newSum / fromIntegral newLen
        else do
          let (lastValue, newQueue) = popAndPush curQueue x
          let newSum = curSum - lastValue + x
          put curState {windowQueue = newQueue, windowSum = newSum}
          return $ newSum / fromIntegral curLen
