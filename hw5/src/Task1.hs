{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}

module Task1
  ( ST
  , STRef
  , newSTRef
  , readSTRef
  , runST
  , writeSTRef
  , modifySTRef
  , modifySTRef'
  , fib
  , sqrt'
  , otherFib
  ) where

import Control.Monad (ap, forM_, liftM)
import Control.Monad.State (MonadState, State, evalState, get, gets, put)
import qualified Data.Map.Lazy as M
import Data.Typeable (Typeable, cast)

data SomeValue =
  forall a. Typeable a =>
            SomeValue a

newtype STRef s a =
  STRef Integer
  deriving (Eq, Ord, Show)

data SomeSTRef s =
  forall a. SomeSTRef (STRef s a)

instance Eq (SomeSTRef s) where
  (SomeSTRef (STRef x)) == (SomeSTRef (STRef y)) = x == y

instance Ord (SomeSTRef s) where
  compare (SomeSTRef (STRef x)) (SomeSTRef (STRef y)) = compare x y

data ValuesMap s = ValuesMap
  { curRefs   :: M.Map (SomeSTRef s) SomeValue
  , curNumber :: Integer
  }

newtype ST s a = ST
  { unST :: State (ValuesMap s) a
  }

instance Monad (ST s) where
  return value = ST $ return value
  (ST action) >>= f = ST $ action >>= unST . f

instance Applicative (ST s) where
  (<*>) = ap
  pure = return

instance Functor (ST s) where
  fmap = liftM

instance MonadState (ValuesMap s) (ST s) where
  get = ST get
  put = ST . put

newSTRef :: Typeable a => a -> ST s (STRef s a)
newSTRef value = do
  ValuesMap {curRefs = currentRefsMap, curNumber = curMaxNumber} <- get
  let nextMaxNumber = curMaxNumber + 1
  let newRef = STRef nextMaxNumber
  let nextRefsMap = M.insert (SomeSTRef newRef) (SomeValue value) currentRefsMap
  put ValuesMap {curRefs = nextRefsMap, curNumber = nextMaxNumber}
  return newRef

getValueFromSomeValue :: Typeable a => SomeValue -> a
getValueFromSomeValue (SomeValue value) =
  let (Just castedValue) = cast value
   in castedValue

readSTRef :: Typeable a => STRef s a -> ST s a
readSTRef stref = do
  currentRefsMap <- gets curRefs
  let (Just someValue) = M.lookup (SomeSTRef stref) currentRefsMap
  return $ getValueFromSomeValue someValue

writeSTRef :: Typeable a => STRef s a -> a -> ST s ()
writeSTRef stref newValue = do
  valuesMap@ValuesMap {curRefs = currentRefsMap} <- get
  let newMap = M.insert (SomeSTRef stref) (SomeValue newValue) currentRefsMap
  put valuesMap {curRefs = newMap}

modifySTRef :: Typeable a => STRef s a -> (a -> a) -> ST s ()
modifySTRef stref f = readSTRef stref >>= writeSTRef stref . f

modifySTRef' :: Typeable a => STRef s a -> (a -> a) -> ST s ()
modifySTRef' ref f = do
  oldValue <- readSTRef ref
  let !newValue = f oldValue
  writeSTRef ref newValue

runST :: (forall s. ST s a) -> a
runST (ST action) = evalState action (ValuesMap M.empty 0)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib i =
  runST $ do
    s <- newSTRef 0
    t <- newSTRef 1
    forM_ [2 .. i] $ \_ -> do
      _s <- readSTRef s
      _t <- readSTRef t
      writeSTRef s _t
      writeSTRef t (_s + _t)
    readSTRef t

otherFib :: Int -> Int
otherFib 0 = 0
otherFib 1 = 1
otherFib i =
  runST $ do
    s <- newSTRef 0
    t <- newSTRef 1
    forM_ [2 .. i] $ \_ -> do
      _s <- readSTRef s
      _t <- readSTRef t
      modifySTRef s (const _t)
      modifySTRef t (+ _s)
    readSTRef t

eps :: Double
eps = 1.0e-15

whileM :: Monad m => m Bool -> m () -> m ()
whileM c act =
  c >>= \b ->
    if b
      then act >> whileM c act
      else pure ()

sqrt' :: Double -> Double
sqrt' x
  | x < 1 = error "x < 1 not supported"
  | x == 0 = 0
  | otherwise =
    runST $ do
      l <- newSTRef 0
      r <- newSTRef x
      let checkCond = do
            l_ <- readSTRef l
            r_ <- readSTRef r
            pure (r_ - l_ > eps)
      whileM checkCond $ do
        l_ <- readSTRef l -- l^2 < x
        r_ <- readSTRef r -- r^2 >= x
        let m = (l_ + r_) / 2
        if m * m >= x
          then writeSTRef r m
          else writeSTRef l m
      readSTRef r
