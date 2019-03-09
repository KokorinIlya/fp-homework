{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module Block2
  ( Monad(..)
  , MonadFish(..)
  , MonadJoin(..)
  ) where

import Prelude (flip, id)

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

class MonadFish m where
  returnFish :: a -> m a
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

class MonadJoin m where
  returnJoin :: a -> m a
  join :: m (m a) -> m a

instance MonadFish m => Monad m where
  return = returnFish
  {-
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
  id :: a' -> a' => a = m b

  (>=>) :: (m b -> m b) -> (b -> m c) -> (m b -> m c)
  id >=> :: (b -> m c) -> m b -> m c
  -}
  (>>=) = flip (id >=>)

instance Monad m => MonadFish m where
  returnFish = return
  f >=> g = \x -> f x >>= g

instance MonadFish m => MonadJoin m where
  returnJoin = returnFish
  {-
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
  id1 :: a' -> a'
  id1 :: a -> m b => a = m b

  id2 :: b' -> b'
  id2 :: b -> m c => b = m c => a = m (m c)

  (>=>) :: (m (m c) -> m (m c)) -> (m c -> m c) -> (m (m c) -> m c)

  id >=> id :: m (m c) -> m c
  -}
  join = id >=> id


instance Monad m => MonadJoin m where
  returnJoin = return
  {-
  (>>=) :: m a -> (a -> m b) -> m b
  id :: a' -> a' => a = m b
  m a = m (m b)
  (>>= id) :: m (m b) -> m b
  -}
  join = (>>= id)
