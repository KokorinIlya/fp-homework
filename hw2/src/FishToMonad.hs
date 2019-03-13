{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module FishToMonad
  ( Monad(..)
  , MonadFish(..)
  ) where

import Prelude (flip, id)

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

class MonadFish m where
  returnFish :: a -> m a
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

instance MonadFish m => Monad m where
  return = returnFish
  {-
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
  id :: a' -> a' => a = m b

  (>=>) :: (m b -> m b) -> (b -> m c) -> (m b -> m c)
  id >=> :: (b -> m c) -> m b -> m c
  -}
  (>>=) = flip (id >=>)
