{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module MonadToFish
  ( Monad (..)
  , MonadFish (..)
  ) where

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

class MonadFish m where
  returnFish :: a -> m a
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

instance Monad m => MonadFish m where
  returnFish = return
  f >=> g = \x -> f x >>= g
