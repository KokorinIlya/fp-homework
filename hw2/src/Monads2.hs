{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module Monads2
  ( Monad (..)
  , MonadJoin (..)
  ) where

import Prelude (id)

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

class MonadJoin m where
  returnJoin :: a -> m a
  join :: m (m a) -> m a

instance Monad m => MonadJoin m where
  returnJoin = return
  {-
  (>>=) :: m a -> (a -> m b) -> m b
  id :: a' -> a' => a = m b
  m a = m (m b)
  (>>= id) :: m (m b) -> m b
  -}
  join = (>>= id)
