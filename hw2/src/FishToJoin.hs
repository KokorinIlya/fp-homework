{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module FishToJoin
  ( MonadFish (..)
  , MonadJoin (..)
  ) where

import Prelude (id)

class MonadFish m where
  returnFish :: a -> m a
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

class MonadJoin m where
  returnJoin :: a -> m a
  join :: m (m a) -> m a

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
