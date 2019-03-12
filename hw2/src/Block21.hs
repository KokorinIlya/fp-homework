{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module Block21 where

import Block2 (Monad (..), MonadFish (..), MonadJoin (..))
import Prelude (id, flip)

instance Monad m => MonadFish m where
  returnFish = return
  f >=> g = \x -> f x >>= g


instance Monad m => MonadJoin m where
  returnJoin = return
  {-
  (>>=) :: m a -> (a -> m b) -> m b
  id :: a' -> a' => a = m b
  m a = m (m b)
  (>>= id) :: m (m b) -> m b
  -}
  join = (>>= id)


instance MonadFish m => Monad m where
  return = returnFish
  {-
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
  id :: a' -> a' => a = m b

  (>=>) :: (m b -> m b) -> (b -> m c) -> (m b -> m c)
  id >=> :: (b -> m c) -> m b -> m c
  -}
  (>>=) = flip (id >=>)
