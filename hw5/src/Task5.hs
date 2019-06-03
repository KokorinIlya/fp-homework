{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

module Task5
  ( Lens
  , Lens'
  , set
  , view
  , over
  , lens
  , (.~)
  , (^.)
  , (%~)
  , _1
  , _2
  , choosing
  , (<%~)
  , (<<%~)
  ) where

import Data.Functor.Const (Const (..), getConst)
import Data.Functor.Identity (Identity (..), runIdentity)

type Lens s t a b
   = forall f. Functor f =>
                 (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

set :: Lens s t a b -> b -> s -> t
set l a = runIdentity . l (Identity . const a)

view :: Lens s t a b -> s -> a
view l = getConst . l Const

over :: Lens s t a b -> (a -> b) -> s -> t
over l fn = runIdentity . l (Identity . fn)

infixr 4 .~

(.~) :: Lens s t a b -> b -> s -> t
(.~) = set

infixl 8 ^.

(^.) :: s -> Lens s t a b -> a
x ^. l = view l x

infixr 4 %~

(%~) :: Lens s t a b -> (a -> b) -> s -> t
(%~) = over

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter f s = setter s <$> f (getter s)

_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (, x) <$> f a

_2 :: Lens (x, a) (x, b) a b
_2 f (x, b) = (x, ) <$> f b

choosing :: forall s1 s2 t1 t2 a b. Lens s1 t1 a b -> Lens s2 t2 a b -> Lens (Either s1 s2) (Either t1 t2) a b
choosing lens1 lens2 = lens eitherGet eitherSet
  where
    eitherGet :: Either s1 s2 -> a
    eitherGet (Left s1)  = view lens1 s1
    eitherGet (Right s2) = view lens2 s2
    eitherSet :: Either s1 s2 -> b -> Either t1 t2
    eitherSet (Left s1) value  = Left $ set lens1 value s1
    eitherSet (Right s2) value = Right $ set lens2 value s2

(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = (f (view l s), over l f s)

(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = (view l s, over l f s)
