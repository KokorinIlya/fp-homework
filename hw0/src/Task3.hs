module Task3
  ( identity
  , composition
  , contraction
  , permutation
  ) where

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

{-
Думаю, что будет похоже на решения других людей, так как мы это уже проходили на ТТ.
Такие решения даны в любой книжке по комбинаторному исчислению, для примера можно взять
https://en.wikipedia.org/wiki/B,_C,_K,_W_system
и
https://en.wikipedia.org/wiki/SKI_combinator_calculus
-}
identity :: a -> a
identity = s const const

composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (const s) const

contraction :: (a -> a -> b) -> a -> b
contraction = s s (s const)

permutation :: (a -> b -> c) -> b -> a -> c
permutation = s (s (const (s (const s) const)) s) (const const)
