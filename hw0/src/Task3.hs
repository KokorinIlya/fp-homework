module Task3
       (
         identity
       , composition
       , contraction
       , permutation
       ) where

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

identity :: a -> a
identity = s const const

-- B
composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (const s) const

contraction :: (a -> a -> b) -> a -> b
contraction = s s (s const)

permutation :: (a -> b -> c) -> b -> a -> c
permutation = s (s (const (s (const s) const)) s) (const const)
