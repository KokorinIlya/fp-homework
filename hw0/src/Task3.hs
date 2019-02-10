module Task3
       (
         identity
       --, pierce
       ) where

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

identity :: a -> a
identity = s const const
