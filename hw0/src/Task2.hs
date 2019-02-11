module Task2
       (
         doubleNeg
       , pierce
       , doubleNegElim
       ) where

import Data.Void (Void)

type Neg a = a -> Void

doubleNeg :: a -> Neg (Neg a)
doubleNeg aValue aToVoid = aToVoid aValue

{-
Закон Пирса эквиваленет правилу исключённого третьего и снятию двойного отрицаия,
а значит, не вополняется в ИИВ.

Опровергнем его в ИИВ, воспользовавщись полнотой ИИВ в топологических моделях.

Пусть D = R. [a] = (-inf; 0) U (0; +inf), [b] - пустое множество, топология
евклидова.

Тогда [a -> b] = int(R \ [a] U [b]) = int({0}) = пустое множество

[(a -> b) -> a] = int(R \ [a -> b] U [a]) = R

[((a -> b) -> a) -> a] = int (R \ [((a -> b) -> a)] U [a]) =
= (-inf; 0) U (0; +inf) /= R, то есть закон Пирса оценивается не в R, по полноте
ИИВ в топологических моделях, он недоказуем в ИИВ
-}
pierce :: ((a -> b) -> a) -> a
pierce = undefined

{-
Опровергнем !!a -> a в ИИВ

Пусть D = R
[a] = R \ {0} = (-inf; 0) U (0; inf)
[!a] = int(R \ [a]) = int({0}) = пустое множество
[!!a] = int(R \ [!a]) = int(R) = R
[!!a -> a] = int(R \ [!!a] U [a]) = int([a]) = (-inf; 0) U (0; inf) /= R

Оценивается не в R => недоказуемо в ИИВ
-}
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = undefined

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim = undefined
