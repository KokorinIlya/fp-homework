{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Block4
       (
         Pair (..)
       , NonEmpty (..)
       , splitOn
       , joinWith
       ) where

import           NonEmptys (NonEmpty (..), reverseNonEmpty)

data Pair a = Pair a a

instance Foldable Pair where
  foldMap :: Monoid m => (a -> m) -> Pair a -> m
  foldMap toMonoidMapper (Pair first second) =
    let firstMonoid  = toMonoidMapper first
        secondMonoid = toMonoidMapper second in
    firstMonoid <> secondMonoid

  foldr :: (a -> b -> b) -> b -> Pair a -> b
  foldr f z (Pair first second) = f first (f second z)

splitOn :: forall a . (Eq a) => a -> [a] -> NonEmpty [a]
splitOn splitElem list =
  reverseNonEmpty $ fmap reverse (foldl processCurSymbol ([] :| []) list)

  where
    processCurSymbol :: NonEmpty [a] -> a -> NonEmpty [a]
    processCurSymbol (x :| xs) curElem =
      if curElem == splitElem
      then [] :| (x : xs)
      else (curElem : x) :| xs

joinWith :: forall a . a -> NonEmpty [a] -> [a]
joinWith _ (theOnly :| []) = theOnly
joinWith elemToJoin (first :| (second : others)) =
  first ++ joinWithEndingElem (second :| others)

  where
    joinWithEndingElem :: NonEmpty [a] -> [a]
    joinWithEndingElem nonEmpty =
      joinWithEndingElemAcc (reverseNonEmpty nonEmpty) []

    joinWithEndingElemAcc :: NonEmpty [a] -> [a] -> [a]
    joinWithEndingElemAcc (x :| []) acc =
      elemToJoin : (x ++ acc)
    joinWithEndingElemAcc (curList :| (nextList : otherLists)) acc =
      joinWithEndingElemAcc (nextList :| otherLists)
        (elemToJoin : (curList ++ acc))
