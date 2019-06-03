{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Task2 where

data Number
  = IntNumber Int
  | DoubleNumber Double

data ValueType
  = NumberVariable Number
  | BoolVariable Bool
  | StringVariable String
  | UnaryFunction (ValueType -> ValueType)
  | BinaryFunction (ValueType -> ValueType -> ValueType)

newtype Variable = Variable ValueType

