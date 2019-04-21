module ProgramStructure
  ( Identifier(..)
  , Variable (..)
  , SingleQuotes (..)
  , FragmentsList (..)
  , Assignment (..)
  ) where

import Data.List.NonEmpty (NonEmpty (..))

newtype Identifier =
  Identifier (NonEmpty Char)
  deriving (Show)

data Variable
  = IdentifiedVariable Identifier
  | ScriptArgument Int
  deriving (Show)

newtype SingleQuotes =
  SingleQuotes String
  deriving (Show)

data FragmentsList
  = End
  | VariableRef Variable
                FragmentsList
  | SimpleString String
                 FragmentsList
  deriving (Show)

instance Semigroup FragmentsList where
  End <> x = x
  VariableRef var x <> y = VariableRef var (x <> y)
  SimpleString str x <> y = SimpleString str (x <> y)

instance Monoid FragmentsList where
  mempty = End

data Assignment =
  Assignment Identifier
             FragmentsList
  deriving (Show)
