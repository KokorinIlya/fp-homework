module ProgramStructure
  ( Identifier(..)
  , Variable(..)
  , SingleQuotes(..)
  , DoubleQuotesInner(..)
  , Assignment(..)
  , ImplicitQuotesInner(..)
  , ShellCommand(..)
  , Command(..)
  , While(..)
  , If(..)
  , ElseIf(..)
  ) where

import Data.List.NonEmpty (NonEmpty (..))

newtype Identifier =
  Identifier (NonEmpty Char)
  deriving (Show, Eq, Ord)

data Variable
  = IdentifiedVariable Identifier
  | ScriptArgument Int
  | InlineCall [Command]
  deriving (Show)

newtype SingleQuotes =
  SingleQuotes String
  deriving (Show)

data DoubleQuotesInner
  = DoubleQuotesEnd
  | DoubleQuotesVariableRef Variable
                            DoubleQuotesInner
  | DoubleQuotesSimpleString String
                             DoubleQuotesInner
  deriving (Show)

data ImplicitQuotesInner
  = ImplicitQuotesEnd
  | ImplicitQuotesVariableRef Variable
                              ImplicitQuotesInner
  | ImplicitQuotesSimpleString String
                               ImplicitQuotesInner
  | ImplicitQuotesDoubleQuotes DoubleQuotesInner
                               ImplicitQuotesInner
  deriving (Show)

data Assignment =
  Assignment Identifier
             ImplicitQuotesInner
  deriving (Show)

newtype ShellCommand =
  ShellCommand (NonEmpty ImplicitQuotesInner)
  deriving (Show)

data Command
  = AssignmentCommand Assignment
  | CallCommand ShellCommand
  | WhileCommand While
  | IfCommand If
  deriving (Show)

data While = While
  { whileConditions :: NonEmpty Command
  , whileActions    :: [Command]
  } deriving (Show)

data ElseIf = ElseIf
  { elseIfConditions :: NonEmpty Command
  , elseIfActions    :: [Command]
  } deriving (Show)

data If = If
  { ifConditions :: NonEmpty Command
  , ifActions    :: [Command]
  , elseIfs      :: [ElseIf]
  , maybeElse    :: Maybe [Command]
  } deriving (Show)
