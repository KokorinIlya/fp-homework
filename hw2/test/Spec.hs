module Main
  ( main
  ) where

import Task1Spec (stringSumSpec, treeSpec, nonEmptySpec)
import Task3Spec (elementParserSpec, eofParserSpec, okParserSpec, satisfyParserSpec, streamParserSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  Task1Spec.stringSumSpec
  Task1Spec.treeSpec
  Task1Spec.nonEmptySpec
  Task3Spec.elementParserSpec
  Task3Spec.eofParserSpec
  Task3Spec.okParserSpec
  Task3Spec.satisfyParserSpec
  Task3Spec.streamParserSpec
