module Main
  ( main
  ) where

import Block1Spec (nonEmptySpec, stringSumSpec, treeSpec)
import Block3Spec (correctBracketSequenceSpec, elementParserSpec, eofParserSpec, numberParserSpec,
                  numbersListsParserSpec, okParserSpec, satisfyParserSpec, streamParserSpec, parserSpec)
import Block2Spec (evalSpec, movingAverageSpec)
import BonusBlockSpec (contSpec, monadContSpec, trivialContSpec)
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    Block1Spec.stringSumSpec
    Block1Spec.treeSpec
    Block1Spec.nonEmptySpec
    Block3Spec.elementParserSpec
    Block3Spec.eofParserSpec
    Block3Spec.okParserSpec
    Block3Spec.satisfyParserSpec
    Block3Spec.streamParserSpec
    Block3Spec.correctBracketSequenceSpec
    Block3Spec.numberParserSpec
    Block3Spec.numbersListsParserSpec
    Block3Spec.parserSpec
    BonusBlockSpec.trivialContSpec
    BonusBlockSpec.contSpec
    BonusBlockSpec.monadContSpec
    Block2Spec.movingAverageSpec
    Block2Spec.evalSpec
