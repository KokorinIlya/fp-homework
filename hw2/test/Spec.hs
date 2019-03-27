module Main
  ( main
  ) where

import Test.Hspec (hspec)

import Block1Spec (nonEmptySpec, stringSumSpec, treeSpec)
import Block2Spec (evalSpec, movingAverageSpec)
import Block3Spec (correctBracketSequenceSpec, elementParserSpec, eofParserSpec, numberParserSpec,
                   numbersListsParserSpec, okParserSpec, parserSpec, satisfyParserSpec,
                   streamParserSpec)
import BonusBlockSpec (contSpec, kernelPlaygroundSpec, monadContSpec, trivialContSpec)

main :: IO ()
main =
  hspec $ do
    Block1Spec.stringSumSpec
    Block1Spec.treeSpec
    Block1Spec.nonEmptySpec
    Block2Spec.movingAverageSpec
    Block2Spec.evalSpec
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
    BonusBlockSpec.kernelPlaygroundSpec
