module Main
  ( main
  ) where

import Task5Spec (choosingSpec, simpleLensSpec, strangeLensSpec, tupleFirstLensSpec,
                  tupleSecondLensSpec)
import Task7Spec (fileSystemTraversalsSpec)
import Task8Spec (fileSystemModifySpec)
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    simpleLensSpec
    tupleFirstLensSpec
    tupleSecondLensSpec
    choosingSpec
    strangeLensSpec
    fileSystemTraversalsSpec
    fileSystemModifySpec
