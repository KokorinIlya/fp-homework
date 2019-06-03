module Main
  ( main
  ) where

import Task5Spec (choosingSpec, simpleLensSpec, strangeLensSpec, tupleFirstLensSpec,
                  tupleSecondLensSpec)
import Task7Spec (fileSystemTraversalsSpec)
import Task8Spec (changeExtensionSpec, getAllChildrenSpec, rmSpec)
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
    changeExtensionSpec
    getAllChildrenSpec
    rmSpec
