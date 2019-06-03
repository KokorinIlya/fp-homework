module Task8Spec
  ( fileSystemModifySpec
  ) where

import Lens.Micro (filtered, traversed, (^..), (^?))
import Task6 (FS (..), contents, isFile, name)
import Task7 (cd, file, ls)
import Task8 (changeExtension)
import Test.Hspec (SpecWith, describe, it, shouldBe)

testFS :: FS
testFS =
  Dir
    { _name = "/"
    , _contents =
        [ Dir
            { _name = "A"
            , _contents =
                [ Dir {_name = "B", _contents = [File {_name = "C.c"}, File {_name = "CC.cc"}]}
                , File {_name = "BB"}
                , Dir {_name = "BBB", _contents = [File {_name = "1"}, File {_name = "2"}]}
                ]
            }
        , Dir {_name = "AA", _contents = []}
        , File {_name = "A1.a1"}
        , File {_name = "A2.a2"}
        , File {_name = "A3.a3"}
        ]
    }

fileSystemModifySpec :: SpecWith ()
fileSystemModifySpec =
  describe "Task8.changeExtension" $ do
    it "changes extensions of all files in directory" $ do
      (changeExtension "bb" testFS) ^.. contents . traversed . name `shouldBe` ["A", "AA", "A1.bb", "A2.bb", "A3.bb"]
