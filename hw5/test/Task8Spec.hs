module Task8Spec
  ( changeExtensionSpec
  , getAllChildrenSpec
  , rmSpec
  ) where

import Lens.Micro ((^..), (^?))
import Task6 (FS (..))
import Task7 (cd, ls)
import Task8 (changeExtension, getAllChildren, rm)
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
                , File {_name = "BB.aa"}
                , Dir {_name = "BBB", _contents = [File {_name = "1"}, File {_name = "2"}]}
                ]
            }
        , Dir {_name = "AA", _contents = []}
        , Dir {_name = "AAOther", _contents = []}
        , File {_name = "A1.a1"}
        , File {_name = "A2.a2"}
        , File {_name = "A3.a3"}
        ]
    }

changeExtensionSpec :: SpecWith ()
changeExtensionSpec =
  describe "Task8.changeExtension" $ do
    it "changes extensions of all files in directory" $ do
      (changeExtension "bb" testFS) ^.. ls `shouldBe` ["A", "AA", "AAOther", "A1.bb", "A2.bb", "A3.bb"]
    it "changes extensions of all files in directory" $ do
      let (Just subdir) = testFS ^? cd "AA"
      (changeExtension "bb" subdir) ^.. ls `shouldBe` []
    it "changes extensions of all files in directory" $ do
      let (Just subdir) = testFS ^? cd "A"
      (changeExtension "bb" subdir) ^.. ls `shouldBe` ["B", "BB.bb", "BBB"]

getAllChildrenSpec :: SpecWith ()
getAllChildrenSpec =
  describe "Task8.getAllChildren" $ do
    it "gets all children" $ do
      getAllChildren testFS `shouldBe`
        ["A1.a1", "A2.a2", "A3.a3", "A", "AA", "AAOther", "BB.aa", "B", "BBB", "C.c", "CC.cc", "1", "2"]
    it "gets empty list of children" $ do
      let (Just subdir) = testFS ^? cd "AA"
      getAllChildren subdir `shouldBe` []
    it "gets empty list of children from file" $ do getAllChildren File {_name = "filename"} `shouldBe` []

rmSpec :: SpecWith ()
rmSpec =
  describe "Task8.rm" $ do
    it "removes empty directories with matching name" $ do
      rm "AA" testFS ^.. ls `shouldBe` ["A", "AAOther", "A1.a1", "A2.a2", "A3.a3"]
    it "removes empty directories with matching name" $ do
      rm "AAOther" testFS ^.. ls `shouldBe` ["A", "AA", "A1.a1", "A2.a2", "A3.a3"]
    it "doesn't remove empty directories with non-matching name" $ do
      rm "AB" testFS ^.. ls `shouldBe` ["A", "AA", "AAOther", "A1.a1", "A2.a2", "A3.a3"]
    it "doesn't remove non-empty directories" $ do
      rm "A" testFS ^.. ls `shouldBe` ["A", "AA", "AAOther", "A1.a1", "A2.a2", "A3.a3"]
    it "doesn't remove directories from files" $ do
      rm "A" File {_name = "filename"} ^.. ls `shouldBe` []
