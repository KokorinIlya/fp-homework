module Task7Spec
  ( fileSystemTraversalsSpec
  ) where

import Lens.Micro ((^..), (^?))
import Task6 (FS (..))
import Task7 (cd, file, ls)
import Test.Hspec (SpecWith, describe, it, shouldBe)

testFS :: FS
testFS =
  Dir
    { _name = "/"
    , _contents =
        [ Dir
            { _name = "A"
            , _contents =
                [ Dir {_name = "B", _contents = [File {_name = "C"}, File {_name = "CC"}]}
                , File {_name = "BB"}
                , Dir {_name = "BBB", _contents = [File {_name = "1"}, File {_name = "2"}]}
                ]
            }
        , Dir {_name = "AA", _contents = []}
        , File {_name = "AAA"}
        ]
    }

fileSystemTraversalsSpec :: SpecWith ()
fileSystemTraversalsSpec =
  describe "Task7.cd, ls, file'" $ do
    it "gets existing file from folder" $ do testFS ^? cd "A" . cd "B" . file "C" `shouldBe` Just "C"
    it "gets non-existing file from folder" $ do testFS ^? cd "A" . cd "B" . file "BB" `shouldBe` Nothing
    it "gets file from file" $ do File {_name = "C"} ^? file "C" `shouldBe` Just "C"
    it "gets file from file" $ do File {_name = "B"} ^? file "C" `shouldBe` Nothing
    it "doen't get directories as files" $ do testFS ^? file "A" `shouldBe` Nothing
    it "gets content from existing folder" $ do testFS ^.. cd "A" . cd "B" . ls `shouldBe` ["C", "CC"]
    it "gets content from non-existing folder" $ do testFS ^.. cd "A" . cd "ABACABA" . ls `shouldBe` []
    it "gets content from empty folder" $ do testFS ^.. cd "AA" . ls `shouldBe` []
    it "gets content from folder, containing other folders" $ do testFS ^.. cd "A" . ls `shouldBe` ["B", "BB", "BBB"]
    it "gets content from file" $ do testFS ^.. cd "A" . cd "B" . cd "C" . ls `shouldBe` []
