module Task3Spec
  ( elementParserSpec
  , eofParserSpec
  , okParserSpec
  , satisfyParserSpec
  , streamParserSpec
  ) where

import Block3 (Parser (..), element, eof, ok, satisfy, stream)
import Test.QuickCheck (property)
import Data.Char (isDigit)
import Test.Hspec (SpecWith, describe, it, shouldBe)

elementParserSpec :: SpecWith ()
elementParserSpec =
  describe "Block3.element" $ do
    it "parses signle element correctly" $ do
      runParser (element 'c') "caba" `shouldBe` Just ('c', "aba")

    it "fails when first element doesn't match" $ do
      runParser (element 'c') "abacaba" `shouldBe` Nothing

    it "fails on empty string" $ do
      runParser (element 'c') "" `shouldBe` Nothing

eofParserSpec :: SpecWith ()
eofParserSpec =
  describe "Block3.eof" $ do
    it "parses eof" $ do
      runParser eof "caba" `shouldBe` Nothing

    it "fails when first element doesn't match" $ do
      runParser eof "" `shouldBe` Just ((), "")

okParserSpec :: SpecWith ()
okParserSpec =
  describe "Block3.ok" $ do
    it "parse every string" $ do
      property checkEveryString

  where
    checkEveryString :: String -> Bool
    checkEveryString s = runParser ok s == Just ((), s)

satisfyParserSpec :: SpecWith ()
satisfyParserSpec =
  describe "Block3.satisfy" $ do
    it "parses signle element correctly" $ do
      runParser (satisfy isDigit) "1aba" `shouldBe` Just ('1', "aba")

    it "fails when first element doesn't match" $ do
      runParser (satisfy isDigit) "abacaba" `shouldBe` Nothing

    it "fails on empty string" $ do
      runParser (satisfy isDigit) "" `shouldBe` Nothing

streamParserSpec :: SpecWith ()
streamParserSpec =
  describe "Block3.stream" $ do
    it "parses begining of the string correctly" $ do
      runParser (stream "aba") "abacaba" `shouldBe` Just ("aba", "caba")

    it "fails when beginig of the stream doesn't match" $ do
      runParser (stream "aba") "gdwawjdawdaw" `shouldBe` Nothing

    it "fails on empty string" $ do
      runParser (stream "aba") "" `shouldBe` Nothing

    it "parses beginig of the correct stream" $ do
      property checkEveryStream
  where
    checkEveryStream :: String -> String -> Bool
    checkEveryStream prefix suffix = runParser (stream prefix) (prefix ++ suffix) == Just (prefix, suffix)


