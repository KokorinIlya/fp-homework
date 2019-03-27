module Block3Spec
  ( correctBracketSequenceSpec
  , elementParserSpec
  , eofParserSpec
  , numberParserSpec
  , numbersListsParserSpec
  , okParserSpec
  , parserSpec
  , satisfyParserSpec
  , streamParserSpec
  ) where

import Data.Char (isDigit)
import Data.Maybe (isNothing)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Test.QuickCheck (property)

import Block3 (CorrectBracketSequence (..), Parser (..), correctBracketSequenceParser, element, eof,
               numberParser, numbersListsParser, ok, satisfy, stream)
import Utils (mapFirst)

parserSpec :: SpecWith ()
parserSpec =
  describe "Block3.Parser" $ do
    it "can be used for applicative style programming" $ do
      runParser ((++) <$> stream "aba" <*> stream "caba") "abacaba" `shouldBe` Just ("abacaba", "")

    it "fails, if first appicative fails" $ do
      runParser ((++) <$> stream "aca" <*> stream "caba") "abacaba" `shouldBe` Nothing

    it "fails, if second appicative fails" $ do
      runParser ((++) <$> stream "aba" <*> stream "caca") "abacaba" `shouldBe` Nothing

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

correctBracketSequenceSpec :: SpecWith ()
correctBracketSequenceSpec =
  describe "Block3.correctBracketSequenceParser" $ do
    it "Parses empty sequences" $ do
      showSeq (runParser correctBracketSequenceParser "") `shouldBe` Just ("", "")

    it "Parses trivial sequences" $ do
      showSeq (runParser correctBracketSequenceParser "()") `shouldBe` Just ("()", "")

    it "Parses concatenations" $ do
      showSeq (runParser correctBracketSequenceParser "()()") `shouldBe` Just ("()()", "")

    it "Parses inner sequences" $ do
        showSeq (runParser correctBracketSequenceParser "(())") `shouldBe` Just ("(())", "")

    it "Parses composition of concatenations and inner sequences" $ do
      showSeq (runParser correctBracketSequenceParser "(())()(()(()))()()") `shouldBe` Just ("(())()(()(()))()()", "")

    it "Fails on balance > 0" $ do
      isNothing (runParser correctBracketSequenceParser "((()") `shouldBe` True

    it "Fails on balance < 0" $ do
      isNothing (runParser correctBracketSequenceParser "()(()))") `shouldBe` True

    it "Fails on non-bracket symbols" $ do
      isNothing (runParser correctBracketSequenceParser "() a") `shouldBe` True
  where
    showSeq :: Maybe (CorrectBracketSequence, String) -> Maybe (String, String)
    showSeq = fmap $ mapFirst show

numberParserSpec :: SpecWith ()
numberParserSpec =
  describe "Block3.numberParser" $ do
    it "parses ordinary number" $ do
      runParser numberParser "123" `shouldBe` Just (123 :: Int, "")

    it "parses BIG numbers" $ do
      runParser numberParser "1234567890987654321234567890" `shouldBe` Just (1234567890987654321234567890 :: Integer, "")

    it "parses number, discarding tail" $ do
      runParser numberParser "123abacaba" `shouldBe` Just (123 :: Int, "abacaba")

    it "parses number with +" $ do
      runParser numberParser "+123" `shouldBe` Just (123 :: Int, "")

    it "parses number with -" $ do
      runParser numberParser "-123-" `shouldBe` Just (-123 :: Int, "-")

    it "fails on empty input" $ do
      runParser numberParser "" `shouldBe` (Nothing :: Maybe (Int, String))

    it "fails on two-signed numbers" $ do
      runParser numberParser "++123" `shouldBe` (Nothing :: Maybe (Int, String))

    it "fails on non-number chars" $ do
      runParser numberParser "aba123" `shouldBe` (Nothing :: Maybe (Int, String))

    it "parses char plus string" $ do
      property checkEveryNumber

    where
      checkEveryNumber :: Int -> Int -> Bool
      checkEveryNumber n s =
        let addingString = replicate s 'a'
         in runParser numberParser (show n ++ addingString) == Just (n, addingString)

numbersListsParserSpec :: SpecWith ()
numbersListsParserSpec =
  describe "Block3.numbersListsParser" $ do
    it "parses empty lists" $ do
      runParser numbersListsParser "0 , 0   , 0, 0" `shouldBe` Just ([[], [], [], []] :: [[Int]], "")

    it "parses empty list" $ do
      runParser numbersListsParser "" `shouldBe` Just ([] :: [[Int]], "")

    it "parses non empty lists" $ do
      runParser numbersListsParser "2, 1,+10  , 3,5,-7, 2" `shouldBe` Just ([[1, 10], [5, -7, 2]] :: [[Int]], "")

    it "parses lists when whitespaces present in the beginning of the line" $ do
      runParser numbersListsParser "      2, 1,+10  , 3,5,-7, 2" `shouldBe` Just ([[1, 10], [5, -7, 2]] :: [[Int]], "")

    it "parses length with signs" $ do
      runParser numbersListsParser "+2, -1,+10  , +3,+5,-7, 2" `shouldBe` Just ([[-1, 10], [5, -7, 2]] :: [[Int]], "")

    it "fails, when not all input is parsed" $ do
      runParser numbersListsParser "+2, -1,+10  , +3,+5,-7, 2 " `shouldBe` (Nothing :: Maybe ([[Int]], String))

    it "fails, when negative list length is given" $ do
      runParser numbersListsParser "1, 2, 0, -1" `shouldBe` (Nothing :: Maybe ([[Int]], String))

    it "fails, when length is not valid" $ do
      runParser numbersListsParser "+2, -1" `shouldBe` (Nothing :: Maybe ([[Int]], String))

    it "parses every correct list" $ do
      property checkCorrectLists

  where
    checkCorrectLists :: [Int] -> [[Int]] -> Bool
    checkCorrectLists headList tailLists =
      let stringForm = tail (foldMap listToString (headList : tailLists))
       in runParser numbersListsParser stringForm == Just (headList : tailLists, "")

    listToString :: [Int] -> String
    listToString list =
      let stringList = foldMap intToString list
       in ',' : (show (length list) ++ stringList)

    intToString :: Int -> String
    intToString x = ',' : show x


