module Task5Spec
  ( simpleLensSpec
  , tupleFirstLensSpec
  , tupleSecondLensSpec
  , choosingSpec
  , strangeLensSpec
  ) where

import Test.Hspec (SpecWith, describe, it, shouldBe)

import Data.Function ((&))
import Task5 (Lens', choosing, lens, (%~), (.~), (<%~), (<<%~), (^.), _1, _2)

data Person = Person
  { _name    :: String
  , _age     :: Int
  , _address :: Address
  } deriving (Eq, Show)

data Address = Address
  { _house  :: Int
  , _street :: String
  , _city   :: String
  } deriving (Eq, Show)

address :: Lens' Person Address
address = lens _address (\person newAddress -> person {_address = newAddress})

house :: Lens' Address Int
house = lens _house (\curAddress newHouse -> curAddress {_house = newHouse})

data Foo = Foo
  { _foo :: String
  , _x   :: Int
  } deriving (Eq, Show)

data Bar = Bar
  { _bar :: String
  , _y   :: Int
  } deriving (Eq, Show)

foo :: Lens' Foo String
foo = lens _foo (\curFoo newFoo -> curFoo {_foo = newFoo})

bar :: Lens' Bar String
bar = lens _bar (\curBar newBar -> curBar {_bar = newBar})

james :: Person
james = Person {_name = "James", _age = 28, _address = Address {_house = 42, _street = "Some Road", _city = "London"}}

simpleLensSpec :: SpecWith ()
simpleLensSpec =
  describe "Task5.Lens'" $ do
    it "gets values" $ do james ^. address . house `shouldBe` 42
    it "sets values" $ do
      (james & address . house .~ 24) `shouldBe`
        Person {_name = "James", _age = 28, _address = Address {_house = 24, _street = "Some Road", _city = "London"}}
    it "changes values" $ do
      (james & address . house %~ (+ 1)) `shouldBe`
        Person {_name = "James", _age = 28, _address = Address {_house = 43, _street = "Some Road", _city = "London"}}

tupleFirstLensSpec :: SpecWith ()
tupleFirstLensSpec =
  describe "Task5._1'" $ do
    it "gets values" $ do ("Hello", 42 :: Int) ^. _1 `shouldBe` "Hello"
    it "gets values of complex types" $ do (james, "Hello") ^. _1 . address . house `shouldBe` 42
    it "sets values" $ do (("Hello", 2) & _1 .~ "hello") `shouldBe` (("hello", 2) :: (String, Int))
    it "sets values with different types" $ do (("Hello", 2) & _1 .~ 1) `shouldBe` ((1, 2) :: (Int, Int))
    it "changes values" $ do (("Hello", 2) & _1 %~ (++ ", world")) `shouldBe` (("Hello, world", 2) :: (String, Int))

tupleSecondLensSpec :: SpecWith ()
tupleSecondLensSpec =
  describe "Task5._2'" $ do
    it "gets values" $ do ("Hello", 42) ^. _2 `shouldBe` (42 :: Int)
    it "gets values of complex types" $ do ("Hello", james) ^. _2 . address . house `shouldBe` 42
    it "sets values" $ do ((("Hello", 2) :: (String, Int)) & _2 .~ 1) `shouldBe` (("Hello", 1) :: (String, Int))
    it "sets values with different types" $ do (("Hello", 2 :: Int) & _2 .~ "world") `shouldBe` ("Hello", "world")
    it "changes values" $ do (("Hello", 2) & _2 %~ (+ 1)) `shouldBe` (("Hello", 3) :: (String, Int))

choosingSpec :: SpecWith ()
choosingSpec =
  describe "Task5.choosing" $ do
    it "gets values" $ do Left (Foo "foo" 1) ^. choosing foo bar `shouldBe` "foo"
    it "gets values" $ do Right (Bar "bar" 2) ^. choosing foo bar `shouldBe` "bar"
    it "sets values" $ do (Left (Foo "foo" 1) & choosing foo bar .~ "newFoo") `shouldBe` Left (Foo "newFoo" 1)
    it "sets values" $ do (Right (Bar "bar" 1) & choosing foo bar .~ "newBar") `shouldBe` Right (Bar "newBar" 1)
    it "changes values" $ do (Left (Foo "foo" 1) & choosing foo bar %~ ("new" ++)) `shouldBe` Left (Foo "newfoo" 1)
    it "changes values" $ do (Right (Bar "bar" 1) & choosing foo bar %~ ("new" ++)) `shouldBe` Right (Bar "newbar" 1)

strangeLensSpec :: SpecWith ()
strangeLensSpec =
  describe "Task5.<%~, <<%~" $ do
    it "works with <%~" $ do (((1, "hello") :: (Int, String)) & _1 <%~ negate) `shouldBe` (-1, (-1, "hello"))
    it "works with <<%~" $ do (((1, "hello") :: (Int, String)) & _1 <<%~ negate) `shouldBe` (1, (-1, "hello"))
    it "works with <%~ and different types" $ do
      (((1, "hello") :: (Int, String)) & _1 <%~ show) `shouldBe` ("1", ("1", "hello"))
    it "works with <<%~ and different types" $ do
      (((1, "hello") :: (Int, String)) & _1 <<%~ show) `shouldBe` (1, ("1", "hello"))
