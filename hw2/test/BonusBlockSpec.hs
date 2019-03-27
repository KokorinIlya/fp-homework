module BonusBlockSpec
  ( contSpec
  , monadContSpec
  , trivialContSpec
  , kernelPlaygroundSpec
  ) where

import Test.Hspec (SpecWith, describe, it, shouldBe)
import BonusBlock (Cont (..), kernelPlayground, example, forkExample, yieldExample, longForkExample)

trivialContSpec :: SpecWith ()
trivialContSpec =
  describe "CPS" $ do
    it "should make computations correctly" $ do
      pythagoras 3 4 id `shouldBe` "Result is 25"
  where
    square :: Int -> (Int -> r) -> r
    square x onComplete = onComplete $ x * x
    add :: Int -> Int -> (Int -> r) -> r
    add x y onComplete = onComplete $ x + y
    toString :: Int -> (String -> r) -> r
    toString x onComplete = onComplete $ "Result is " ++ show x
    pythagoras :: Int -> Int -> (String -> r) -> r
    pythagoras x y onComplete = square x $ \x2 -> square y $ \y2 -> add x2 y2 $ \s -> toString s onComplete

contSpec :: SpecWith ()
contSpec =
  describe "BonusBlock.Cont" $ do
    it "should make computations like in traditional CPS" $ do
      runCont (pythagoras 3 4) id `shouldBe` "Result is 25"
  where
    square :: Int -> Cont r Int
    square x = Cont $ \onComplete -> onComplete $ x * x

    add :: Int -> Int -> Cont r Int
    add x y = Cont $ \onComplete -> onComplete $ x + y

    toString :: Int -> Cont r String
    toString x = Cont $ \onComplete -> onComplete $ "Result is " ++ show x

    pythagoras :: Int -> Int -> Cont r String
    pythagoras x y = Cont $ \onComplete ->
      runCont (square x) $ \x2 ->
      runCont (square y) $ \y2 ->
      runCont (add x2 y2) $ \s ->
      runCont (toString s) onComplete

monadContSpec :: SpecWith ()
monadContSpec =
  describe "BonusBlock.Cont" $ do
    it "should make computations using bind" $ do
      runCont (pythagoras 3 4) id `shouldBe` "Result is 25"

    it "should make computations using do-notation" $ do
      runCont (doPythagoras 3 4) id `shouldBe` "Result is 25"

    it "should make computations using <*>" $ do
      runCont (doComputational <$> computeX <*> computeY <*> computeZ <*> computeV) id `shouldBe` 16

  where
    square :: Int -> Cont r Int
    square x = return $ x * x

    add :: Int -> Int -> Cont r Int
    add x y = return $ x + y

    toString :: Int -> Cont r String
    toString x = return $ "Result is " ++ show x

    pythagoras :: Int -> Int -> Cont r String
    pythagoras x y = square x >>= \x2 -> square y >>= \y2 -> add x2 y2 >>= \s -> toString s

    doPythagoras :: Int -> Int -> Cont r String
    doPythagoras x y = do
      x2 <- square x
      y2 <- square y
      s <- add x2 y2
      toString s

    doComputational :: Int -> Int -> Int -> Int -> Int
    doComputational x y z v = (x + y - z) * v

    computeX :: Cont r Int
    computeX = return 2

    computeY :: Cont r Int
    computeY = return 3

    computeZ :: Cont r Int
    computeZ = return 1

    computeV :: Cont r Int
    computeV = return 4

kernelPlaygroundSpec :: SpecWith ()
kernelPlaygroundSpec =
  describe "BonusBlock.kernelPlayground" $ do
    it "processes trivial sequence of actions" $ do
      kernelPlayground example "name1\nname2\nname3" `shouldBe` "Hello, name1!\nAlso, hello, name2!\n"
    it "processes fork actions" $ do
      kernelPlayground forkExample "name1\nname2" `shouldBe` "Hello from child process\nHello, name1, from parent process\n"
    it "processes fork-yield actions" $ do
      kernelPlayground yieldExample "name1\nname2" `shouldBe` "Hello from child process, child process is yielding\nHello, name1, from parent process\nChild process is back!\n"
    it "run processes in sequence, without yields" $ do
      kernelPlayground longForkExample "name1\nname2" `shouldBe` "Hello from child process\nChild process continues running\nHello, name1, from parent process\n"
