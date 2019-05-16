{-# LANGUAGE BangPatterns #-}
module Task4Spec
  ( hashTableSpec
  ) where

import Control.Monad (forM_)
import Task4 (ConcurrentHashTable, newCHT, putCHT, sizeCHT)

import Test.Hspec (SpecWith, describe, it, shouldBe)
import Control.Concurrent (MVar, readMVar, forkIO, newEmptyMVar, putMVar)

hashTableSpec :: SpecWith ()
hashTableSpec =
  describe "Task4.ConcurrentHashTable" $ do
    it "performs insertion from single thread" $ do
      size <- test1
      size `shouldBe` 10000
    it "performs insertion from multiple threads" $ do
      size <- test2
      size `shouldBe` 5000
    it "performs insertion of dublicates" $ do
      size <- test3
      size `shouldBe` 1
  where
    test1 = do
      let inds = [1 .. 10000] :: [Int]
      ht <- newCHT
      forM_ inds $ \i -> putCHT ("Hello" ++ show (i + 10 + 1337)) (i * 10) ht
      sizeCHT ht
    test2 = do
      let inds = [1000,2000 .. 5000] :: [Int]
      mvars <- getMVars 5
      ht <- newCHT
      forM_ (inds `zip` mvars) $ \(i, mvar) -> forkIO $ task ht i (i + 999) >> putMVar mvar ()
      readMVars mvars
      sizeCHT ht
      where
        readMVars :: [MVar ()] -> IO ()
        readMVars [] = return ()
        readMVars (curMVar:otherMVars) = do
          !_ <- readMVar curMVar
          readMVars otherMVars
        getMVars :: Int -> IO [MVar ()]
        getMVars n
          | n == 0 = return []
          | otherwise = do
            curMVar <- newEmptyMVar
            otherMVars <- getMVars $ n - 1
            return $ curMVar : otherMVars
        task :: ConcurrentHashTable String Int -> Int -> Int -> IO ()
        task ht l r = forM_ [l .. r] $ \i -> putCHT ("Hello" ++ show (i + 10 + 1337)) (i * 10) ht
    test3 = do
      let inds = [1 .. 10000] :: [Int]
      ht <- newCHT
      forM_ inds $ \i -> putCHT "Hello" (i * 10) ht
      sizeCHT ht
