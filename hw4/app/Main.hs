{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, readMVar, threadDelay)
import Control.Exception (Exception, throwIO)
import Control.Monad (forM_, when)
import Control.Monad.Trans (lift)
import Task4 (ConcurrentHashTable, newCHT, putCHT, sizeCHT)
import Task5 (AllocateT, allocate, release, resourceFork, runAllocateT, tryAll)

data Dummy =
  Dummy
  deriving (Show)

instance Exception Dummy

main :: IO ()
main = do
  _ <- tryAll test1
  putStrLn "___"
  test2
  putStrLn "___"
  test3
  putStrLn "___"
  _ <- tryAll test4
  putStrLn "___"
  test5
  putStrLn "___"
  test6
  putStrLn "___"
  _ <- tryAll test7
  putStrLn "___"
  test8
  putStrLn "___"
  test9
  putStrLn "___"
  test10
  putStrLn "___"
  test11
  putStrLn "___"

test1 :: IO ()
test1 =
  runAllocateT $ do
    (_, _) <- allocate (putStrLn "A aquired") (\_ -> putStrLn "A released")
    lift $ putStrLn "After a"
    (_, bKey) <- allocate (putStrLn "B aquired") (\_ -> putStrLn "B released")
    lift $ putStrLn "After b"
    release bKey
    (_, _) <- allocate (putStrLn "C aquired") (\_ -> throwIO Dummy)
    lift $ putStrLn "After c"
    _ <- lift $ throwIO Dummy
    lift $ putStrLn "finishing"

test2 :: IO ()
test2 =
  runAllocateT $ do
    (_, _) <- allocate (putStrLn "A aquired") (\_ -> putStrLn "A released")
    lift $ putStrLn "After a"
    (_, bKey) <- allocate (putStrLn "B aquired") (\_ -> putStrLn "B released")
    lift $ putStrLn "After b"
    release bKey
    (_, _) <- allocate (putStrLn "C aquired") (\_ -> putStrLn "C released")
    lift $ putStrLn "After c"
    lift $ putStrLn "finishing"

test3 :: IO ()
test3 = do
  let inds = [1 .. 10000] :: [Int]
  ht <- newCHT
  forM_ inds $ \i -> do
    putCHT ("Hello" ++ show (i + 10 + 1337)) (i * 10) ht
    when (i `mod` 1000 == 0) $ putStrLn $ "Iteration " ++ show i
  endSize <- sizeCHT ht
  print endSize

test4 :: IO ()
test4 =
  runAllocateT $ do
    (_, _) <- allocate (putStrLn "A aquired") (\_ -> putStrLn "A released")
    lift $ putStrLn "After a"
    (_, _) <- allocate (putStrLn "B aquired") (\_ -> putStrLn "B released")
    lift $ putStrLn "After b"
    (_, _) <- allocate (putStrLn "C aquired") (\_ -> putStrLn "C released")
    lift $ putStrLn "After c"
    _ <- lift $ throwIO Dummy
    lift $ putStrLn "finishing"

test5 :: IO ()
test5 = do
  let inds = [1000,2000 .. 5000] :: [Int]
  mvars <- getMVars 5
  ht <- newCHT
  forM_ (inds `zip` mvars) $ \(i, mvar) -> forkIO $ task ht i (i + 999) >> putMVar mvar 0
  readMVars mvars
  endSize <- sizeCHT ht
  print endSize
  where
    readMVars :: [MVar Int] -> IO ()
    readMVars [] = return ()
    readMVars (curMVar:otherMVars) = do
      !x <- readMVar curMVar
      print x
      readMVars otherMVars
    getMVars :: Int -> IO [MVar Int]
    getMVars n
      | n == 0 = return []
      | otherwise = do
        curMVar <- newEmptyMVar
        otherMVars <- getMVars $ n - 1
        return $ curMVar : otherMVars
    task :: ConcurrentHashTable String Int -> Int -> Int -> IO ()
    task ht l r = forM_ [l .. r] $ \i -> putCHT ("Hello" ++ show (i + 10 + 1337)) (i * 10) ht

test6 :: IO ()
test6 =
  runAllocateT $ do
    (_, _) <- allocate (putStrLn "A aquired") (\_ -> putStrLn "A released")
    lift $ putStrLn "After a"
    (_, bKey) <- allocate (putStrLn "B aquired") (\_ -> putStrLn "B released")
    lift $ putStrLn "After b"
    release bKey
    release bKey
    (_, _) <- allocate (putStrLn "C aquired") (\_ -> putStrLn "C released")
    lift $ putStrLn "After c"
    release bKey
    lift $ putStrLn "finishing"

test7 :: IO ()
test7 =
  runAllocateT $ do
    (_, _) <- allocate (putStrLn "A aquired") (\_ -> putStrLn "A released")
    lift $ putStrLn "After a"
    (_, bKey) <- allocate (putStrLn "B aquired") (\_ -> putStrLn "B released")
    lift $ putStrLn "After b"
    release bKey
    lift $ putStrLn "After releasing b"
    (_, _) <- allocate (throwIO Dummy) (\_ -> putStrLn "C released")
    lift $ putStrLn "After c"
    (_, _) <- allocate (putStrLn "D aquired") (\_ -> putStrLn "D released")
    lift $ putStrLn "After d"
    lift $ putStrLn "finishing"

myFork :: IO () -> IO ()
myFork action = () <$ forkIO action

test8 :: IO ()
test8 =
  runAllocateT $ do
    (_, _) <- allocate (putStrLn "A aquired") (\_ -> putStrLn "A released")
    lift $ putStrLn "After a"
    resourceFork
      myFork
      (allocate (putStrLn "C aquired") (\_ -> putStrLn "C released") >> lift (putStrLn "Finished helper thread"))
    lift $ putStrLn "finishing"

test9 :: IO ()
test9 =
  runAllocateT $ do
    (_, _) <- allocate (putStrLn "A aquired") (\_ -> putStrLn "A released")
    lift $ putStrLn "After a"
    (_, bKey) <- allocate (putStrLn "B aquired") (\_ -> putStrLn "B released")
    lift $ putStrLn "After b"
    (_, cKey) <- allocate (putStrLn "C aquired") (\_ -> putStrLn "C released")
    lift $ putStrLn "After c"
    release bKey
    resourceFork myFork forkFunction
    release cKey
    lift $ putStrLn "finished"
  where
    forkFunction :: AllocateT IO ()
    forkFunction = do
      (_, _) <- allocate (putStrLn "D aquired") (\_ -> putStrLn "D released")
      lift $ putStrLn "After d"
      resourceFork
        myFork
        (allocate (putStrLn "E aquired") (\_ -> putStrLn "E released") >> lift (putStrLn "Finished last thread"))
      lift $ putStrLn "Finished helper thread"

test10 :: IO ()
test10 =
  runAllocateT $ do
    (_, _) <- allocate (putStrLn "A aquired") (\_ -> putStrLn "A released")
    lift $ putStrLn "After a"
    (_, bKey) <- allocate (putStrLn "B aquired") (\_ -> putStrLn "B released")
    lift $ putStrLn "After b"
    (_, cKey) <- allocate (putStrLn "C aquired") (\_ -> putStrLn "C released")
    lift $ putStrLn "After c"
    release bKey
    resourceFork myFork forkFunction
    release cKey
    release cKey
    lift $ putStrLn "finished"
  where
    forkFunction :: AllocateT IO ()
    forkFunction = do
      (_, dKey) <- allocate (putStrLn "D aquired") (\_ -> putStrLn "D released")
      lift $ putStrLn "After d"
      resourceFork
        myFork
        (allocate (putStrLn "E aquired") (\_ -> putStrLn "E released") >> lift (putStrLn "Finished last thread"))
      lift $ threadDelay 1000000
      release dKey
      lift $ putStrLn "Finished helper thread"

test11 :: IO ()
test11 =
  runAllocateT $ do
    (_, _) <- allocate (putStrLn "A aquired") (\_ -> putStrLn "A released")
    lift $ putStrLn "After a"
    (_, bKey) <- allocate (putStrLn "B aquired") (\_ -> putStrLn "B released")
    lift $ putStrLn "After b"
    (_, cKey) <- allocate (putStrLn "C aquired") (\_ -> putStrLn "C released")
    lift $ putStrLn "After c"
    release bKey
    resourceFork myFork forkFunction
    release cKey
    release cKey
    lift $ putStrLn "finished"
  where
    forkFunction :: AllocateT IO ()
    forkFunction = do
      (_, _) <- allocate (putStrLn "D aquired") (\_ -> putStrLn "D released")
      lift $ putStrLn "After d"
      resourceFork
        myFork
        (allocate (putStrLn "E aquired") (\_ -> putStrLn "E released") >> lift (putStrLn "Finished last thread"))
      lift $ threadDelay 1000000
      lift $ putStrLn "Finished helper thread"
