module Main where

import Control.Monad (forM_, when)
import Task4 (newCHT, putCHT, sizeCHT)
import Control.Exception (Exception, throwIO)
import Control.Monad.Trans (lift)
import Task5 (allocate, release, runAllocateT, tryAll)

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

test3 :: IO ()
test3 = do
  let inds = [1 .. 10000] :: [Int]
  ht <- newCHT
  forM_ inds $ \i -> do
    putCHT ("Hello" ++ show (i + 10 + 1337)) (i * 10) ht
    when (i `mod` 1000 == 0) $ putStrLn $ "Iteration " ++ show i
  endSize <- sizeCHT ht
  print endSize

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
