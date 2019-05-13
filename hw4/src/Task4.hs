{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task4 where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TArray (TArray)
import Control.Concurrent.STM.TMVar (TMVar, newTMVar, readTMVar)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Exception.Base (SomeException)
import Control.Monad (when)
import Control.Monad.STM (atomically, catchSTM)
import Data.Array.MArray (getBounds, newArray, readArray, writeArray)

newtype Hash a =
  Hash Int

class Hashable a where
  hashCode :: a -> Hash a

maxLoadFactor :: Double
maxLoadFactor = 0.75

data ConcurrentHashTable k v = ConcurrentHashTable
  { size     :: TVar Int
  , elements :: TMVar (TArray Int (Maybe (k, v)))
  }

newCHT :: IO (ConcurrentHashTable k v)
newCHT =
  atomically $ do
    newTableSize <- newTVar 0
    newTableArray <- newArray (0, 4) Nothing
    newTableArrayPointer <- newTMVar newTableArray
    return $ ConcurrentHashTable newTableSize newTableArrayPointer

getNextIndex :: Int -> Int -> Int -> Int -> Maybe Int
getNextIndex !curIndex !startIndex !leftBorder !rightBorder =
  let curNext =
        if curIndex + 1 == rightBorder
          then leftBorder
          else curIndex + 1
   in if curNext == startIndex
        then Nothing
        else Just curNext

getCHT ::
     forall k v. (Hashable k, Eq k)
  => k
  -> ConcurrentHashTable k v
  -> IO (Maybe v)
getCHT key ConcurrentHashTable {elements = curArrayPointer} =
  atomically $ do
    let (Hash startIndex) = hashCode key
    curArray <- readTMVar curArrayPointer
    (!leftBorder, !rightBorder) <- getBounds curArray
    findInCHT startIndex startIndex leftBorder rightBorder curArray
  where
    findInCHT :: Int -> Int -> Int -> Int -> TArray Int (Maybe (k, v)) -> STM (Maybe v)
    findInCHT curIndex startIndex leftBorder rightBorder array = do
      nextElement <- readArray array curIndex
      case nextElement of
        Nothing -> return Nothing
        Just (curKey, curValue) ->
          if curKey == key
            then return $ Just curValue
            else case getNextIndex curIndex startIndex leftBorder rightBorder of
                   Nothing        -> return Nothing
                   Just nextIndex -> findInCHT nextIndex startIndex leftBorder rightBorder array

putCHT :: (Hashable k, Eq k) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT k v t = undefined

sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT ConcurrentHashTable {size = curTableSize} = atomically $ readTVar curTableSize
