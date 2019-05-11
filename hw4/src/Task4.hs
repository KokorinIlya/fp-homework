{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task4 where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TArray (TArray)
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
  , elements :: TVar (TArray Int (Maybe (k, v)))
  }

newCHT :: IO (ConcurrentHashTable k v)
newCHT =
  atomically $ do
    newTableSize <- newTVar 0
    newTableArray <- newArray (0, 4) Nothing
    newTableArrayPointer <- newTVar newTableArray
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
    curArray <- readTVar curArrayPointer
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

data InsertResult
  = SucessfullyInserted
  | ChangedValue
  | NeedsRehashing

putCHT :: (Hashable k, Eq k) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT k v t = atomically $ flip catchSTM (\(_ :: SomeException) -> return ()) $ putImpl k v t
  where
    putImpl :: (Hashable k, Eq k) => k -> v -> ConcurrentHashTable k v -> STM ()
    putImpl key value ConcurrentHashTable {size = curTableSizePtr, elements = curArrayPointer} = do
      let (Hash startIndex) = hashCode key
      curTableSize <- readTVar curTableSizePtr
      curArray <- readTVar curArrayPointer
      (!leftBorder, !rightBorder) <- getBounds curArray
      insertionResult <- insert key value curArray startIndex startIndex leftBorder rightBorder
      case insertionResult of
        SucessfullyInserted -> do
          let !newTableSize = curTableSize + 1
              !tableSize = fromIntegral $ rightBorder - leftBorder
          writeTVar curTableSizePtr newTableSize
          when (fromIntegral newTableSize * maxLoadFactor > tableSize) $ do
            let !curLength = rightBorder - leftBorder
                !newLength = 2 * curLength
            emptyArray <- newArray (0, newLength) Nothing :: STM (TArray Int (Maybe (k, v)))
            newSize <- newTVar 0
            newArrayPtr <- newTVar emptyArray
            let newTable = ConcurrentHashTable newSize newArrayPtr
            return ()
        ChangedValue -> return ()
        NeedsRehashing -> undefined
    insert :: (Hashable k, Eq k) => k -> v -> TArray Int (Maybe (k, v)) -> Int -> Int -> Int -> Int -> STM InsertResult
    insert key value array !curIndex !startIndex !leftBorder !rightBorder = do
      curElem <- readArray array curIndex
      case curElem of
        Nothing -> do
          writeArray array curIndex $ Just (key, value)
          return SucessfullyInserted
        Just (curKey, _) ->
          if curKey == key
            then do
              writeArray array curIndex $ Just (key, value)
              return ChangedValue
            else case getNextIndex curIndex startIndex leftBorder rightBorder of
                   Nothing -> return NeedsRehashing
                   Just nextIndex -> insert key value array nextIndex startIndex leftBorder rightBorder

sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT ConcurrentHashTable {size = curTableSize} = atomically $ readTVar curTableSize
