{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task4
  ( ConcurrentHashTable(..)
  , newCHT
  , getCHT
  , putCHT
  , sizeCHT
  ) where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TArray (TArray)
import Control.Concurrent.STM.TMVar (TMVar, newTMVar, putTMVar, readTMVar, takeTMVar)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Monad (forM_, when)
import Control.Monad.STM (atomically)
import Data.Array.MArray (getBounds, newArray, readArray, writeArray)
import Data.Hashable (Hashable (..))

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
    newTableArray <- newArray (0, 8) Nothing
    newTableArrayPointer <- newTMVar newTableArray
    return $ ConcurrentHashTable newTableSize newTableArrayPointer

getCHT ::
     forall k v. (Hashable k, Eq k)
  => k
  -> ConcurrentHashTable k v
  -> IO (Maybe v)
getCHT key ConcurrentHashTable {elements = curArrayPointer} =
  atomically $ do
    curArray <- readTMVar curArrayPointer
    (!leftBorder, !rightBorder) <- getBounds curArray
    let !startIndex = hash key `mod` rightBorder
    findInCHT startIndex startIndex leftBorder rightBorder curArray
  where
    findInCHT :: Int -> Int -> Int -> Int -> TArray Int (Maybe (k, v)) -> STM (Maybe v)
    findInCHT !curIndex !startIndex !leftBorder !rightBorder array = do
      nextElement <- readArray array curIndex
      case nextElement of
        Nothing -> return Nothing
        Just (curKey, curValue) ->
          if curKey == key
            then return $ Just curValue
            else case getNextIndex curIndex startIndex leftBorder rightBorder of
                   Nothing        -> return Nothing
                   Just nextIndex -> findInCHT nextIndex startIndex leftBorder rightBorder array
    getNextIndex :: Int -> Int -> Int -> Int -> Maybe Int
    getNextIndex !curIndex !startIndex !leftBorder !rightBorder =
      let curNext =
            if curIndex + 1 == rightBorder
              then leftBorder
              else curIndex + 1
       in if curNext == startIndex
            then Nothing
            else Just curNext

data InsertionResult
  = InsertedNewKey
  | ChangedOldKey

putCHT :: (Hashable k, Eq k) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT k v ConcurrentHashTable {size = curSizePointer, elements = curArrayPointer} =
  atomically $ do
    curElementCount <- readTVar curSizePointer
    curArray <- readTMVar curArrayPointer
    (!leftBorder, !rightBorder) <- getBounds curArray
    insertionResult <- addToArray k v leftBorder rightBorder curArray
    case insertionResult of
      InsertedNewKey -> do
        let !oldSize = rightBorder - leftBorder
        let !newElemCount = curElementCount + 1
        when (fromIntegral newElemCount >= maxLoadFactor * fromIntegral oldSize) $ do
          let !newLeftBound = 0
          let !newRightBound = 2 * oldSize
          _ <- takeTMVar curArrayPointer
          arrayToInsert <- newArray (newLeftBound, newRightBound) Nothing
          rehashImpl leftBorder rightBorder newLeftBound newRightBound curArray arrayToInsert
          putTMVar curArrayPointer arrayToInsert
        writeTVar curSizePointer newElemCount
      ChangedOldKey -> return ()
  where
    addToArray :: (Hashable k, Eq k) => k -> v -> Int -> Int -> TArray Int (Maybe (k, v)) -> STM InsertionResult
    addToArray keyToAdd valueToAdd !leftBorder !rightBorder arrayToAdd = do
      let !startIndex = hash keyToAdd `mod` rightBorder
      insertImpl keyToAdd valueToAdd startIndex leftBorder rightBorder arrayToAdd
    rehashImpl ::
         (Hashable k, Eq k)
      => Int
      -> Int
      -> Int
      -> Int
      -> TArray Int (Maybe (k, v))
      -> TArray Int (Maybe (k, v))
      -> STM ()
    rehashImpl oldLeftBound oldRightBound newLeftBound newRightBound arrayToCopy arrayToInsert =
      forM_ [oldLeftBound .. oldRightBound] $ \i -> do
        maybeElem <- readArray arrayToCopy i
        case maybeElem of
          Nothing -> return ()
          Just (keyToCopy, valueToCopy) -> do
            _ <- addToArray keyToCopy valueToCopy newLeftBound newRightBound arrayToInsert
            return ()
    insertImpl :: (Hashable k, Eq k) => k -> v -> Int -> Int -> Int -> TArray Int (Maybe (k, v)) -> STM InsertionResult
    insertImpl keyToAdd valueToAdd !curIndex !leftBorder !rightBorder array = do
      maybeCurElement <- readArray array curIndex
      case maybeCurElement of
        Nothing -> do
          writeArray array curIndex $ Just (keyToAdd, valueToAdd)
          return InsertedNewKey
        Just (curKey, _) ->
          if curKey == keyToAdd
            then do
              writeArray array curIndex $ Just (keyToAdd, valueToAdd)
              return ChangedOldKey
            else let !nextIndex = getNextIndex curIndex leftBorder rightBorder
                  in insertImpl keyToAdd valueToAdd nextIndex leftBorder rightBorder array
    getNextIndex :: Int -> Int -> Int -> Int
    getNextIndex !curIndex !leftBorder !rightBorder
      | curIndex + 1 == rightBorder = leftBorder
      | otherwise = curIndex + 1

sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT ConcurrentHashTable {size = curTableSize} = atomically $ readTVar curTableSize
