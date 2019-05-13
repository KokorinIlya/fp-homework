{-# LANGUAGE BangPatterns #-}

module Task3
  ( verifySolution
  , gauss
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Control.Parallel.Strategies (Eval, rpar, runEval)
import Data.Bits (xor)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zipWithIndexImpl 0
  where
    zipWithIndexImpl :: Int -> [a] -> [(Int, a)]
    zipWithIndexImpl _ []     = []
    zipWithIndexImpl n (x:xs) = (n, x) : zipWithIndexImpl (n + 1) xs

listToMVector :: [Bool] -> ST s (UMV.STVector s Bool)
listToMVector = UV.unsafeThaw . UV.fromList

createMatrix :: [[Bool]] -> Int -> ST s (MV.STVector s (UMV.STVector s Bool))
createMatrix a m = do
  external <- MV.new m :: ST s (MV.STVector s (UMV.STVector s Bool))
  mVectors <- traverse listToMVector a
  forM_ (zipWithIndex mVectors) $ uncurry (MV.unsafeWrite external)
  return external

gauss :: [[Bool]] -> [Bool] -> Maybe [Bool]
gauss [] _ = Nothing
gauss _ [] = Nothing
gauss a@(firstS:_) y@(_:_) =
  runST $ do
    let m = length a
    let n = length firstS
    aV <- createMatrix a m
    yV <- listToMVector y
    wh <- UMV.replicate n (-1) :: ST s (UMV.STVector s Int)
    gaussImpl aV yV m n 0 0 wh
    solution <- getSolution yV n 0 wh
    if verifySolution a y solution
      then return $ Just solution
      else return Nothing
  where
    gaussImpl ::
         MV.STVector s (UMV.STVector s Bool)
      -> UMV.STVector s Bool
      -> Int
      -> Int
      -> Int
      -> Int
      -> UMV.STVector s Int
      -> ST s ()
    gaussImpl aV yV !m !n !row !col wh =
      when (row < m && col < n) $ do
        maybeTrueIndex <- findTrue aV m col row
        case maybeTrueIndex of
          Nothing -> gaussImpl aV yV m n row (col + 1) wh
          Just trueIndex -> do
            MV.swap aV trueIndex row
            UMV.swap yV trueIndex row
            UMV.write wh col row
            forM_ [0 .. m - 1] $ \i ->
              when (i /= row) $ do
                curRow <- MV.read aV row
                rowToXor <- MV.read aV i
                startElem <- UMV.read rowToXor col
                when startElem $ do
                  forM_ [0 .. n - 1] $ \j -> do
                    curElem <- UMV.read curRow j
                    UMV.modify rowToXor (`xor` curElem) j
                  curY <- UMV.read yV row
                  UMV.modify yV (`xor` curY) i
            gaussImpl aV yV m n (row + 1) (col + 1) wh
    findTrue :: MV.STVector s (UMV.STVector s Bool) -> Int -> Int -> Int -> ST s (Maybe Int)
    findTrue aV !m !col !curIndex
      | curIndex == m = return Nothing
      | otherwise = do
        curRow <- MV.read aV curIndex
        curValue <- UMV.read curRow col
        if curValue
          then return $ Just curIndex
          else findTrue aV m col (curIndex + 1)
    getSolution :: UMV.STVector s Bool -> Int -> Int -> UMV.STVector s Int -> ST s [Bool]
    getSolution yV !n !curVariableNum wh
      | curVariableNum == n = return []
      | otherwise = do
        curWhere <- UMV.read wh curVariableNum
        if curWhere /= -1
          then (:) <$> UMV.read yV curWhere <*> getSolution yV n (curVariableNum + 1) wh
          else (False :) <$> getSolution yV n (curVariableNum + 1) wh

verifySolution :: [[Bool]] -> [Bool] -> [Bool] -> Bool
verifySolution a y x = runEval $ verifySolutionImpl (zip a y) x
  where
    verifySolutionImpl :: [([Bool], Bool)] -> [Bool] -> Eval Bool
    verifySolutionImpl [] _ = return True
    verifySolutionImpl ((aHead, yHead):tailToVerify) xs = do
      !curRes <- rpar $ verifyString (zip aHead xs) yHead False
      !others <- verifySolutionImpl tailToVerify xs
      return $ curRes && others
    verifyString :: [(Bool, Bool)] -> Bool -> Bool -> Bool
    verifyString [] !curY !acc = curY == acc
    verifyString ((!curA, !curX):stringTail) !curY !acc =
      let !curRes = curA && curX
          !newAcc = curRes `xor` acc
       in verifyString stringTail curY newAcc
