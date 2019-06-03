{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RecordWildCards #-}

module Task7
  ( cd
  , ls
  , file
  ) where

import Lens.Micro (Traversal', failing, filtered, traversed, (^.), _2)
import Task6 (FS (..), name, _Dir, _File)

cd :: FilePath -> Traversal' FS FS
cd pathToGo = _Dir . _2 . traversed . filtered (checkDir pathToGo)
  where
    checkDir :: FilePath -> FS -> Bool
    checkDir _ File {..}              = False
    checkDir pathToCheck dir@Dir {..} = dir ^. name == pathToCheck

ls :: Traversal' FS FilePath
ls = _Dir . _2 . traversed . name

file :: FilePath -> Traversal' FS FilePath
file fileName = failing lookupFileInDirectory lookupFileInFile
  where
    lookupFileInDirectory :: Traversal' FS FilePath
    lookupFileInDirectory = _Dir . _2 . traversed . filtered checkFile . name
    lookupFileInFile :: Traversal' FS FilePath
    lookupFileInFile = _File . filtered (== fileName)
    checkFile :: FS -> Bool
    checkFile curFile@File {..} = curFile ^. name == fileName
    checkFile Dir {..}          = False
