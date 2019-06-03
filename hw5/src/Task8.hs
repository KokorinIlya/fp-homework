{-# LANGUAGE RecordWildCards #-}

module Task8
  ( changeExtension
  , getAllChildren
  , rm
  ) where

import Lens.Micro (filtered, traversed, (%~), (&), (^.), (^..), _2)
import System.FilePath ((-<.>))
import Task6 (FS (..), isDir, isFile, name, _Dir)

changeExtension :: FilePath -> FS -> FS
changeExtension newExtension rootDirectory =
  rootDirectory & _Dir . _2 . traversed . filtered isFile %~ changeExtensionForSingleFile
  where
    changeExtensionForSingleFile :: FS -> FS
    changeExtensionForSingleFile curFile = curFile & name %~ (-<.> newExtension)

getAllChildren :: FS -> [FilePath]
getAllChildren fs = getFilenamesInCurrentDirectory ++ (getDirectoriesInCurrentDirectory >>= getAllChildren)
  where
    getFilenamesInCurrentDirectory :: [FilePath]
    getFilenamesInCurrentDirectory = fs ^.. _Dir . _2 . traversed . filtered isFile . name
    getDirectoriesInCurrentDirectory :: [FS]
    getDirectoriesInCurrentDirectory = fs ^.. _Dir . _2 . traversed . filtered isDir

rm :: FilePath -> FS -> FS
rm dirName = (& _Dir . _2 %~ filter dirIsRetained)
  where
    dirIsRetained :: FS -> Bool
    dirIsRetained File {..}                   = True
    dirIsRetained curDir@Dir {_contents = []} = curDir ^. name == dirName
    dirIsRetained Dir {_contents = _:_}       = True
