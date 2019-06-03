{-# LANGUAGE RecordWildCards #-}

module Task6
  ( FS(..)
  , scanDirectory'
  , name
  , isDir
  , isFile
  , _Dir
  , _File
  ) where

import Lens.Micro (Lens', Traversal', lens)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (splitDirectories, takeFileName, (</>))

data FS
  = Dir { _name     :: FilePath -- название папки, не полный путь
        , _contents :: [FS] }
  | File { _name :: FilePath -- название файла, не полный путь
          }
  deriving (Show)

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast [x]    = Just x
safeLast (_:xs) = safeLast xs

scanDirectory' :: FilePath -> IO (Maybe FS)
scanDirectory' path = do
  fileExists <- doesFileExist path
  if fileExists
    then return $ Just $ File (takeFileName path)
    else do
      directoryExists <- doesDirectoryExist path
      if directoryExists
        then do
          let maybeDirName = safeLast $ splitDirectories path
          case maybeDirName of
            Nothing -> return Nothing
            Just dirName -> do
              directoryEntries <- listDirectory path
              subTrees <- parseSubdirectories path directoryEntries
              return $ Just $ Dir dirName subTrees
        else return Nothing
  where
    parseSubdirectories :: FilePath -> [FilePath] -> IO [FS]
    parseSubdirectories _ [] = return []
    parseSubdirectories basePath (curSubdir:otherSubdirs) = do
      maybeTree <- scanDirectory' $ basePath </> curSubdir
      case maybeTree of
        Nothing      -> parseSubdirectories basePath otherSubdirs
        Just curTree -> (curTree :) <$> parseSubdirectories basePath otherSubdirs

name :: Lens' FS FilePath
name = lens nameGetter nameSetter
  where
    nameGetter :: FS -> FilePath
    nameGetter Dir {_name = dirName}   = dirName
    nameGetter File {_name = fileName} = fileName
    nameSetter :: FS -> FilePath -> FS
    nameSetter dir@Dir {..} newDirName    = dir {_name = newDirName}
    nameSetter file@File {..} newFileName = file {_name = newFileName}

isFile :: FS -> Bool
isFile File {..} = True
isFile Dir {..}  = False

isDir :: FS -> Bool
isDir Dir {..}  = True
isDir File {..} = False

-- Should be prism, but profunctor optics are too painfull
_Dir :: Traversal' FS (FilePath, [FS])
_Dir f (Dir dirName dirContents) = uncurry Dir <$> f (dirName, dirContents)
_Dir _ file                      = pure file

_File :: Traversal' FS FilePath
_File f (File fileName) = File <$> f fileName
_File _ dir             = pure dir
