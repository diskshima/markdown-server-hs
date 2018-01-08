module FileUtils
  (
    addSlashToDir,
    isFilePathDirectory,
  ) where

import           Data.String.Conversions   (convertString)
import           Filesystem                (isDirectory)
import           Filesystem.Path.CurrentOS (FilePath, decodeString)
import           Prelude                   as P
import           System.FilePath           (joinPath)

addSlashToDir :: P.FilePath -> P.FilePath -> IO P.FilePath
addSlashToDir baseDir dirOrFile = do
  let fullpath = joinPath [baseDir, dirOrFile]
  isDir <- isFilePathDirectory fullpath
  return $ if isDir
    then dirOrFile ++ "/"
    else dirOrFile

isFilePathDirectory :: P.FilePath -> IO Bool
isFilePathDirectory = isDirectory . decodeString
