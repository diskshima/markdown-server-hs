module FileUtils
  (
    addSlashToDir,
    isFilePathDirectory,
    sortDirsFirst,
  ) where

import           Data.List                 (isSuffixOf, sortBy)
import qualified Data.Ord                  as Ordering
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

endsWithSlash :: P.FilePath -> Bool
endsWithSlash = isSuffixOf "/"

sortDirsFirst :: [P.FilePath] -> [P.FilePath]
sortDirsFirst = sortBy (\fp1 fp2 ->
  if endsWithSlash fp1
    then
      if endsWithSlash fp2
        then compare fp1 fp2
        else Ordering.LT
   else
     if endsWithSlash fp2
       then Ordering.GT
       else compare fp1 fp2)
