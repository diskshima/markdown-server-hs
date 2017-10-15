{-# LANGUAGE OverloadedStrings #-}
module Main where

import           CMark                   as C
import           Control.Applicative     ((<|>))
import           Control.Monad.IO.Class  (liftIO)
import           Data.ByteString         as BS
import qualified Data.ByteString.Char8   as B8
import           Data.List               as L
import           Data.String.Conversions (convertString)
import           Data.Text               as T
import           Lib
import           Snap                    (Snap, getParam, ifTop, quickHttpServe,
                                          redirect, route, writeBS)
import           System.Directory        (doesDirectoryExist, doesFileExist,
                                          listDirectory)
import           System.Environment      (getArgs)
import           System.FilePath         (joinPath)

main :: IO ()
main = do
  args <- getArgs
  let docdir = L.head args
  quickHttpServe (site docdir)

site :: FilePath -> Snap ()
site docdir =
  ifTop (handleTop "/") <|>
  route [("/:path", pathHandler docdir)]

handleTop :: ByteString -> Snap ()
handleTop path = do
  joinedPaths <- liftIO $ joinPaths path
  writeBS $ B8.pack joinedPaths

joinPaths :: ByteString -> IO String
joinPaths path = joinWithComma <$> listDirectory (B8.unpack path)

joinWithComma :: [String] -> String
joinWithComma = L.intercalate ","

pathHandler :: FilePath -> Snap ()
pathHandler docdir = do
  param <- getParam "path"
  case param of
    Nothing -> writeBS "N/A"
    Just x -> do
      content <- buildContent filepath
      writeBS content
      where
        filepath = getFilePath docdir (B8.unpack x)

buildContent :: FilePath -> Snap ByteString
buildContent filepath = do
  isDir <- liftIO . doesDirectoryExist $ filepath
  liftIO $ if isDir
             then buildDirContent filepath
             else buildFileContent filepath

buildFileContent :: FilePath -> IO ByteString
buildFileContent = toHtmlBS . convertString

buildDirContent :: FilePath -> IO ByteString
buildDirContent filepath = do
  content <- joinPaths $ convertString filepath
  return $ convertString content

getFilePath :: FilePath -> FilePath -> FilePath
getFilePath a b = joinPath [a, b]

toHtmlBS :: ByteString -> IO ByteString
toHtmlBS path = do
  content <- toHtml $ convertString path
  return $ convertString content

toHtml :: FilePath -> IO Text
toHtml path = do
  content <- T.pack <$> Prelude.readFile path
  return $ C.commonmarkToHtml cmarkOpts content

cmarkOpts :: [C.CMarkOption]
cmarkOpts = [C.optNormalize, C.optSmart, C.optSafe]
