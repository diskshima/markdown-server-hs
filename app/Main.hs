{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative    ((<|>))
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString        (ByteString, unpack)
import qualified Data.ByteString.Char8  as B8
import           Data.List              (intercalate)
import           Lib
import           Snap                   (Snap, getParam, ifTop, quickHttpServe,
                                         redirect, route, writeBS)
import           System.Directory       (listDirectory)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
  ifTop (handleTop "/") <|>
  route [("/:path", pathHandler)]

handleTop :: ByteString -> Snap ()
handleTop path = do
  joinedPaths <- liftIO $ joinPaths path
  writeBS $ B8.pack joinedPaths

joinPaths :: ByteString -> IO String
joinPaths path = joinWithComma <$> listDirectory (B8.unpack path)

joinWithComma :: [String] -> String
joinWithComma = intercalate ","

pathHandler :: Snap ()
pathHandler = do
  param <- getParam "path"
  maybe (writeBS "root!!")
        writeBS param
