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
import           System.Directory        (listDirectory)

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
joinWithComma = L.intercalate ","

pathHandler :: Snap ()
pathHandler = do
  param <- getParam "path"
  case param of
    Nothing -> writeBS "N/A"
    Just x -> do
      content <- (liftIO . toHtmlBS) x
      writeBS content

toHtmlBS :: ByteString -> IO ByteString
toHtmlBS path = do
  content <- toHtml . convertString $ path
  return $ convertString content

toHtml :: FilePath -> IO Text
toHtml path = do
  content <- T.pack <$> Prelude.readFile path
  return $ C.commonmarkToHtml cmarkOpts content

cmarkOpts :: [C.CMarkOption]
cmarkOpts = [C.optNormalize, C.optSmart, C.optSafe]
