module HtmlUtils
  (
    wrapWithAnchor,
    joinAsList,
    buildDirListHtml,
    toHtmlBS,
  ) where

import           CMark                   as C
import           Data.ByteString         as BS
import           Data.ByteString.UTF8    as BU8
import           Data.List               as L
import           Data.String.Conversions (convertString)
import           Data.Text               as T
import           FileUtils
import           Prelude                 as P
import           System.Directory        (listDirectory)
import           Text.Printf             (printf)

wrapWithAnchor :: String -> String
wrapWithAnchor str = printf "<a href=\"./%s\">%s</a>" str str

joinAsList :: [String] -> String
joinAsList strs = "<ul><li>" ++ L.intercalate "</li><li>" (L.map wrapWithAnchor strs) ++ "</li></ul>"

buildDirListHtml :: ByteString -> IO ByteString
buildDirListHtml path = do
  let strPath = BU8.toString path
  dirs <- listDirectory strPath
  slashedDirs <- mapM (addSlashToDir strPath) dirs
  return $ convertString . joinAsList $ slashedDirs

toHtmlBS :: ByteString -> IO ByteString
toHtmlBS path = do
  content <- toHtml $ convertString path
  return $ convertString content

toHtml :: P.FilePath -> IO Text
toHtml path = do
  content <- T.pack <$> P.readFile path
  return $ C.commonmarkToHtml cmarkOpts content

cmarkOpts :: [C.CMarkOption]
cmarkOpts = [C.optNormalize, C.optSmart, C.optSafe]
