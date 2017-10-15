{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Blaze.ByteString.Builder (toByteString)
import           CMark                    as C
import           Control.Applicative      ((<|>))
import           Control.Lens.Lens        ((&))
import           Control.Lens.Operators   ((.~))
import           Control.Monad.IO.Class   (liftIO)
import           Data.Binary.Builder      (Builder)
import           Data.ByteString          as BS
import qualified Data.ByteString.UTF8     as BU8
import           Data.List                as L
import           Data.String.Conversions  (convertString)
import           Data.Text                as T
import           Heist                    (HeistConfig, HeistState, MIMEType,
                                           defaultInterpretedSplices,
                                           emptyHeistConfig,
                                           hcInterpretedSplices, hcNamespace,
                                           hcTemplateLocations, initHeist,
                                           loadTemplates)
import           Heist.Interpreted        (Splice, bindSplice, renderTemplate,
                                           textSplice)
import           Lib
import           Snap                     (Snap, getParam, ifTop,
                                           quickHttpServe, redirect, route,
                                           writeBS)
import           System.Directory         (doesDirectoryExist, doesFileExist,
                                           listDirectory)
import           System.Environment       (getArgs)
import           System.FilePath          (joinPath)
import           Text.XmlHtml             (docContent, parseHTML)

main :: IO ()
main = do
  args <- getArgs
  let docdir = L.head args
  quickHttpServe (site docdir)

site :: FilePath -> Snap ()
site docdir =
  ifTop (handleTop docdir) <|>
  route [("/:path", pathHandler docdir)]

renderSimple :: Splice IO -> IO (Maybe (Builder, MIMEType))
renderSimple mainSplice = do
  state <- heistState
  let newState = bindSplice "main" mainSplice state
  renderTemplate newState "simple"

heistState :: IO (HeistState IO)
heistState = do
  errOrConfig <- initHeist heistConfig
  case errOrConfig of
    Left errs -> error ("errors: " ++ L.unlines errs)
    Right con -> return con

heistConfig :: HeistConfig IO
heistConfig = emptyHeistConfig
  & hcTemplateLocations .~ [ loadTemplates "templates" ]
  & hcInterpretedSplices .~ defaultInterpretedSplices
  & hcNamespace .~ ""

handleTop :: FilePath -> Snap ()
handleTop = buildContent

joinPaths :: ByteString -> IO String
joinPaths path = joinWithComma <$> listDirectory (BU8.toString path)

joinWithComma :: [String] -> String
joinWithComma = L.intercalate ","

pathHandler :: FilePath -> Snap ()
pathHandler docdir = do
  param <- getParam "path"
  case param of
    Nothing -> writeBS "N/A"
    Just x -> buildContent $ getFilePath docdir (BU8.toString x)

buildContent :: FilePath -> Snap ()
buildContent filepath = do
  isDir <- liftIO . doesDirectoryExist $ filepath
  content <- liftIO $ if isDir
               then buildDirContent filepath
               else buildFileContent filepath
  writeBS content

buildFileContent :: FilePath -> IO ByteString
buildFileContent filepath = do
  content <- toHtmlBS . convertString $ filepath
  Just (output, _) <- renderSimple $ htmlSplice content
  return $ toByteString output

htmlSplice :: ByteString -> Splice IO
htmlSplice content = case parseHTML "" content of
  Left err -> error err
  Right doc -> return $ docContent doc

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
