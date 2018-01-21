{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Blaze.ByteString.Builder (toByteString)
import           Control.Applicative      ((<|>))
import           Control.Concurrent       (threadDelay)
import           Control.Lens.Lens        ((&))
import           Control.Lens.Operators   ((.~))
import           Control.Monad            (forever, when)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Binary.Builder      (Builder)
import           Data.ByteString          as BS
import           Data.ByteString.Char8    as BC8
import           Data.List                as L
import           Data.String.Conversions  (convertString)
import           Data.Text                (Text)
import           DirWatch                 (watchDirectoryTree)
import           FileUtils
import           Heist                    (HeistConfig, HeistState, MIMEType,
                                           defaultInterpretedSplices,
                                           emptyHeistConfig,
                                           hcInterpretedSplices, hcNamespace,
                                           hcTemplateLocations, initHeist,
                                           loadTemplates)
import           Heist.Interpreted        (Splice, bindSplice, renderTemplate,
                                           textSplice)
import           HtmlUtils
import           Network.URI.Encode       (decode)
import           Network.WebSockets       (PendingConnection, acceptRequest,
                                           receiveData, sendTextData)
import           Network.WebSockets.Snap  (runWebSocketsSnap)
import           Prelude                  as P
import           Snap                     (Handler, Snap, getParam, getRequest,
                                           ifTop, quickHttpServe, redirect,
                                           route, rqURI, writeBS)
import           System.Directory         (canonicalizePath, doesDirectoryExist,
                                           doesFileExist)
import           System.Environment       (getArgs)
import           System.FilePath          (joinPath)
import           Text.XmlHtml             (docContent, parseHTML)

main :: IO ()
main = do
  args <- getArgs
  let docdir = L.head args
  quickHttpServe (site docdir)

site :: P.FilePath -> Snap ()
site docdir =
  route [("/ws", webSocketsDriver docdir)] <|>
  pathHandler docdir

webSocketsDriver :: P.FilePath -> Snap ()
webSocketsDriver docdir = runWebSocketsSnap (wsApp docdir)

wsApp :: P.FilePath -> PendingConnection -> IO ()
wsApp docdir pending = do
  conn <- acceptRequest pending
  forever $ do
    path <- receiveData conn :: IO ByteString
    filepath <- canonicalizePath . joinPath $ [docdir, convertString path]
    P.putStrLn $ "Monitor request for " ++ filepath
    watchDirectoryTree docdir $ \fp ->
      when (filepath == fp) (sendTextData conn ("Updated" :: ByteString))

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

pathHandler :: P.FilePath -> Snap ()
pathHandler docdir = do
  filepath <- buildPath docdir
  buildContent (decode filepath)

buildPath :: P.FilePath -> Snap P.FilePath
buildPath docdir = do
  req <- getRequest
  let uriPath = rqURI req
      noRootPath = BC8.dropWhile (== '/') uriPath
  return $ joinPath [docdir, convertString noRootPath]

buildContent :: P.FilePath -> Snap ()
buildContent filepath = do
  isDir <- liftIO . doesDirectoryExist $ filepath
  content <- liftIO $ if isDir
               then buildDirContent filepath
               else buildFileContent filepath
  writeBS content

buildFileContent :: P.FilePath -> IO ByteString
buildFileContent filepath = do
  content <- toHtmlBS . convertString $ filepath
  Just (output, _) <- renderSimple $ htmlSplice content
  return $ toByteString output

htmlSplice :: ByteString -> Splice IO
htmlSplice content = case parseHTML "" content of
  Left err  -> error err
  Right doc -> return $ docContent doc

buildDirContent :: P.FilePath -> IO ByteString
buildDirContent filepath = do
  content <- buildDirListHtml $ convertString filepath
  Just (output, _) <- renderSimple $ htmlSplice content
  return $ toByteString output
