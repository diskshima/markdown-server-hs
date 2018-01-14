module DirWatch
  ( watchDirectory
  )
  where

import           Control.Concurrent (threadDelay)
import           Control.Monad      (forever)
import           System.FSNotify    (Event (..), watchDir, withManager)

type Action = FilePath -> IO ()

callback :: Action -> Event -> IO ()
callback action (Added filepath _)    = action filepath
callback action (Modified filepath _) = action filepath
callback action (Removed filepath _)  = action filepath

watchDirectory :: FilePath -> Action -> IO ()
watchDirectory filepath action =
  withManager $ \mgr -> do
    print $ "Watching " ++ filepath
    watchDir mgr filepath (const True) (callback action)
    forever $ threadDelay 5000000
