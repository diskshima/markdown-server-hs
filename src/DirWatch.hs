module DirWatch
  ( watchDirectoryTree
  )
  where

import           Control.Concurrent (threadDelay)
import           Control.Monad      (forever)
import           System.FSNotify    (Event (..), watchTree, withManager)

type Action = FilePath -> IO ()

callback :: Action -> Event -> IO ()
callback action (Added filepath _)    = action filepath
callback action (Modified filepath _) = action filepath
callback action (Removed filepath _)  = action filepath

watchDirectoryTree :: FilePath -> Action -> IO ()
watchDirectoryTree filepath action =
  withManager $ \mgr -> do
    print $ "Watching " ++ filepath
    watchTree mgr filepath (const True) (callback action)
    forever $ threadDelay 5000000
