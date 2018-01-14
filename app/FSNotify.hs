module Main where

import           DirWatch
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  watchDirectory (head args) (\fp -> print $ "Action on " ++ fp)
