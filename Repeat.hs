module Repeat
( repeatAt
) where

import Import
import Control.Concurrent(forkIO, threadDelay)
import Data.Time.Clock

repeatedAction :: Int -> IO a -> IO ()
repeatedAction delay action =
  void $ forkIO $ forever $ threadDelay delay >> action

repeatAt :: UTCTime -> IO a -> IO ()
repeatAt time action = do
  now <- getCurrentTime
  let diff = diffUTCTime time now
      delay = round (realToFrac diff :: Double) * 1000000
  void $ forkIO $ do
    threadDelay delay
    putStrLn "void action"
    void action
    putStrLn "repeatedAction delay action"
    repeatedAction delay action