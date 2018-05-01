{-# LANGUAGE OverloadedStrings #-}

module Kubernetes.WSClient
    (
      -- * Client connection 
      kClient
      -- * App
      , runClient
      , readLine
      , readResize
      , readStdIn
      , readStdOut 
      , readStdErr
      , readErr
      , isOpen
    )
  where 

import Control.Exception
import Control.Concurrent(forkIO)
import Control.Concurrent.STM
import Control.Monad (forever, unless)
import Data.Text (Text)
import Network.Socket(withSocketsDo)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import System.Timeout
import Kubernetes.Util



isOpen :: WS.Connection -> IO Bool 
isOpen = undefined



_timeout :: TimeoutInterval 
_timeout = 30 * 1000000
runClient :: String -> Int -> String -> IO ([Text], [Text], [Text], [Text], [Text])
runClient domain port route = 
  withSocketsDo $ WS.runClient domain port route (\c -> kClient c _timeout)

type TimeoutInterval = Int

kClient :: WS.Connection -> TimeoutInterval -> IO ([Text], [Text], [Text], [Text], [Text])
kClient conn timeoutInterval = do
    c1 <- atomically newTChan
    c2 <- atomically newTChan 
    c3 <- atomically newTChan 
    c4 <- atomically newTChan 
    c5 <- atomically newTChan
    let channels = zip allChannels [c1, c2, c3, c4, c5]
    rcv <- timeout timeoutInterval $ forkIO $ forever $ do
        msg <- (WS.receiveData conn) `catch` (\e@(SomeException s) -> return "")
        writeMsg channels $ T.splitAt 1 msg
    WS.sendClose conn ("Bye!" :: T.Text)
    drainChannels channels

drainChannels :: [(ChannelId, TChan Text)] -> IO ([Text], [Text], [Text], [Text], [Text])
drainChannels channels = do 
  inBuf <- channelReader $ snd $ getChannelIdSTM StdIn channels
  outBuf <- channelReader $ snd $ getChannelIdSTM StdOut channels
  stdErrBuf <- channelReader $ snd $ getChannelIdSTM StdErr channels
  errBuf <- channelReader $ snd $ getChannelIdSTM Error channels
  resizeBuf <- channelReader $ snd $ getChannelIdSTM Resize channels
  return (inBuf, outBuf, stdErrBuf, errBuf, resizeBuf)

channelReader :: TChan Text -> IO [Text]
channelReader inChan = do 
  loop inChan []
  where 
    loop inChan l@(h : t) = do 
      nextItem <- atomically . readTChan $ inChan
      emptyTChan <- atomically . isEmptyTChan $ inChan
      if (emptyTChan) then
        return l
      else
        loop inChan (nextItem : l)



writeMsg :: [(ChannelId, TChan Text)] -> (Text, Text) -> IO ()
writeMsg channels (channel, message) = do 
  let chanId = readChannel channel
  case chanId of
    Nothing -> throwIO $ InvalidChannel channel
    Just aChan -> 
      mapM_ 
        (\(x, y) -> atomically $ writeTChan y message) 
          $ filter (\(x, y) -> x == aChan) channels

getChannelIdSTM :: ChannelId -> [(ChannelId, TChan Text)] -> (ChannelId, TChan Text)
getChannelIdSTM aChannelId channels = 
  head $ filter(\(x, _) -> x == aChannelId) channels

readStdInSTM :: [(ChannelId, TChan Text)] -> STM Text
readStdInSTM channels = readTChan $ snd $ getChannelIdSTM StdIn channels

readStdIn :: [(ChannelId, TChan Text)] -> IO Text 
readStdIn = atomically . readStdInSTM

readStdOutSTM :: [(ChannelId, TChan Text)] -> STM Text
readStdOutSTM channels = readTChan $ snd $ getChannelIdSTM StdOut channels

readStdOut :: [(ChannelId, TChan Text)] -> IO Text 
readStdOut = atomically . readStdOutSTM

readStdErrSTM :: [(ChannelId, TChan Text)] -> STM Text
readStdErrSTM channels = readTChan $ snd $ getChannelIdSTM StdErr channels

readStdErr :: [(ChannelId, TChan Text)] -> IO Text 
readStdErr = atomically . readStdErrSTM

readErrSTM :: [(ChannelId, TChan Text)] -> STM Text 
readErrSTM channels = readTChan $ snd $ getChannelIdSTM Error channels

readErr :: [(ChannelId, TChan Text)] -> IO Text 
readErr = atomically . readErrSTM

readResizeSTM :: [(ChannelId, TChan Text)] -> STM Text 
readResizeSTM channels = readTChan $ snd $ getChannelIdSTM Resize channels

readResize :: [(ChannelId, TChan Text)] -> IO Text 
readResize = atomically . readResizeSTM

readLineSTM :: TChan Text -> STM Text
readLineSTM aChannel = do 
  messages <- (T.split (\c -> c == '\n')) <$> readTChan aChannel 
  case messages of 
    h : t -> do 
        unGetTChan aChannel $ T.unlines t
        return h 
    _ -> return ""

readLine :: TChan Text -> IO Text 
readLine = atomically . readLineSTM