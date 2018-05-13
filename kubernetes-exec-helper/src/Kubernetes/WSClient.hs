{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
  Module : Kubernetes.WSClient
  Description : This module implements a web socket client attaching to kubectl exec command. 

  This implementation is based on the 
  python reference implementation 
  <https://github.com/kubernetes-client/python-base/tree/a41c44715241552de73361673152f3f0d0bb9bc4/stream 
  here>.
  
-}
module Kubernetes.WSClient
    (
      runClient
      -- * Reads      
      , readErr
      , readErrSTM
      , readLine
      , readLineSTM
      , readResize
      , readResizeSTM
      , readStdErr
      , readStdErrSTM
      , readStdIn
      , readStdInSTM
      , readStdOut 
      , readStdOutSTM
      , readChannelIdSTM
      -- *Writes 
      , writeErr
      , writeErrSTM
      , writeResize
      , writeResizeSTM
      , writeStdErr
      , writeStdErrSTM
      , writeStdIn 
      , writeStdInSTM
      , writeStdOut
      , writeStdOutSTM
      , writeChannelIdSTM -- ^ Write to a "ChannelId" prefixing the channel code.
      -- * Accesors
      , getTChanSTM
    )
  where 

import Control.Concurrent(ThreadId)
import Control.Concurrent.Async(waitAny, async, Async, wait)
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Monad (forever)
import Control.Monad.Reader
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)
import Kubernetes.K8SChannel
import Kubernetes.KubeConfig
import Network.Socket(withSocketsDo)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import System.Timeout (timeout) 
import System.IO (hSetBuffering, BufferMode(..), stdin)

-- | Run the web socket client. 
runClient :: String -- ^ Host  
            -> Int  -- ^ Port 
            -> String -- ^ Path
            -> Maybe TimeoutInterval -- ^ Channel timeout.
            -> CreateWSClient Text 
            -> IO ()
runClient domain port route timeout createWSClient = do 
  withSocketsDo $ WS.runClient domain port route $ 
    (\c -> bracket 
          (return c)
          (\conn -> WS.sendClose conn ("Closing session" :: T.Text))
          (\conn -> k8sClient conn timeout createWSClient))

-- | Socket IO handler.
k8sClient :: WS.Connection -> Maybe TimeoutInterval -> CreateWSClient Text -> IO ()
k8sClient conn interval clientState@(CreateWSClient (cW ,channels)) = do
    rcv <- worker channels conn
    sender <- async $ forever $ do 
        nextMessage <- atomically . readTChan $ cW 
        WS.sendTextData conn nextMessage
    _ <- waitAny [rcv, sender]
    return ()
    where
      worker channels conn= async $ forever $ do 
            msg <- WS.receiveData conn
            publishMessage channels $ T.splitAt 1 msg
      timedThread aWorker channels connection timeoutInterval =
        case timeoutInterval of  
            Nothing ->  Just <$> aWorker channels connection 
            Just m -> timeout m $ aWorker channels connection

-- | Publish messages from the reader into the channel.
publishMessage :: [(ChannelId, TChan Text)] -> (Text, Text) -> IO ()
publishMessage channels c@(channel, message) = do 
  let chanId = readChannel channel
  case chanId of
    Nothing -> throwIO $ InvalidChannel channel
    Just aChan -> do
      atomically $
        writeTChan (getChannelIdSTM aChan channels) message

{- | 
  Query a channelId from a list of Channels.
-}
getTChanSTM :: ChannelId -> [(ChannelId, TChan Text)] -> TChan Text 
getTChanSTM a b = getChannelIdSTM a b 

getChannelIdSTM :: ChannelId -> [(ChannelId, TChan Text)] -> TChan Text
getChannelIdSTM aChannelId channels = 
  snd $ head $ Prelude.filter(\(x, _) -> x == aChannelId) channels

readChannelIdSTM :: ChannelId -> [(ChannelId, TChan Text)] -> STM Text 
readChannelIdSTM channel channels = 
    readTChan $ getChannelIdSTM channel channels

readStdInSTM :: [(ChannelId, TChan Text)] -> STM Text
readStdInSTM channels = readTChan $ getChannelIdSTM StdIn channels

readStdIn :: [(ChannelId, TChan Text)] -> IO Text 
readStdIn = atomically . readStdInSTM

readStdOutSTM :: [(ChannelId, TChan Text)] -> STM Text
readStdOutSTM channels = readTChan $ getChannelIdSTM StdOut channels

readStdOut :: [(ChannelId, TChan Text)] -> IO Text 
readStdOut = atomically . readStdOutSTM

readStdErrSTM :: [(ChannelId, TChan Text)] -> STM Text
readStdErrSTM channels = readTChan $ getChannelIdSTM StdErr channels

readStdErr :: [(ChannelId, TChan Text)] -> IO Text 
readStdErr = atomically . readStdErrSTM

readErrSTM :: [(ChannelId, TChan Text)] -> STM Text 
readErrSTM channels = readTChan $ getChannelIdSTM Error channels

readErr :: [(ChannelId, TChan Text)] -> IO Text 
readErr = atomically . readErrSTM

readResizeSTM :: [(ChannelId, TChan Text)] -> STM Text 
readResizeSTM channels = readTChan $ getChannelIdSTM Resize channels

readResize :: [(ChannelId, TChan Text)] -> IO Text 
readResize = atomically . readResizeSTM

readLineSTM :: TChan Text -> STM Text
readLineSTM aChannel = do 
  messages <- readTChan aChannel 
  return messages

readLine :: TChan Text -> IO Text 
readLine = atomically . readLineSTM

writeErrSTM :: Text -> TChan Text -> STM ()
writeErrSTM = writeChannelIdSTM Error

writeResizeSTM :: Text -> TChan Text -> STM () 
writeResizeSTM = writeChannelIdSTM Resize

writeStdErrSTM :: Text -> TChan Text -> STM () 
writeStdErrSTM = writeChannelIdSTM StdErr

writeStdOutSTM :: Text -> TChan Text -> STM () 
writeStdOutSTM = writeChannelIdSTM StdOut

writeStdInSTM :: Text -> TChan Text -> STM () 
writeStdInSTM = writeChannelIdSTM StdIn

writeErr :: Text -> TChan Text -> IO ()
writeErr = \text chan -> atomically (writeErrSTM text chan)

writeResize :: Text -> TChan Text -> IO ()
writeResize = \text chan -> atomically (writeResizeSTM text chan)

writeStdIn :: Text -> TChan Text -> IO () 
writeStdIn = \text chan -> atomically (writeStdInSTM text chan) 

writeStdOut :: Text -> TChan Text -> IO () 
writeStdOut = \text chan -> atomically (writeStdOutSTM text chan) 

writeStdErr :: Text -> TChan Text -> IO ()
writeStdErr = \text chan -> atomically (writeStdErrSTM text chan)


-- Write 'content' to 'ChannelId'.
writeChannelIdSTM :: ChannelId -> Text -> TChan Text -> STM () 
writeChannelIdSTM channelId content chan = 
  writeTChan chan $ T.pack (show channelId) <> content
