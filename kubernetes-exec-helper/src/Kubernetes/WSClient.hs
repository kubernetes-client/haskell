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
      , readLine
      , readChannelIdSTM
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
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)
import Kubernetes.K8SChannel
import Kubernetes.CreateWSClient
import Kubernetes.KubeConfig
import Network.Socket as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Printf as Printf
import Network.Socket as S
import qualified Network.WebSockets as WS
import Kubernetes.Model
import Kubernetes.KubeConfig
import System.Timeout (timeout) 
import System.IO (hSetBuffering, BufferMode(..), stdin)

getHeaders :: V1Container -> WS.Headers 
getHeaders = undefined 

getFullHost :: V1Container -> String 
getFullHost = undefined 

getConnectionOptions :: V1Container -> WS.ConnectionOptions 
getConnectionOptions _ = WS.defaultConnectionOptions

getPath :: V1Container -> String 
getPath = undefined 

getDomain :: V1Container -> String 
getDomain = undefined 


runClient :: CreateWSClient Text -> IO () 
runClient createWSClient = do 
  let 
    container = configuration createWSClient
    headers = getHeaders container
    connectionOptions = getConnectionOptions container
    fullHost = getFullHost container
    route = getRoute createWSClient 
    path = getPath container
    domain = getDomain container
    timeout = getTimeOut createWSClient 
  (socket, addr) <- return $ clientSession createWSClient
  res <- finally 
          (do 
              _ <- S.connect socket (S.addrAddress addr) 
                      `catch` (\a@(SomeException e) -> T.putStrLn (T.pack $ show a))
              withSocketsDo $ 
                WS.runClientWithSocket 
                  socket fullHost route connectionOptions headers 
                    $ (\conn -> k8sClient timeout createWSClient conn))
          (
            T.putStrLn "Closing socket. Bye"
             >> S.close socket)
  return ()

{- | 
  Read commands from std in and send it to the pod.
-}
readCommands :: TChan Text -> IO ()
readCommands writerChannel = do 
  hSetBuffering stdin NoBuffering
  line <- T.pack <$> getLine
  atomically $ writeTChan writerChannel $ line
  readCommands writerChannel

-- | Socket IO handler.
k8sClient :: Maybe TimeoutInterval -> CreateWSClient Text -> WS.Connection -> IO ()
k8sClient interval clientState conn = do
    rcv <- worker (channels clientState) conn
    sender <- async $ forever $ do 
        nextMessage <- atomically . readTChan $ writer clientState
        WS.sendTextData conn nextMessage
    commandReader <- async $ readCommands $ writer clientState
    _ <- waitAny [rcv, sender, commandReader]
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

readLineSTM :: TChan Text -> STM Text
readLineSTM aChannel = readTChan aChannel 

readLine :: TChan Text -> IO Text 
readLine = atomically . readLineSTM
