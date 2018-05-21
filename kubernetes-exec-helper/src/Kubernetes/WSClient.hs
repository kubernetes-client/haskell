{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
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
      runClient -- * Client.
      -- * Reads      
      , readLine
      , readChannelIdSTM
      -- * Accessors
      , getTChanSTM
    )
  where 

import Control.Concurrent(ThreadId, threadDelay)
import Control.Concurrent.Async(waitAny, async, Async, wait)
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Monad (forever)
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 as CB8
import Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)
import Kubernetes.K8SChannel
import Kubernetes.CreateWSClient
import Kubernetes.ClientHelper
import Kubernetes.KubeConfig as KubeConfig
import Kubernetes.Core
import Network.Connection
import Network.Socket as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Printf as Printf
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WS
import Kubernetes.Model
import Kubernetes.KubeConfig
import Kubernetes.API.CoreV1
import Kubernetes.Client
import Kubernetes.MimeTypes
import System.Timeout (timeout) 
import System.IO (hSetBuffering, BufferMode(..), stdin)
import Wuss as WSS (runSecureClient)


runClient :: CreateWSClient Text -> Name -> Namespace -> IO () 
runClient createWSClient name namespace = do 
  let 
    headers = getHeaders createWSClient
    connectionOptions = getConnectionOptions createWSClient
    uri  = getURIAuth createWSClient
    host = getHost createWSClient
    port = getPort createWSClient
    path = CB8.unpack $
              Prelude.foldr (\p acc -> p <> acc) "" $ rUrlPath $ 
                fullRequest createWSClient name namespace
    params = []
    timeoutInt = getTimeOut createWSClient
    kubeConfig = kubernetesConfig createWSClient
    clusterClientParams_ = TLSSettings $ clusterClientParams createWSClient
  case(host, port) of 
    (Just h, Just p) -> do 
      execAttach_ <- async (attachExec createWSClient name namespace) -- TODO: Where should this go
      Prelude.putStrLn "calling attach."
      runClientWithTLS h p path clusterClientParams_ (\conn -> k8sClient timeoutInt createWSClient conn)     
      wait execAttach_ 
      return ()

    _ -> return ()


runClientWithTLS h p path tlsSettings application = do
  let options = WS.defaultConnectionOptions
  let headers = []
  let connectionParams = ConnectionParams {
      connectionHostname = h 
    , connectionPort = p 
    , connectionUseSecure = Just tlsSettings
    , connectionUseSocks = Nothing
  }
  context <- initConnectionContext 
  connection <- connectTo context connectionParams 
  stream <- WS.makeStream 
    (fmap Just $ connectionGetChunk connection)
    (maybe (return()) (connectionPut connection . BL.toStrict))
  Prelude.putStrLn $ "Path " <> path
  WS.runClientWithStream stream h path options headers application
    `catch` (\exc@(SomeException e) -> Prelude.putStrLn $ show exc)

{- | 
  Read commands from std in and send it to the pod.
-}
readCommands :: TChan Text -> IO ()
readCommands writerChannel = do 
  hSetBuffering stdin NoBuffering
  T.putStr prompt
  line <- T.pack <$> Prelude.getLine
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
  snd $ Prelude.head $ Prelude.filter(\(x, _) -> x == aChannelId) channels

readChannelIdSTM :: ChannelId -> [(ChannelId, TChan Text)] -> STM Text 
readChannelIdSTM channel channels = 
    readTChan $ getChannelIdSTM channel channels

readLineSTM :: TChan Text -> STM Text
readLineSTM aChannel = readTChan aChannel 

readLine :: TChan Text -> IO Text 
readLine = atomically . readLineSTM


fullRequest client name namespace = 
  applyContainer containerName_ $ applyCommands (commands client ) $ makeRequest name namespace
  where 
    container_ = container client 
    containerName_ = v1ContainerName container_

applyCommands commands request = 
    Prelude.foldr (\ele acc -> applyOptionalParam request ele) request commands

applyContainer containerName request = 
    applyOptionalParam request (Container containerName) 

makeRequest name namespace = 
  (Kubernetes.API.CoreV1.connectGetNamespacedPodExec 
    (Accept MimeJSON) 
    name
    namespace
    )

attachExec client name namespace = do
  let 
    container_ = container client
    containerName_ = v1ContainerName container_
    kubeConfig = kubernetesConfig client
    tlsParams = clusterClientParams client
    fullRequest_ = fullRequest client name namespace 
  Prelude.putStrLn $ show fullRequest_
  manager <- newManager tlsParams 
  dispatchMime 
    manager 
    kubeConfig
    fullRequest_