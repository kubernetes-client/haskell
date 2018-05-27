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
import Data.Maybe
import Data.Monoid ((<>))
import Data.Proxy as P (Proxy(..))
import Data.Text as T
import Data.Text.Encoding as TE 
import Data.Text.IO as T 
import Kubernetes.API.CoreV1
import Kubernetes.Client
import Kubernetes.ClientHelper
import Kubernetes.Core
import Kubernetes.CreateWSClient
import Kubernetes.K8SChannel
import Kubernetes.KubeConfig
import Kubernetes.KubeConfig as KubeConfig
import Kubernetes.MimeTypes
import Kubernetes.Misc 
import Kubernetes.Model
import Network.Connection
import Network.HTTP.Types (renderQuery)
import Network.Socket as S
import Network.TLS as TLS          (credentialLoadX509, ClientParams(..))
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.Text
import qualified Data.Text.IO as T
import qualified Network.HTTP.Client as NH
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WS
import qualified Text.Printf as Printf
import System.IO (hSetBuffering, BufferMode(..), stdin)
import System.Timeout (timeout) 
import Wuss as WSS

runClient :: CreateWSClient Text -> KubernetesConfig -> TLS.ClientParams -> Name -> Namespace -> IO () 
runClient createWSClient kubeConfig clientParams name namespace = do 
  let 
    timeoutInt = getTimeOut createWSClient
    commands_ = commands createWSClient
    r = 
        Prelude.foldr (\c reqAcc -> applyOptionalParam reqAcc c) 
              (connectGetNamespacedPodExec (Accept MimePlainText) name namespace)
              commands_ 
  (InitRequest req) <- _toInitRequest kubeConfig r
  T.putStrLn $ T.pack (endpoint req) 
  execAttach_ <- 
    async (attachExec createWSClient kubeConfig clientParams name namespace r) -- TODO: Where should this go
  runClientWithTLS (host req) (port req) (endpoint req) kubeConfig clientParams (\conn -> k8sClient timeoutInt createWSClient conn)
  wait execAttach_ >> return ()
  where 
    endpoint req = 
      T.unpack $ 
        T.pack $
          BC.unpack $
            NH.method req <> " " <> NH.host req <> NH.path req <> NH.queryString req
    host req = T.unpack $ T.pack $ BC.unpack $ NH.host req 
    port req = PortNum 8443 -- $ ((fromIntegral (NH.port req :: Int)))

runClientWithTLS :: String -> PortNumber -> String -> KubernetesConfig -> TLS.ClientParams -> WS.ClientApp () -> IO ()
runClientWithTLS host portNumber urlRequest kubeConfig tlsSettings application = do
  let options = WS.defaultConnectionOptions
  let headers = []
  let connectionParams = ConnectionParams {
      connectionHostname = host 
    , connectionPort = portNumber 
    , connectionUseSecure = Just $ TLSSettings tlsSettings
    , connectionUseSocks = Nothing
  }
  Prelude.putStrLn $ show host <> " : " <> (show portNumber)
  context <- initConnectionContext
  handle (\exc@(SomeException e) -> 
                    Prelude.putStrLn $ "Exception " <> show exc <> (show host) <> ":" <> (show portNumber)) $ do 
    connection <- connectTo context connectionParams 
    stream <- WS.makeStream 
      (fmap Just $ connectionGetChunk connection)
      (maybe (return()) (connectionPut connection . BL.toStrict))
    WS.runClientWithStream stream host urlRequest options headers application

-- | Socket IO handler.
k8sClient :: Maybe TimeoutInterval -> CreateWSClient Text -> WS.Connection -> IO ()
k8sClient interval clientState conn = do
    Prelude.putStrLn "worker client..."
    rcv <- worker (channels clientState) conn
    sender <- async $ forever $ do 
        nextMessage <- atomically . readTChan $ writer clientState
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
  snd $ Prelude.head $ Prelude.filter(\(x, _) -> x == aChannelId) channels

readChannelIdSTM :: ChannelId -> [(ChannelId, TChan Text)] -> STM Text 
readChannelIdSTM channel channels = 
    readTChan $ getChannelIdSTM channel channels

readLineSTM :: TChan Text -> STM Text
readLineSTM aChannel = readTChan aChannel 

readLine :: TChan Text -> IO Text 
readLine = atomically . readLineSTM

