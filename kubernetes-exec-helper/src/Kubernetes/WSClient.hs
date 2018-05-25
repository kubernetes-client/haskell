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
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 as CB8
import qualified Data.ByteString.Lazy.Char8 as BCL
import Data.Proxy as P (Proxy(..))
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text as Text 
import Data.Text.IO as T 
import Data.Text.Encoding as TE 
import Kubernetes.API.CoreV1
import Kubernetes.Client
import Kubernetes.ClientHelper
import Kubernetes.Core
import Kubernetes.CreateWSClient
import Kubernetes.K8SChannel
import Kubernetes.KubeConfig
import Kubernetes.KubeConfig as KubeConfig
import Kubernetes.MimeTypes
import Kubernetes.Model
import Network.Connection
import Network.HTTP.Types (renderQuery)
import Network.Socket as S
import qualified Data.Text
import qualified Data.Text.IO as T
import qualified Network.HTTP.Client as NH
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WS
import qualified Text.Printf as Printf
import System.IO (hSetBuffering, BufferMode(..), stdin)
import System.Timeout (timeout) 
import Wuss as WSS (runSecureClient)


runClient :: CreateWSClient Text -> Name -> Namespace -> AuthApiKeyBearerToken -> IO () 
runClient createWSClient name namespace bearerToken = do 
  let 
    headers = getHeaders createWSClient
    connectionOptions = getConnectionOptions createWSClient
    uri  = getURIAuth createWSClient
    host = getHost createWSClient
    port = getPort createWSClient
    path = CB8.unpack $
              Prelude.foldr (\p acc -> p <> acc) "" $ rUrlPath $ 
                fullRequest createWSClient name namespace
    params = CB8.unpack $ 
                CB8.fromStrict $ renderQuery True 
                  $ paramsQuery $ rParams $ fullRequest createWSClient name namespace
    kubeConfig = kubernetesConfig createWSClient                  
    timeoutInt = getTimeOut createWSClient
    clusterClientParams_ = TLSSettings $ clusterClientParams createWSClient
  case(host, port) of
    (Just h, Just p) -> do
      r@(InitRequest urlRequest) <- 
          _toInitRequest kubeConfig $ (_mkRequest "GET" $ [BCL.pack $ path <> params])
            `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyBearerToken)  :: IO (InitRequest Text MimeAny Text MimeAny)
      execAttach_ <- async (attachExec createWSClient name namespace) -- TODO: Where should this go
      runClientWithTLS h p (path <> params) bearerToken clusterClientParams_ 
        (\conn -> k8sClient timeoutInt createWSClient conn)     
      wait execAttach_ 
      return ()
    _ -> return ()


runClientWithTLS :: String -> PortNumber -> String -> AuthApiKeyBearerToken -> TLSSettings -> WS.ClientApp () -> IO ()
runClientWithTLS h p urlRequest (AuthApiKeyBearerToken bearerToken) tlsSettings application = do
  let options = WS.defaultConnectionOptions
  let bearerTokenString = "Bearer " <> bearerToken
  let headers = Prelude.map (\(x, y) -> (x, TE.encodeUtf8 y)) $ 
        [("sec-websocket-protocol", "v4.channel.k8s.io")
        , ("Connection", "upgrade"), ("Upgrade", "websocket")
        , ("Authorization", bearerTokenString)]
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
  WS.runClientWithStream stream h urlRequest options headers application
    `catch` (\exc@(SomeException e) -> 
                    Prelude.putStrLn $ "Exception " <> show exc)

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
            publishMessage channels $ Text.splitAt 1 msg
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

fullRequest :: CreateWSClient a
  -> Name
  -> Namespace 
  -> KubernetesRequest ConnectGetNamespacedPodExec MimeNoContent Text MimeJSON
fullRequest client name namespace = 
  applyCommands (commands client ) $ makeRequest name namespace
  where 
    container_ = container client 
    containerName_ = v1ContainerName container_


applyCommands :: (Kubernetes.Core.HasOptionalParam req param, Foldable t) =>
  t param
  -> KubernetesRequest req contentType res accept
  -> KubernetesRequest req contentType res accept
applyCommands commands request = 
    Prelude.foldr (\ele acc -> applyOptionalParam request ele) request commands

applyContainer :: Kubernetes.Core.HasOptionalParam req Container =>
  Text
  -> KubernetesRequest req contentType res accept
  -> KubernetesRequest req contentType res accept
applyContainer containerName request = 
    applyOptionalParam request (Container containerName) 

makeRequest :: Name ->  Namespace -> 
  KubernetesRequest ConnectGetNamespacedPodExec MimeNoContent Text MimeJSON
makeRequest name namespace = 
  (Kubernetes.API.CoreV1.connectGetNamespacedPodExec 
    (Accept MimeJSON) 
    name
    namespace
    )

-- | Dispatch 'ConnectGetNamespacedPodExec' request.
attachExec :: CreateWSClient a -> Name -> Namespace -> IO (MimeResult Text)
attachExec client name namespace = do
  let 
    container_ = container client
    containerName_ = v1ContainerName container_
    kubeConfig = kubernetesConfig client
    tlsParams = clusterClientParams client
    fullRequest_ = fullRequest client name namespace 
  manager <- newManager tlsParams 
  dispatchMime 
    manager 
    kubeConfig
    fullRequest_