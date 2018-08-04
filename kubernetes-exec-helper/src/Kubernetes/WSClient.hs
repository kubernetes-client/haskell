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

import Control.Concurrent.Async(waitAny, async, Async)
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Monad (forever)
import Data.Char(chr)
import Data.Monoid ((<>))
import Data.Text as T
import Data.Text.Encoding as TE
import Data.Function ((&))
import Kubernetes.API.CoreV1
import Kubernetes.Client
import Kubernetes.Core
import Kubernetes.CreateWSClient
import Kubernetes.K8SChannel
import Kubernetes.MimeTypes
import Kubernetes.Model
import Network.Connection
import Network.Socket as S
import Network.TLS as TLS          (ClientParams(..))
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Types.URI as NHURI
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WS
import qualified Text.Printf as Printf
import System.Log.Logger
import System.Timeout (timeout) 

type InitRequestS = InitRequest ConnectGetNamespacedPodExec MimeNoContent Text MimeNoContent
runClient :: CreateWSClient Text -> KubernetesConfig -> TLS.ClientParams -> Name -> Namespace -> IO (Async())
runClient createWSClient_ kubeConfig clientParams name_ namespace_ = do 
  let 
    timeoutInt = getTimeOut createWSClient_
    commands_ = 
        Prelude.map 
          (\(Command c) -> ("command", Just c)) $ commands createWSClient_
    initR :: KubernetesRequest ConnectGetNamespacedPodExec MimeNoContent Text MimeNoContent 
      = connectGetNamespacedPodExec (Accept MimeNoContent) name_ namespace_
    c = initR -&- (Stdin True) -&- (Stdout True) -&- (Stderr True)
    rParamsP = rParams c
    commandQuery = NHURI.queryTextToQuery commands_
    rParamsP1 = rParamsP {paramsQuery = (commandQuery <> (paramsQuery rParamsP))}
  InitRequest req :: InitRequestS <- _toInitRequest kubeConfig (c {rParams = rParamsP1})
  client_ <-   
      async $ 
        runClientWithTLS (host req) (port req) (endpoint req) 
          (NH.requestHeaders req) clientParams 
            (\conn -> k8sClient timeoutInt createWSClient_ conn)
  return client_
  where 
    endpoint req = 
      T.unpack $ 
        T.pack $
          BC.unpack $
             NH.path req <> NH.queryString req
    host req = T.unpack $ T.pack $ BC.unpack $ NH.host req 
    port req = (read $ (Printf.printf "%d" (NH.port req)) :: PortNumber)


runClientWithTLS :: String -> PortNumber -> String -> WS.Headers -> TLS.ClientParams -> WS.ClientApp () -> IO ()
runClientWithTLS host portNum urlRequest headers tlsSettings application = do
  let options = WS.defaultConnectionOptions
  let connectionParams = ConnectionParams {
      connectionHostname = host 
    , connectionPort = portNum
    , connectionUseSecure = Just . TLSSettings $ tlsSettings
    , connectionUseSocks = Nothing
  }
  let headers_ = ("Sec-WebSocket-Protocol", "v4.channel.k8s.io") : headers 
  context <- initConnectionContext
  handle (\exc@(SomeException _) -> 
                    errorM "WSClient" $ show (" Exception "  :: String)
                        <> show exc <> (show host) <> ":" <> (show portNum)) $ do 
    connection <- connectTo context connectionParams 
    stream <- WS.makeStream 
      (readChunk connection)
      (maybe (return()) (\byteString -> do 
        debugM "WSClient" $ (">>>" :: String) <> show (BL.toStrict byteString)
        connectionPut connection $ BL.toStrict byteString))
    WS.runClientWithStream stream host urlRequest options headers_ application
    where
      readChunk conn = do 
        byteString <- connectionGetChunk conn
        debugM "WSClient" $ ("<<<" :: String) <> (Prelude.take 120 $ show byteString)
        return $ Just byteString

waitForThreads :: Maybe (Async a) -> (Async a) -> IO (Async a, a)
waitForThreads (Just r) (sender) = waitAny [r, sender]
waitForThreads Nothing sender = waitAny [sender] 

-- | Socket IO handler.
k8sClient :: Maybe TimeoutInterval -> CreateWSClient Text -> WS.Connection -> IO ()
k8sClient interval clientState conn = do
    rcv <- timedThread worker (channels clientState) conn interval 
    sender <- async $ forever $ do 
        nextMessage <- atomically . readTChan $ writer clientState
        WS.sendTextData conn nextMessage
    -- TODO: Fix this code.
    _ <- waitForThreads rcv sender
    return ()
    where
      worker channels_ conn_= async $ forever $ do 
            msg <- WS.receiveData conn_
            publishMessage channels_ $ T.splitAt 1 msg
      timedThread aWorker channels_ connection timeoutInterval =
        case timeoutInterval of  
            Nothing ->  Just <$> aWorker channels_ connection 
            Just m -> timeout m $ aWorker channels_ connection

-- | Publish messages from the reader into the channel.
publishMessage :: [(ChannelId, TChan Text)] -> (Text, Text) -> IO ()
publishMessage channels_ c@(channel, message) = do 
  debugM "WSClient" $ "channel : " <> (show $ asByte channel)
  let chanId = readChannel (asByte channel)
  debugM "WSClient" $ show chanId
  case chanId of
    Nothing -> throwIO $ InvalidChannel $ pack $ show chanId
    Just aChan -> do
      atomically $
        writeTChan (getChannelIdSTM aChan channels_) message
  where 
    asByte :: Text -> Char 
    asByte aText = BC.head $ TE.encodeUtf8 aText
{- | 
  Query a channelId from a list of Channels.
-}
getTChanSTM :: ChannelId -> [(ChannelId, TChan Text)] -> TChan Text 
getTChanSTM a b = getChannelIdSTM a b 

getChannelIdSTM :: ChannelId -> [(ChannelId, TChan Text)] -> TChan Text
getChannelIdSTM aChannelId channels_ = 
  snd $ Prelude.head $ Prelude.filter(\(x, _) -> x == aChannelId) channels_

readChannelIdSTM :: ChannelId -> [(ChannelId, TChan Text)] -> STM Text 
readChannelIdSTM channel = 
    \channels_ -> readTChan $ getChannelIdSTM channel channels_

readLineSTM :: TChan Text -> STM Text
readLineSTM aChannel = readTChan aChannel 

readLine :: TChan Text -> IO Text 
readLine = atomically . readLineSTM
