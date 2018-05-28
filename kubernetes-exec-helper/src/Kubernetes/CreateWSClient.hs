{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Kubernetes.CreateWSClient 
  (
    CreateWSClient
    , writer
    , channels
    , commands
    , getTimeOut
    , createWSClient
  )
where 

import Control.Concurrent.STM
import Data.ByteString.Lazy as LazyBString
import Data.ByteString.Lazy.Internal as LByteString 
import Data.ByteString as S
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text 
import Data.Text.Encoding as E (encodeUtf8)
import Kubernetes.Client
import Kubernetes.ClientHelper
import Network.Socket as Socket
import Network.HTTP.Types.URI
import Network.TLS as TLS
import Network.WebSockets as WS (Headers, defaultConnectionOptions, ConnectionOptions)
import Network.URI
import Text.Printf as Printf 
import Kubernetes.K8SChannel as K8SChannel
import Kubernetes.Model
import Kubernetes.KubeConfig
import Kubernetes.Core
import Kubernetes.Client
import Kubernetes.MimeTypes 
import Kubernetes.API.CoreV1
{- | 
  CreateWSClient maintains all threads that are running,  
  a writer channel to send messages to the server 
  and a list of all "(ChannelId, TChan Text)" pairs.
-}
data CreateWSClient a = CreateWSClient {
    _writer :: TChan a -- ^ Write back to the server.
    , _channels :: [(ChannelId, TChan a)] -- ^ Read from the server
    , _commands :: [Command] -- ^ the list of commands to run.
    , _timeOutInterval :: Maybe TimeoutInterval
    }

writer :: CreateWSClient a -> TChan a 
writer clientState = _writer clientState

channels :: CreateWSClient a -> [(ChannelId, TChan a)]
channels aState = _channels aState

commands :: CreateWSClient a -> [Command]
commands client = _commands client


getTimeOut :: CreateWSClient a -> Maybe TimeoutInterval 
getTimeOut client = _timeOutInterval client


{-| 
  A convenience method to return a client state with relevant 
  reader and writer channels.
-}
createWSClient :: [Command] -> IO (CreateWSClient Text)
createWSClient commands = do
    cW <- atomically newTChan :: IO (TChan Text) -- Writer
    c0 <- atomically newTChan :: IO (TChan Text)
    c1 <- atomically newTChan :: IO (TChan Text)
    c2 <- atomically newTChan :: IO (TChan Text)
    c3 <- atomically newTChan :: IO (TChan Text)
    c4 <- atomically newTChan :: IO (TChan Text)
    return 
      $ CreateWSClient             
            cW 
            (Prelude.zip K8SChannel.allChannels [c0, c1, c2, c3, c4])
            commands
            (Just $ (30 * (10 ^6)) :: Maybe Int)


