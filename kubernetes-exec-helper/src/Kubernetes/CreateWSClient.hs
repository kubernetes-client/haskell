{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Kubernetes.CreateWSClient 
  (
    CreateWSClient
    , writer
    , channels
    , configuration
    , commands
    , getTimeOut
    , createWSClient
    , getHost
    , getPort 
    , getHeaders
    , getConnectionOptions
    , prompt -- A command prompt.
  )
where 
import Control.Concurrent.STM 
import Data.Text 
import Data.Text.Encoding as E (encodeUtf8)
import Network.Socket as Socket
import Network.HTTP.Types.URI
import Network.WebSockets as WS (Headers, defaultConnectionOptions, ConnectionOptions)
import Text.Printf as Printf 
import Kubernetes.K8SChannel as K8SChannel
import Kubernetes.Model
import Kubernetes.KubeConfig

newtype Route = Route {_route :: String}
{- | 
  CreateWSClient maintains all threads that are running,  
  a writer channel to send messages to the server 
  and a list of all "(ChannelId, TChan Text)" pairs.
-}
data CreateWSClient a = CreateWSClient {
    _writer :: TChan a -- ^ Write back to the server.
    , _channels :: [(ChannelId, TChan a)] -- ^ Read from the server
    , _configuration :: V1Container -- ^ The container
    , _commands :: [Command] -- ^ the list of commands to run.
    , _timeOutInterval :: Maybe TimeoutInterval
    }

prompt :: Text 
prompt = "k8:s>"

getHeaders :: CreateWSClient a -> WS.Headers 
getHeaders _ = [] 

writer :: CreateWSClient a -> TChan a 
writer clientState = _writer clientState

channels :: CreateWSClient a -> [(ChannelId, TChan a)]
channels aState = _channels aState

configuration :: CreateWSClient a -> V1Container 
configuration = _configuration

commands :: CreateWSClient a -> [Command]
commands client = _commands client

getTimeOut :: CreateWSClient a -> Maybe TimeoutInterval 
getTimeOut client = _timeOutInterval client

{-- The query string for the url.
-}
getQueryString :: CreateWSClient a -> Query
getQueryString client = 
    Prelude.map(\a@(Command _uc) -> ("command", Just $ encodeUtf8 _uc)) $ commands client

getPath :: CreateWSClient a -> String 
getPath client = "/"

{-| 
  A convenience method to return a client state with relevant 
  reader and writer channels.
-}
createWSClient :: V1Container -> [Command] -> IO (CreateWSClient Text)
createWSClient v1Container commands = do
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
            v1Container
            commands
            (Just $ (30 * (10 ^6)) :: Maybe Int)

getHost :: CreateWSClient a -> String 
getHost = undefined

getPort :: CreateWSClient a -> Socket.PortNumber
getPort  = undefined

getConnectionOptions :: CreateWSClient a -> WS.ConnectionOptions 
getConnectionOptions _ = WS.defaultConnectionOptions
