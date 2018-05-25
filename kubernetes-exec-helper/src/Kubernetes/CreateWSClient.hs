{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Kubernetes.CreateWSClient 
  (
    CreateWSClient
    , writer
    , channels
    , container
    , kubernetesConfig
    , commands
    , getTimeOut
    , createWSClient
    , clusterClientParams
    , getHeaders
    , getConnectionOptions
    , prompt
    , getHost
    , getPort
    , getURIAuth
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
import Network.Socket as Socket
import Network.HTTP.Types.URI
import Network.TLS as TLS (ClientParams(..))
import Network.WebSockets as WS (Headers, defaultConnectionOptions, ConnectionOptions)
import Network.URI
import Text.Printf as Printf 
import Kubernetes.K8SChannel as K8SChannel
import Kubernetes.Model
import Kubernetes.KubeConfig
import Kubernetes.Core
import Kubernetes.Client

newtype Route = Route {_route :: String}
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

prompt :: Text 
prompt = "k8:s>"

getHeaders :: CreateWSClient a -> WS.Headers 
getHeaders _ = [] 

writer :: CreateWSClient a -> TChan a 
writer clientState = _writer clientState

channels :: CreateWSClient a -> [(ChannelId, TChan a)]
channels aState = _channels aState

container :: CreateWSClient a -> V1Container 
container = _container

kubernetesConfig :: CreateWSClient a -> KubernetesConfig
kubernetesConfig = _kubernetesConfig

commands :: CreateWSClient a -> [Command]
commands client = _commands client

clusterClientParams :: CreateWSClient a -> TLS.ClientParams 
clusterClientParams client = _clusterClientParams client 

getTimeOut :: CreateWSClient a -> Maybe TimeoutInterval 
getTimeOut client = _timeOutInterval client

{-| 
  A convenience method to return a client state with relevant 
  reader and writer channels.
-}
createWSClient :: TLS.ClientParams -> KubernetesConfig -> V1Container -> [Command] -> IO (CreateWSClient Text)
createWSClient clientParams kubeConfig v1Container commands = do
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
            kubeConfig 
            clientParams            
            v1Container
            commands
            (Just $ (30 * (10 ^6)) :: Maybe Int)

getURIAuth :: CreateWSClient a -> Maybe URIAuth 
getURIAuth (CreateWSClient _ _ kubeConfig _ _ _ _) = do 
    uri <- parseURI $ LByteString.unpackChars $ configHost kubeConfig
    uriAuthI <- uriAuthority uri 
    return uriAuthI

getHost :: CreateWSClient a -> Maybe String 
getHost createWSClient = do 
  uriAuth_@(URIAuth _ regName _) <- getURIAuth createWSClient
  return regName

getPort :: CreateWSClient a -> Maybe Socket.PortNumber
getPort  createWSClient = do
  uriAuth_@(URIAuth _ _ port) <- getURIAuth createWSClient 
  case port of 
    _:p -> return $ (read p :: Socket.PortNumber)
    _ -> return 80 -- default

getConnectionOptions :: CreateWSClient a -> WS.ConnectionOptions 
getConnectionOptions _ = WS.defaultConnectionOptions

