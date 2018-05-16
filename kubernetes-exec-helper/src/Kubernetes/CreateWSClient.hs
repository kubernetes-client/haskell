{-# LANGUAGE OverloadedStrings #-}
module Kubernetes.CreateWSClient 
  (
    CreateWSClient
    , writer
    , channels
    , clientSession
    , configuration
    , getTimeOut
    , createWSClient
    , getRoute
    , prompt -- A command prompt.
  )
where 
import Control.Concurrent.STM 
import Data.Text 
import Network.Socket
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
    , _clientSession :: (Socket, AddrInfo) -- ^ Handle to the 'Socket' and the 'AddrInfo'
    , _configuration :: V1Container -- ^ The container
    , _commands :: [Command] -- ^ the list of commands to run.
    }

prompt :: Text 
prompt = "k8:s>"

writer :: CreateWSClient a -> TChan a 
writer clientState = _writer clientState

channels :: CreateWSClient a -> [(ChannelId, TChan a)]
channels aState = _channels aState

clientSession :: CreateWSClient a -> (Socket, AddrInfo)
clientSession = _clientSession

configuration :: CreateWSClient a -> V1Container 
configuration = _configuration

commands :: CreateWSClient a -> [Command]
commands client = _commands client

getTimeOut :: CreateWSClient a -> Maybe TimeoutInterval 
getTimeOut = undefined 

{-| 

-}
getRoute :: CreateWSClient a -> String
getRoute = undefined

makeSocketPair :: String -> Int -> IO (Socket, AddrInfo)
makeSocketPair hostName port = do 
  let hints = defaultHints
                  {addrSocketType = Stream}

  serverAddr:_ <- getAddrInfo (Just hints) (Just hostName) (Just $ Printf.printf "%d" port)
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  setSocketOption sock NoDelay 1
  return (sock, serverAddr)

{-| 
  A convenience method to return a client state with relevant 
  reader and writer channels.
-}
createWSClient :: V1Container -> IO (CreateWSClient Text)
createWSClient v1Container = do
    cW <- atomically newTChan :: IO (TChan Text) -- Writer channel
    c0 <- atomically newTChan :: IO (TChan Text)
    c1 <- atomically newTChan :: IO (TChan Text)
    c2 <- atomically newTChan :: IO (TChan Text)
    c3 <- atomically newTChan :: IO (TChan Text)
    c4 <- atomically newTChan :: IO (TChan Text)
    socket <- makeSocketPair (getHost v1Container) (getPort v1Container)
    return 
      $ CreateWSClient 
            cW 
            (Prelude.zip K8SChannel.allChannels [c0, c1, c2, c3, c4])
            socket
            v1Container
            []


getHost :: V1Container -> String 
getHost = undefined

getPort :: V1Container -> Int
getPort  = undefined