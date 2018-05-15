{-# LANGUAGE OverloadedStrings #-}
module Kubernetes.CreateWSClient 
  (
    CreateWSClient
    , writer
    , channels
    , clientSession
    , createWSClient
    , createWSClientFromContainer
  )
where 
import Control.Concurrent.STM 
import Data.Text 
import Network.Socket
import Text.Printf as Printf 
import Kubernetes.K8SChannel as K8SChannel
import Kubernetes.Model
{- | 
  CreateWSClient maintains all threads that are running,  
  a writer channel to send messages to the server 
  and a list of all "(ChannelId, TChan Text)" pairs.
-}
data CreateWSClient a = CreateWSClient {
    _writer :: TChan a -- ^ Write back to the server.
    , _channels :: [(ChannelId, TChan a)] -- ^ Read from the server
    , _clientSession :: (Socket, AddrInfo) -- ^ Handle to the 'Socket' and the 'AddrInfo'
    }

writer :: CreateWSClient a -> TChan a 
writer clientState = _writer clientState

channels :: CreateWSClient a -> [(ChannelId, TChan a)]
channels = _channels 

clientSession :: CreateWSClient a -> (Socket, AddrInfo)
clientSession = _clientSession

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
createWSClient :: String -> Int -> IO (CreateWSClient Text)
createWSClient host port = do
    cW <- atomically newTChan :: IO (TChan Text) -- Writer channel
    c0 <- atomically newTChan :: IO (TChan Text)
    c1 <- atomically newTChan :: IO (TChan Text)
    c2 <- atomically newTChan :: IO (TChan Text)
    c3 <- atomically newTChan :: IO (TChan Text)
    c4 <- atomically newTChan :: IO (TChan Text)
    socket <- makeSocketPair host port
    return 
      $ CreateWSClient 
            cW 
            (Prelude.zip K8SChannel.allChannels [c0, c1, c2, c3, c4])
            socket

{- | 
  Create a 'CreateWSClient' from a 'V1Container' given a 'containerName'. 
-}          

createWSClientFromContainer :: Text -> IO (CreateWSClient Text)
createWSClientFromContainer containerName =  
    createWSClient host port 
  where 
    container = mkV1Container containerName
    port = getPort container :: Int 
    host = getHost container :: String

getPort :: V1Container -> Int
getPort = undefined 

getHost :: V1Container -> String
getHost = undefined
