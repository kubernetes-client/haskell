{-# LANGUAGE OverloadedStrings #-}
{-| 
  Module : Kubernetes.K8SChannel
  Description : This module maintains types to interact with STM channels and IO.
-}
module Kubernetes.K8SChannel where 

import Control.Exception
import Control.Exception.Safe
import Control.Concurrent(ThreadId)
import Control.Concurrent.Async(Async)
import Control.Concurrent.STM
import Data.Text
import Network.Socket
import qualified Network.WebSockets as WS
import Kubernetes.KubeConfig
import System.IO (stdin, stdout, stderr, Handle)

-- | An interval in seconds for the timeout.
type TimeoutInterval = Int

-- | ChannelId encoding '[stdin, stdout, stderr]'.
data ChannelId = StdIn | StdOut | StdErr | Error | Resize deriving (Eq, Ord, Enum)

-- | An invalid channel.
newtype InvalidChannel = InvalidChannel Text deriving (Show, Typeable)


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

-- A flag from the python library.
type PreloadContent = Bool

-- | Secure web sockets connection?
data Protocol = 
  WS -- ^ "ws://abc.co" 
  | WSS -- ^ "wss://abc.co"

instance Show Protocol where 
  show WS = "ws" 
  show WSS = "wss"

-- | The host.
type Host = String
-- | The port. 
type Port = Int 

-- | The URL with "Protocol", "Host" and "Port"
newtype URL = URL {_unP :: (Protocol, Host, Port)} 

-- | The kube config.
type KubeConfig = Config

-- | Command contains the "Executable" and a list of "Arguments"
type Command = String

-- | A reader configuration when running the client.
data ExecClientConfig = 
  ExecClientConfig {
  _kubeConfig :: KubeConfig
  , _url :: URL 
  , _timeout :: Maybe TimeoutInterval
  , _preload :: Bool
  , _commands :: Command
  } 

-- | 'Show' instance for channels with 
-- | 'StdIn' -> 0
-- | 'StdOut' -> 1
-- | 'StdErr' -> 2
-- | 'Error' -> 3  -- ^ TODO : what does Error mean?
-- | 'Resize' -> 4 -- ^ TODO : how to test Resize.

instance Show ChannelId where 
  show StdIn = "0"
  show StdOut = "1" 
  show StdErr = "2"
  show Error = "3"
  show Resize = "4"

-- | Parsing incoming text can result in this exception.
-- | Currently, any channel that is not mapped to a channel, 
-- | sends the output intended for the channel to 'stderr'
instance Exception InvalidChannel

-- | Enumerate all of the channels supported.
allChannels :: [ChannelId]
allChannels = [StdIn .. Resize]

-- | Decode text to a channel.
readChannel :: Text -> Maybe ChannelId
readChannel "0" = Just StdIn
readChannel "1" = Just StdOut 
readChannel "2" = Just StdErr 
readChannel "3" = Just Error 
readChannel "4" = Just Resize 
readChannel _ = Nothing

-- | Tie the channel back to the handles on the command line.
mapChannel :: ChannelId -> Handle 
mapChannel StdIn = stdin
mapChannel StdOut = stdout
mapChannel StdErr = stderr
mapChannel _  = stderr


