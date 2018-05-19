{-# LANGUAGE OverloadedStrings #-}
{-| 
  Module : Kubernetes.K8SChannel
  Description : This module maintains types to interact with STM channels and IO.
-}
module Kubernetes.K8SChannel where 

import Control.Exception
import Control.Exception.Safe
import Data.Text
import Kubernetes.KubeConfig
import System.IO (stdin, stdout, stderr, Handle)

-- | An interval in seconds for the timeout.
type TimeoutInterval = Int

-- | ChannelId encoding '[stdin, stdout, stderr]'.
data ChannelId = StdIn | StdOut | StdErr | Error | Resize deriving (Eq, Ord, Enum)

-- | An invalid channel.
newtype InvalidChannel = InvalidChannel Text deriving (Show, Typeable)



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


