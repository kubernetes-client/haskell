{-# LANGUAGE OverloadedStrings #-}
{-| 
  Module : Kubernetes.K8SChannel
  Description : This module maintains types to interact with STM channels and IO.
-}
module Kubernetes.K8SChannel where 

import Control.Exception
import Control.Exception.Safe
import Data.Text
import System.IO (stdin, stdout, stderr, Handle)

-- | An interval in seconds for the timeout.
type TimeoutInterval = Int

-- | ChannelId encoding '[stdin, stdout, stderr]'.
data ChannelId = StdIn | StdOut | StdErr | Error | Resize deriving (Eq, Ord, Enum)

-- | An invalid channel.
newtype InvalidChannel = InvalidChannel Text deriving (Show, Typeable)

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

-- | Decode text to a channel. TODO : Probably not represent this as Text and use bytestrings.
readChannel :: Char -> Maybe ChannelId
readChannel '\NUL' = Just StdIn
readChannel '\SOH' = Just StdOut 
readChannel '\STX' = Just StdErr 
readChannel '\ETX' = Just StdErr 
readChannel _ = Nothing 

-- | Tie the channel back to the handles on the command line.
mapChannel :: ChannelId -> Handle 
mapChannel StdIn = stdin
mapChannel StdOut = stdout
mapChannel StdErr = stderr
mapChannel _  = stderr


