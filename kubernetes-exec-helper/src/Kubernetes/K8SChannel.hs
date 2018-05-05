{-# LANGUAGE OverloadedStrings #-}
module Kubernetes.K8SChannel where 

import Data.Typeable
import Control.Exception
import Data.Text
import System.IO (stdin, stdout, stderr, Handle)

type TimeoutInterval = Int
data ChannelId = StdIn | StdOut | StdErr | Error | Resize deriving (Eq, Ord, Enum)
newtype InvalidChannel = InvalidChannel Text deriving (Show, Typeable)

instance Show ChannelId where 
  show StdIn = "0"
  show StdOut = "1" 
  show StdErr = "2"
  show Error = "3"
  show Resize = "4"

instance Exception InvalidChannel

allChannels :: [ChannelId]
allChannels = [StdIn .. Resize]

-- TODO : Make this into an Either.
readChannel :: Text -> Maybe ChannelId
readChannel "0" = Just StdIn
readChannel "1" = Just StdOut 
readChannel "2" = Just StdErr 
readChannel "3" = Just Error 
readChannel "4" = Just Resize 
readChannel _ = Nothing

-- | Tie the channel back to the appropriate handles
mapChannel :: ChannelId -> Handle 
mapChannel StdIn = stdin
mapChannel StdOut = stdout
mapChannel StdErr = stderr
mapChannel _  = stderr


