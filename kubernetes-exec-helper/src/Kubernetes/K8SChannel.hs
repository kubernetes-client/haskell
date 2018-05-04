{-# LANGUAGE OverloadedStrings #-}
module Kubernetes.K8SChannel where 

import Data.Typeable
import Control.Exception
import Data.Text

type TimeoutInterval = Int
data ChannelId = StdIn | StdOut | StdErr | Error | Resize deriving (Show, Eq, Ord, Enum)
newtype InvalidChannel = InvalidChannel Text deriving (Show, Typeable)

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
