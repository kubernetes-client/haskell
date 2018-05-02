{-# LANGUAGE OverloadedStrings #-}

module Kubernetes.WSStream
    (
      -- * Client connection 
      k8sClient
      -- * App
      , runClient
      , readLine
      , readResize
      , readStdIn
      , readStdOut 
      , readStdErr
      , readErr
      , isOpen
    )
  where 

import Control.Concurrent(ThreadId)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)
import Kubernetes.Util
import Network.Socket(withSocketsDo)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import System.Timeout


-- | State returns all threads that are running, a writer channel to send messages to the server 
-- | and a list of all 'ChannelId' associated with a channel.
-- | Clients can wait on '[Async ThreadId]' and proceed to work with each 
-- | channel.
newtype ClientState a = ClientState {
    _unState :: 
      ([Async ThreadId], TChan a, [(ChannelId, TChan a)])
    }

runClient :: String -- ^ Host  
            -> Int  -- ^ Port 
            -> String -- ^ Path
            -> Maybe TimeoutInterval -- ^ Channel timeout.
            -> IO (ClientState Text)
runClient domain port route = \timeout' -> 
  withSocketsDo $ WS.runClient domain port route (\c -> k8sClient c timeout')


k8sClient :: WS.Connection -> Maybe TimeoutInterval -> IO (ClientState Text)
k8sClient conn interval = do
    cW <- atomically newTChan :: IO (TChan Text)
    c1 <- atomically newTChan
    c2 <- atomically newTChan 
    c3 <- atomically newTChan 
    c4 <- atomically newTChan 
    c5 <- atomically newTChan
    let channels = zip allChannels [c1, c2, c3, c4, c5]
    rcv <- timedThread worker channels interval
    sender <- async $ forever $ do 
        nextMessage <- atomically . readTChan $ cW 
        WS.sendTextData conn nextMessage
    return . ClientState $ (catMaybes[Just sender, rcv], cW, channels)
    where
      worker channels = async $ forever $ do 
            msg <- (WS.receiveData conn) `catch` (\e@(SomeException _) -> return $ T.pack . show $ e)
            writeMsg channels $ T.splitAt 1 msg
      timedThread aWorker channels timeoutInterval =
        case timeoutInterval of  
            Nothing ->  Just <$> aWorker channels
            Just m -> timeout m $ aWorker channels

writeMsg :: [(ChannelId, TChan Text)] -> (Text, Text) -> IO ()
writeMsg channels (channel, message) = do 
  let chanId = readChannel channel
  case chanId of
    Nothing -> throwIO $ InvalidChannel channel
    Just aChan -> 
      mapM_ 
        (\(_, y) -> atomically $ writeTChan y message) 
          $ filter (\(x, _) -> x == aChan) channels

getChannelIdSTM :: ChannelId -> [(ChannelId, TChan Text)] -> (ChannelId, TChan Text)
getChannelIdSTM aChannelId channels = 
  head $ filter(\(x, _) -> x == aChannelId) channels

readStdInSTM :: [(ChannelId, TChan Text)] -> STM Text
readStdInSTM channels = readTChan $ snd $ getChannelIdSTM StdIn channels

readStdIn :: [(ChannelId, TChan Text)] -> IO Text 
readStdIn = atomically . readStdInSTM

readStdOutSTM :: [(ChannelId, TChan Text)] -> STM Text
readStdOutSTM channels = readTChan $ snd $ getChannelIdSTM StdOut channels

readStdOut :: [(ChannelId, TChan Text)] -> IO Text 
readStdOut = atomically . readStdOutSTM

readStdErrSTM :: [(ChannelId, TChan Text)] -> STM Text
readStdErrSTM channels = readTChan $ snd $ getChannelIdSTM StdErr channels

readStdErr :: [(ChannelId, TChan Text)] -> IO Text 
readStdErr = atomically . readStdErrSTM

readErrSTM :: [(ChannelId, TChan Text)] -> STM Text 
readErrSTM channels = readTChan $ snd $ getChannelIdSTM Error channels

readErr :: [(ChannelId, TChan Text)] -> IO Text 
readErr = atomically . readErrSTM

readResizeSTM :: [(ChannelId, TChan Text)] -> STM Text 
readResizeSTM channels = readTChan $ snd $ getChannelIdSTM Resize channels

readResize :: [(ChannelId, TChan Text)] -> IO Text 
readResize = atomically . readResizeSTM

readLineSTM :: TChan Text -> STM Text
readLineSTM aChannel = do 
  messages <- (T.split (\c -> c == '\n')) <$> readTChan aChannel 
  case messages of 
    h : t -> do 
        unGetTChan aChannel $ T.unlines t
        return $ h <> "\n" 
    _ -> return ""

readLine :: TChan Text -> IO Text 
readLine = atomically . readLineSTM

isOpen :: WS.Connection -> IO Bool 
isOpen = undefined
