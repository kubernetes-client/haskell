{-# LANGUAGE OverloadedStrings #-}

module Kubernetes.WSClient
    (
      -- * App
      runClient
      -- * Reads      
      , readErr
      , readErrSTM
      , readLine
      , readLineSTM
      , readResize
      , readResizeSTM
      , readStdErr
      , readStdErrSTM
      , readStdIn
      , readStdInSTM
      , readStdOut 
      , readStdOutSTM
      -- *Writes 
      , writeErr
      , writeErrSTM
      , writeResize
      , writeResizeSTM
      , writeStdErr
      , writeStdErrSTM
      , writeStdIn 
      , writeStdInSTM
      , writeStdOut
      , writeStdOutSTM
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
            publishMessage channels $ T.splitAt 1 msg
      timedThread aWorker channels timeoutInterval =
        case timeoutInterval of  
            Nothing ->  Just <$> aWorker channels
            Just m -> timeout m $ aWorker channels

-- Publish messages from the reader into the channel.
publishMessage :: [(ChannelId, TChan Text)] -> (Text, Text) -> IO ()
publishMessage channels (channel, message) = do 
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


-- | * Readers
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
  messages <- T.split (== '\n') <$> readTChan aChannel 
  case messages of 
    h : t -> do 
        unGetTChan aChannel $ T.unlines t
        return $ h <> "\n" 
    _ -> return ""

readLine :: TChan Text -> IO Text 
readLine = atomically . readLineSTM

writeErrSTM :: Text -> TChan Text -> STM ()
writeErrSTM = writeChannelIdSTM Error

writeResizeSTM :: Text -> TChan Text -> STM () 
writeResizeSTM = writeChannelIdSTM Resize

writeStdErrSTM :: Text -> TChan Text -> STM () 
writeStdErrSTM = writeChannelIdSTM StdErr

writeStdOutSTM :: Text -> TChan Text -> STM () 
writeStdOutSTM = writeChannelIdSTM StdOut

writeStdInSTM :: Text -> TChan Text -> STM () 
writeStdInSTM = writeChannelIdSTM StdIn

writeErr :: Text -> TChan Text -> IO ()
writeErr = \text chan -> atomically (writeErrSTM text chan)

writeResize :: Text -> TChan Text -> IO ()
writeResize = \text chan -> atomically (writeResizeSTM text chan)

writeStdIn :: Text -> TChan Text -> IO () 
writeStdIn = \text chan -> atomically (writeStdInSTM text chan) 

writeStdOut :: Text -> TChan Text -> IO () 
writeStdOut = \text chan -> atomically (writeStdOutSTM text chan) 

writeStdErr :: Text -> TChan Text -> IO ()
writeStdErr = \text chan -> atomically (writeStdErrSTM text chan)


-- Write 'content' to 'ChannelId'.
writeChannelIdSTM :: ChannelId -> Text -> TChan Text -> STM () 
writeChannelIdSTM channelId content chan = 
  writeTChan chan $ T.pack (show Error) <> content



