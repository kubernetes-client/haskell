{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
  -- Module : Kubernetes.WSClient
  -- Description : This module manages all connections and channels associated with 
  -- the channels. 
  -- Note : Flush channels happens after any of the threads close, therefore 
  -- the flush operation may not completely flush all the contents.
-}
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
import Control.Concurrent.Async(waitAny, async, Async)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever)
import Control.Monad.Reader
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)
import Kubernetes.K8SChannel
import Kubernetes.KubeConfig
import Network.Socket(withSocketsDo)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import System.Timeout (timeout) 
import System.IO (hSetBuffering, BufferMode(..), stdin, stdout, stderr)


-- | State maintains all threads that are running, 
-- | a writer channel to send messages to the server 
-- | and a list of all 'ChannelId' associated with a channel.
-- | Clients can wait on '[Async ThreadId]' and proceed to work with each 
-- | channel.
newtype ClientState a = ClientState {
    _unState :: 
      ([Async ThreadId], TChan a, [(ChannelId, TChan a)])
    }

-- | A flag from the python library.
type PreloadContent = Bool

-- | A simple case to distinguish secure web sockets or otherwise.
data Protocol = WS | WSS


instance Show Protocol where 
  show WS = "ws" 
  show WSS = "wss"

type Host = String 
type Port = Int 

newtype URL = URL {_unP :: (Protocol, Host, Port)} 

type KubeConfig = String -- todo : need help here.

-- | A reader configuration when running the client.
type ExecClientConfig = (KubeConfig, URL, Maybe TimeoutInterval, PreloadContent)
newtype KubernetesClientApp a = 
  KubernetesClientApp 
    {runA :: ReaderT ExecClientConfig IO a }
    deriving (Monad, MonadIO, Functor, Applicative
      , MonadReader ExecClientConfig)

-- | The core application when a user attaches a command to the pod.
runApp :: KubeConfig -> Protocol -> Host -> Port -> Maybe TimeoutInterval -> PreloadContent -> IO () 
runApp kC proto host port timeout preloadContent = do
  let config = (kC, (URL (proto, host, port)), timeout, preloadContent) 
  runReaderT (runA exec) config

-- | Read the text from the channels and direct to the appropriate 
-- | local channel.

writeToLocalChannels :: [(ChannelId, TChan Text)] -> IO [Async ThreadId]
writeToLocalChannels channels = 
  mapM (flip writeToLocalChannel channels) $ [StdIn, StdOut, StdErr] 

-- | Write to a local channel. The word local channel is used to represent the 
-- | command line from which a user is running a command.

writeToLocalChannel :: ChannelId -> [(ChannelId, TChan Text)] -> IO (Async ThreadId)
writeToLocalChannel channelId channels = do 
  let std = mapChannel channelId
  hSetBuffering std NoBuffering
  async $ forever $ do
        message <- atomically $ readTChan $ getTChanSTM channelId channels
        T.hPutStr std message

-- | Send 'stdin' to the pod.
readFromStdIn :: TChan Text -> IO (Async ThreadId)
readFromStdIn outputChan = do 
  hSetBuffering stdin NoBuffering 
  async $ forever $ do 
    text <- T.hGetLine stdin 
    atomically $ writeTChan outputChan $ (T.pack $ show StdIn) <> text



{- | Start the web socket client. 
 - | Setup local writers 
 - | Setup a command thread to send commands to the server 
 - | Flush all channels after any of the threads stops.
-}
exec :: KubernetesClientApp ()
exec = do 
  (cfg, URL (proto, host, port), interval, preloadContent) :: ExecClientConfig <- ask
  liftIO $ do 
    ClientState (threads, writer, readers) <- runClient (show (proto :: Protocol) <> "://" <> host) (port) "/" interval
    -- Start all threads to publish to the appropriate channels.
    writers <- writeToLocalChannels readers 
    reader <- readFromStdIn writer
    waitAny_ $ reader : (threads ++ writers)
    flushLocalChannels readers
  where
    waitAny_ :: [Async a] -> IO ()
    waitAny_ threads = waitAny threads >> return ()

flushLocalChannels :: [(ChannelId, TChan Text)] -> IO () 
flushLocalChannels channels = 
  mapM_ flushLocalChannel channels 
  where 
    flushLocalChannel :: (ChannelId, TChan Text) -> IO ()
    flushLocalChannel (channelId, tChan) = do 
      chanEmpty <- atomically $ isEmptyTChan tChan 
      when (not chanEmpty) $ do 
        let std = mapChannel channelId
        hSetBuffering std NoBuffering
        message <- atomically $ readTChan $ getTChanSTM channelId channels
        T.hPutStr std message
        flushLocalChannel (channelId, tChan)


-- | Run the web socket client.
runClient :: String -- ^ Host  
            -> Int  -- ^ Port 
            -> String -- ^ Path
            -> Maybe TimeoutInterval -- ^ Channel timeout.
            -> IO (ClientState Text)
runClient domain port route = \timeout' -> 
  withSocketsDo $ WS.runClient domain port route (\c -> k8sClient c timeout')

-- | Socket IO handler.
k8sClient :: WS.Connection -> Maybe TimeoutInterval -> IO (ClientState Text)
k8sClient conn interval = do
    cW <- atomically newTChan :: IO (TChan Text)
    c0 <- atomically newTChan
    c1 <- atomically newTChan 
    c2 <- atomically newTChan 
    c3 <- atomically newTChan 
    c4 <- atomically newTChan
    let channels = zip allChannels [c0, c1, c2, c3, c4]
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
      atomically $
        writeTChan (snd $ getChannelIdSTM aChan channels) message

getTChanSTM :: ChannelId -> [(ChannelId, TChan Text)] -> TChan Text 
getTChanSTM a b = snd $ getChannelIdSTM a b 
getChannelIdSTM :: ChannelId -> [(ChannelId, TChan Text)] -> (ChannelId, TChan Text)
getChannelIdSTM aChannelId channels = 
  head $ filter(\(x, _) -> x == aChannelId) channels

-- | * Readers
readChannelIdSTM :: ChannelId -> [(ChannelId, TChan Text)] -> STM Text 
readChannelIdSTM channel channels = 
    readTChan $ snd $ getChannelIdSTM channel channels
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



