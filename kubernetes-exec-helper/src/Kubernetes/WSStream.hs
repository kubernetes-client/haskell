{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kubernetes.WSStream (
      -- * App
      runApp -- ^ Opens a stream and exposes 'ClientState' to its clients.
   ) where 

import Control.Concurrent(ThreadId)
import Control.Concurrent.Async(waitAny, async, Async)
import Control.Concurrent.STM
import Control.Monad(forever)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, MonadIO, liftIO, ask)
import Data.Text as T
import Data.Text.IO as T
import Data.Monoid ((<>))
import Kubernetes.K8SChannel 
import Kubernetes.WSClient as Client 
import System.IO (hSetBuffering, BufferMode(..), stdin)
import Network.HTTP.Base (urlEncodeVars)

newtype KubernetesClientApp a = 
  KubernetesClientApp 
    {runA :: ReaderT ExecClientConfig IO a }
    deriving (Monad, MonadIO, Functor, Applicative
      , MonadReader ExecClientConfig)

-- | Read the text from the channels and direct to the appropriate 
-- | local channel.
writeToLocalChannels :: [(ChannelId, TChan Text)] -> IO [Async ThreadId]
writeToLocalChannels channels = 
  mapM (writeToLocalChannel channels) $ [StdIn, StdOut, StdErr] 

-- | Write to a local channel. 
-- | === Note : The word local channel is used to represent the 
-- | command line from which a user is running a command.
writeToLocalChannel :: [(ChannelId, TChan Text)] -> ChannelId -> IO (Async ThreadId)
writeToLocalChannel channels channelId = do 
  let std = mapChannel channelId
  hSetBuffering std NoBuffering
  async $ forever $ do
        message <- atomically $ readTChan $ getTChanSTM channelId channels
        T.hPutStr std message

{- | 
  * Start the web socket client. Setup local writers .
  * Setup a command reader thread to send commands to the server. 
  * Return the 'ClientState' to 
 
 === Note : It helps to view the 'writers' and 'readers' from 
 within the process therefore, the process essentially reads from an
 input file handle and writes to channels for consumption. Readers 
 read from a channel and communicate with the output handle. This world view
 helps to get the direction right.
-}
exec :: KubernetesClientApp (ClientState Text)
exec = do 
  ExecClientConfig cfg 
        (URL (proto, host, port))
        interval
        preloadContent
        command <- ask
  liftIO $ do 
    let queryParams = urlEncodeVars [("command", command)]
    runClient (show (proto :: Protocol) <> "://" <> host) 
              (port) 
              ("/?" <> queryParams) interval

-- | The core application when a user attaches a command to the pod.
runApp :: KubeConfig -> Protocol -> Host -> Port -> 
            Maybe TimeoutInterval -> 
            PreloadContent -> Command -> IO (ClientState Text)
runApp kC proto host port timeoutInterval preloadContent command = do
  let config = ExecClientConfig kC
                      (URL (proto, host, port)) 
                      timeoutInterval
                      preloadContent
                      command
  runReaderT (runA exec) config
