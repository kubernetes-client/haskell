{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import           GHC.Conc(labelThread)
import           Control.Monad(forever, replicateM, replicateM_)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent(threadDelay)
import           Control.Exception
import           System.IO
import qualified Kubernetes.K8SChannel as K8SChannel
import qualified Kubernetes.WSStream as WSStream
import           Data.Text 
import           Data.Text.IO as T
import           Text.Printf as Printf
import           Data.Yaml (decodeFile, decodeEither, decodeFileEither, ParseException)
import           Data.Maybe (fromJust)
import           Kubernetes.Client       (dispatchMime)
import           Kubernetes.ClientHelper
import           Network.WebSockets as WS
import           Network.Socket
import           Kubernetes.WSClient as WSClient
import           Kubernetes.KubeConfig
import           Kubernetes.Model
import           Kubernetes.Core(KubernetesRequest(..), KubernetesConfig(..), newConfig)
import           Kubernetes.MimeTypes
import           Kubernetes.CreateWSClient as CreateWSClient
import           Kubernetes.API.CoreV1
import           Options.Applicative
                      (execParser 
                        , Parser, info, fullDesc, help
                        , metavar, long, metavar
                        , strOption)
import           Data.Semigroup ((<>))

data Settings = Settings {
  settingsFile :: FilePath 
}
-- TODO : The sample is work in progress.
parseSettings :: Parser Settings 
parseSettings = Settings
  <$> strOption 
        (long "yaml"
          <> metavar "yaml"
          <> help "Configuration file for k8s."
        )

parse :: Settings -> IO (Either ParseException Config)
parse (Settings fileName) = decodeFileEither fileName

echoServer :: WS.ServerApp
echoServer pending = do 
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 1 `catch` (\a@(SomeException e) -> T.putStrLn $ pack $ show a)
  Prelude.mapM_ (\handle -> hSetBuffering handle NoBuffering) [stdin, stderr, stdout]
  loop conn `catch` (\a@(SomeException e) -> T.putStr ">>>loop " >> (T.putStrLn $ pack $ show a))
  where 
    loop connection = forever $ do 
      msg <- (WS.receiveData connection :: IO Text) 
      WS.sendTextData connection msg 

sendTestMessages :: TChan Text -> IO ()
sendTestMessages writerChannel = 
  atomically $ writeTChan writerChannel $ "1" <> "echo something\n"

output :: K8SChannel.ChannelId -> TChan Text -> IO ()
output aChannelId aChannel = do 
  let handle = K8SChannel.mapChannel aChannelId
  hSetBuffering handle NoBuffering
  forever $ do 
    text <- readLine $ aChannel
    T.hPutStr handle text

-- Create a test server, a test client and send some messages.
-- Close. 
-- This should all work.
testSetup :: IO ()
testSetup = do
  -- start server 
  server <- async (WS.runServer host port echoServer)
  threadDelay (1 * (10 ^ 6))
  clientState <- createWSClient host port
  client <- async (WSClient.runClient host port route timeout clientState)
  replicateM_ 5 $ sendTestMessages $ CreateWSClient.writer clientState
  mapConcurrently_ (\(channelId, channel) -> (output channelId channel)) 
    $ Prelude.filter(\(cId, _) -> cId /= K8SChannel.StdIn) $ 
      CreateWSClient.channels clientState
  
  waitAny [client, server]
  return ()
  -- start a client
  -- send some messages.
  where
    host = "localhost" 
    port = 20000
    route = "/" 
    timeout = Nothing

exec :: Settings -> IO ()
exec settings  = testSetup

mainWithOpts :: IO ()
mainWithOpts = do 
  exec =<< execParser opts 
  return ()
  where 
    opts = info parseSettings (fullDesc)

main = testSetup