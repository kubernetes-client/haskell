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
import           Data.Text 
import           Data.Text.IO as T
import           Text.Printf as Printf
import           Data.Yaml (decodeFile, decodeEither, decodeFileEither, ParseException)
import           Data.Maybe (fromJust)
import           Kubernetes.Client       (dispatchMime)
import           Kubernetes.ClientHelper
import           Network.WebSockets as WS
import           Network.Socket
import           Network.HTTP.Base (urlEncodeVars)
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
import           System.Wlog




output :: K8SChannel.ChannelId -> TChan Text -> IO ()
output aChannelId aChannel = do 
  let handle = K8SChannel.mapChannel aChannelId
--  hSetBuffering handle NoBuffering
  forever $ do 
    text <- readLine $ aChannel
    T.hPutStr handle text

getHost :: V1Container -> String 
getHost aContainer = undefined 

getPort :: V1Container -> Int 
getPort aContainer = undefined 

testContainer :: V1Container
testContainer = undefined

testSetup :: IO ()
testSetup = do
  host <- return $ getHost testContainer 
  port <- return $ getPort testContainer
  clientState <- createWSClient testContainer
  client <- async (WSClient.runClient clientState)
  outputAsyncs <- mapM (\(channelId, channel) -> async(output channelId channel)) 
    $ Prelude.filter(\(cId, _) -> cId /= K8SChannel.StdIn) $ 
      CreateWSClient.channels clientState
  waitAny $ client : outputAsyncs
  return ()
  -- start a client
  -- send some messages.
-- | A sample test setup, that should be moved to 
-- | test spec. 
mainTest = testSetup
