{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import           GHC.Conc(labelThread)
import           Control.Monad(forever, replicateM, replicateM_, forM)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent(threadDelay)
import           Control.Exception.Safe
import           System.IO
import qualified Kubernetes.K8SChannel as K8SChannel
import           Data.Foldable as Foldable
import           Data.Function
import           Data.Maybe
import           Data.Proxy
import           Data.Text as Text
import           Data.Text.IO as T
import           Text.Printf as Printf
import           Data.Maybe (fromJust)
import           Kubernetes.Client (dispatchMime, MimeResult(..)) 
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
import           Network.TLS as TLS          (credentialLoadX509, ClientParams(..))
import           System.Environment
import           System.Log.Logger
import           System.Log.Handler 
import           System.Log.Handler.Simple
import           System.Log.Formatter

setupKubeConfig :: IO KubernetesConfig
setupKubeConfig = do
    home <- Text.pack <$> getEnv("HOME")
    AuthApiKeyBearerToken bearerToken <- getBearerToken $  
      Printf.printf "%s/.minikube/bearerToken.txt" home
    result <- 
      newConfig
      & fmap (setMasterURI "https://192.168.99.100:8443")    -- fill in master URI
      & fmap (setTokenAuth bearerToken)
    return result

clusterClientSetupParams :: IO TLS.ClientParams
clusterClientSetupParams = do
    home <- getEnv("HOME")
    caStoreFile <- return $ Printf.printf "%s/.minikube/ca.crt" (pack home)
    clientCrt <- return $ Printf.printf "%s/.minikube/client.crt" $ pack home
    clientKey <- return $ Printf.printf "%s/.minikube/client.key" $ pack home 
    myCAStore <- loadPEMCerts caStoreFile -- if using custom CA certs
    myCert    <- credentialLoadX509 clientCrt clientKey 
                  >>= either error return
    defaultTLSClientParams
      & fmap disableServerNameValidation -- if master address is specified as an IP address
      & fmap disableServerCertValidation -- if you don't want to validate the server cert at all (insecure)
      & fmap (setCAStore myCAStore)      -- if using custom CA certs
      & fmap (setClientCert myCert)      -- if using client cert


getBearerToken :: FilePath -> IO AuthApiKeyBearerToken 
getBearerToken aFile = 
    T.readFile aFile >>= \x -> return $ AuthApiKeyBearerToken $ "Bearer " <> x

output :: K8SChannel.ChannelId -> TChan Text -> IO ()
output aChannelId aChannel = do 
  let handle = K8SChannel.mapChannel aChannelId
  hSetBuffering handle NoBuffering
  forever $ do 
    text <- readLine $ aChannel
    T.hPutStr handle text

setupAndRun :: Text -> IO ()
setupAndRun containerName = do
  kubeConfig <- setupKubeConfig
  tlsParams <- clusterClientSetupParams
  clientState <- createWSClient $ [
        Command $ "/bin/sh -c echo This message goes to stderr >&2; echo This message goes to stdout"]
  client <- WSClient.runClient clientState kubeConfig tlsParams name namespace
  outputAsyncs <- mapM (\(channelId, channel) -> async(output channelId channel)) 
    $ Prelude.filter(\(cId, _) -> cId /= K8SChannel.StdIn) $ 
      CreateWSClient.channels clientState
  _ <- waitAny $ client : outputAsyncs
  return ()
  where 
    name = Name containerName 
    namespace = Namespace "default"
-- | A sample test setup, that should be moved to 
-- | test spec. 
main :: IO ()
main = do 
  handler1 <- 
    streamHandler stdout DEBUG >>= 
        \h -> return $ setFormatter h (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  updateGlobalLogger "WSClient"
                   (System.Log.Logger.setLevel DEBUG . setHandlers [handler1])  

  _ <- setupAndRun "busybox-test"

  return ()

{- | 
  Read commands from std in and send it to the pod.
-}
readCommands :: TChan Text -> IO ()
readCommands writerChannel = do 
  hSetBuffering stdin NoBuffering
  line <- Text.pack <$> Prelude.getLine
  atomically $ writeTChan writerChannel $ line
  readCommands writerChannel

