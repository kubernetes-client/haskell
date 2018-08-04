{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import           Control.Monad(forever)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           System.IO
import qualified Kubernetes.K8SChannel as K8SChannel
import           Data.Function
import           Data.Text as Text
import           Data.Text.IO as T
import           Text.Printf as Printf
import           Kubernetes.ClientHelper
import           Kubernetes.WSClient as WSClient
import           Kubernetes.Model
import           Kubernetes.Core(KubernetesConfig(..), newConfig)
import           Kubernetes.CreateWSClient as CreateWSClient
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
    clientKey_ <- return $ Printf.printf "%s/.minikube/client.key" $ pack home 
    myCAStore <- loadPEMCerts caStoreFile -- if using custom CA certs
    myCert    <- credentialLoadX509 clientCrt clientKey_ 
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
  let handle_ = K8SChannel.mapChannel aChannelId
  hSetBuffering handle_ NoBuffering
  forever $ do 
    text <- readLine $ aChannel
    T.hPutStr handle_ text

setupAndRun :: Text -> IO ()
setupAndRun containerName = do
  kubeConfig <- setupKubeConfig
  tlsParams <- clusterClientSetupParams
  clientState <- createWSClient $ [Command $ "ls", Command $ "-l"] -- returns the date.
  client <- WSClient.runClient clientState kubeConfig tlsParams (Name containerName) (Namespace "default")
  outputAsyncs <- mapM (\(channelId, channel) -> async(output channelId channel)) 
    $ Prelude.filter(\(cId, _) -> cId /= K8SChannel.StdIn) $ 
      CreateWSClient.channels clientState
  reader <- async $ readCommands $ writer clientState
  _ <- waitAny $ reader : client : outputAsyncs
  return ()
-- | A sample test setup, that should be moved to 
-- | test spec. 
main :: IO ()
main = do 
  handler1 <- 
    streamHandler stdout level >>=
        \h -> return $ setFormatter h (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  updateGlobalLogger "WSClient"
                   (System.Log.Logger.setLevel level . setHandlers [handler1])

  _ <- setupAndRun "busybox-test"

  return ()
  where 
    level = INFO
{- | 
  Read commands from std in and send it to the pod.
-}
readCommands :: TChan Text -> IO ()
readCommands writerChannel = do 
  hSetBuffering stdin NoBuffering
  T.putStr "k8s:>"
  line <- Text.pack <$> Prelude.getLine
  atomically $ writeTChan writerChannel $ "\NUL" <> line
  debugM "WSClient" $ "written " <> (show line)
  readCommands writerChannel

