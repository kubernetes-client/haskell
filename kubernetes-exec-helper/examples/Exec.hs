{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import           GHC.Conc(labelThread)
import           Control.Monad(forever, replicateM, replicateM_)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent(threadDelay)
import           Control.Exception.Safe
import           System.IO
import qualified Kubernetes.K8SChannel as K8SChannel
import           Data.Function
import           Data.Text 
import           Data.Text.IO as T
import           Text.Printf as Printf
import           Data.Yaml (decodeFile, decodeEither, decodeFileEither, ParseException)
import           Data.Maybe (fromJust)
import           Kubernetes.Client (dispatchMime, MimeResult)
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
import           Network.TLS             (credentialLoadX509)

output :: K8SChannel.ChannelId -> TChan Text -> IO ()
output aChannelId aChannel = do 
  let handle = K8SChannel.mapChannel aChannelId
  hSetBuffering handle NoBuffering
  forever $ do 
    text <- readLine $ aChannel
    T.hPutStr handle text

setupKubeConfig :: IO (Kubernetes.Client.MimeResult V1Container)
setupKubeConfig = do
    kcfg <-
        newConfig
        & fmap (setMasterURI "https://192.168.99.100:8443")    -- fill in master URI
        & fmap disableValidateAuthMethods  -- if using client cert auth

    myCAStore <- loadPEMCerts "/home/dinkarganti/.minikube/ca.crt" -- if using custom CA certs
    myCert    <- credentialLoadX509 "/home/dinkarganti/.minikube/client.crt" "/home/dinkarganti/.minikube/client.key"
                  >>= either error return

    tlsParams <-
        defaultTLSClientParams
        & fmap disableServerNameValidation -- if master address is specified as an IP address
        & fmap disableServerCertValidation -- if you don't want to validate the server cert at all (insecure)          
        & fmap (setCAStore myCAStore)      -- if using custom CA certs
        & fmap (setClientCert myCert)      -- if using client cert

    manager <- newManager tlsParams
    dispatchMime
            manager
            kcfg
            (Kubernetes.API.CoreV1.listPodForAllNamespaces (Accept MimeJSON))
    

getContainer :: IO V1Container
getContainer = undefined


setupAndRun :: IO ()
setupAndRun = do
  kubeConfig <- setupKubeConfig
  testContainer <- getContainer
  clientState <- createWSClient testContainer $ 
      ([
        Command $ "/bin/sh -c echo This message goes to stderr >&2; echo This message goes to stdout"])
  client <- async (WSClient.runClient clientState)
  outputAsyncs <- mapM (\(channelId, channel) -> async(output channelId channel)) 
    $ Prelude.filter(\(cId, _) -> cId /= K8SChannel.StdIn) $ 
      CreateWSClient.channels clientState
  _ <- waitAny $ client : outputAsyncs
  print ("Bye." :: String)
  return ()

-- | A sample test setup, that should be moved to 
-- | test spec. 
main :: IO ()
main = setupAndRun
