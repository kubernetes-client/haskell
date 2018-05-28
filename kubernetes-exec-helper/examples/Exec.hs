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
import           Kubernetes.Misc
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


setupKubeConfig :: IO KubernetesConfig
setupKubeConfig = do
    home <- Text.pack <$> getEnv("HOME")
    AuthApiKeyBearerToken bearerToken <- getBearerToken $  
      Printf.printf "%s/.minikube/bearerToken.txt" home
    result <- 
      newConfig
      & fmap (setMasterURI "https://192.168.99.100")    -- fill in master URI
      & fmap disableValidateAuthMethods  -- if using client cert auth
      & fmap (setTokenAuth bearerToken)
    return result



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
  client <- async $ WSClient.runClient clientState kubeConfig tlsParams name namespace
  outputAsyncs <- mapM (\(channelId, channel) -> async(output channelId channel)) 
    $ Prelude.filter(\(cId, _) -> cId /= K8SChannel.StdIn) $ 
      CreateWSClient.channels clientState
  _ <- waitAny $ client : outputAsyncs
  return ()
  where 
    name = Name containerName 
    namespace = Namespace "default"
    iterContainers :: ClientParams -> KubernetesConfig -> IO ()
    iterContainers tlsParams kubeConfig = do
          return ()

-- | A sample test setup, that should be moved to 
-- | test spec. 
main :: IO ()
main = setupAndRun "busybox-test"


{- | 
  Read commands from std in and send it to the pod.
-}
readCommands :: TChan Text -> IO ()
readCommands writerChannel = do 
  hSetBuffering stdin NoBuffering
  line <- Text.pack <$> Prelude.getLine
  atomically $ writeTChan writerChannel $ line
  readCommands writerChannel

