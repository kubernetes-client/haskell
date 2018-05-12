{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import           System.IO
import qualified Kubernetes.WSStream as WSStream
import           Data.Yaml (decodeFile, decodeEither, decodeFileEither, ParseException)
import           Data.Maybe (fromJust)
import           Kubernetes.Client       (dispatchMime)
import           Kubernetes.ClientHelper
import           Kubernetes.KubeConfig
import           Kubernetes.Model
import           Kubernetes.Core(KubernetesRequest(..), KubernetesConfig(..), newConfig)
import           Kubernetes.MimeTypes
import           Kubernetes.API.CoreV1
import           Options.Applicative 
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


exec :: Settings -> IO ()
exec settings  = do 
  settings <- parse settings
  request@(KubernetesRequest req cType res accept) <- 
    return $
      readNamespacedPod 
        (Accept MimeJSON)
        (Name "busybox-test")
        (Namespace "default")
  tlsParams <- defaultTLSClientParams
  manager <- newManager tlsParams
  config <- newConfig
  case settings of 
    Right s ->  dispatchMime 
                  manager
                  config
                  request 
                    >>= print 
    Left e -> print e 
  return ()

main :: IO ()
main = do 
  exec =<< execParser opts 
  where 
    opts = info parseSettings (fullDesc)