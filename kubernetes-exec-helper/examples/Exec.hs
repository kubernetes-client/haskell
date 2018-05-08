{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- | 
  A simple pod example based on the example [here](https://github.com/kubernetes-client/python/blob/master/examples/exec.py)
. 
-}
module Main where

import System.IO
import qualified Kubernetes.WSStream as WSStream
import Data.Yaml (decodeFile, decodeEither, decodeFileEither, ParseException)
import Data.Maybe (fromJust)
import Kubernetes.KubeConfig(Config, getContext, getCluster)
import Options.Applicative 
import Data.Semigroup ((<>))

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
  config <- parse settings
  context <- return $ getContext <$> config
  cluster <- return $ getCluster <$> config
  putStrLn $ "read context" <> (show config)
  return ()
   
main = do 
  exec =<< execParser opts 
  where 
    opts = info parseSettings (fullDesc)