{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kubernetes.Client.Auth.Exec
  ( Exec
  , execAuth
  , decodeGkeGcloudAuthPluginToken
  ) where

import Data.Aeson (FromJSON (..), eitherDecodeStrict)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Data.Typeable (typeOf)
import GHC.Generics (Generic)
import Kubernetes.Client.Auth.Internal.Types (DetectAuth)
import Kubernetes.Client.KubeConfig (AuthInfo (..), ExecConfig (..), ExecEnvVar (..))
import Kubernetes.OpenAPI.Core
import qualified Data.ByteString.Char8 as ByteString
import qualified System.Process as Process

-------------------------------------------------------------------------------
-- Authentication using Auth

-- | The command that is to be run to obtain the token
data Exec = Exec
  { config :: ExecConfig
  , decodeToken :: String -> Either String Text
  }

runExec :: Exec -> IO Text
runExec Exec { config = ExecConfig {..}, decodeToken } = do
  let
    environment =
      fmap (fmap (\ExecEnvVar {..} -> (unpack name, unpack value))) env

    process =
      (Process.proc (unpack command) (fmap unpack $ fromMaybe [] args))
        { Process.env = environment }

  output <- Process.readCreateProcess process ""

  case decodeToken output of
    Left err ->
      error $ err <> " " <> unpack installHint
    Right ok ->
      pure ok

instance AuthMethod Exec where
  applyAuthMethod _ plugin req = do
    token <- runExec plugin
    pure $
      if (typeOf plugin `elem` rAuthTypes req)
        then
          (req `setHeader` toHeader ("Authorization", "Bearer " <> token))
          { rAuthTypes = [] }
        else
          req

execAuth :: (String -> Either String Text) -> DetectAuth
execAuth decodeToken AuthInfo {exec} (tlsParams, cfg) = do
  config <- exec
  pure $
    pure
      ( tlsParams
      , cfg
          { configAuthMethods = [AnyAuthMethod (Exec config decodeToken)]
          }
      )

-------------------------------------------------------------------------------
-- gke-gcloud-auth-plugin

-- | The result that is written to stdout
data Result = Result
  { kind :: Text
  , apiVersion :: Text
  , status :: Status
  } deriving (Show, Generic)
    deriving (FromJSON)

data Status = Status
  { expirationTimestamp :: Text
  , token :: Text
  } deriving (Show, Generic)
    deriving (FromJSON)

decodeGkeGcloudAuthPluginToken :: String -> Either String Text
decodeGkeGcloudAuthPluginToken str = do
  Result { status = Status { token } } <-
    eitherDecodeStrict (ByteString.pack str)
  pure token
