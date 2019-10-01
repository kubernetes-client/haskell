{-# LANGUAGE OverloadedStrings #-}
module Kubernetes.Client.Auth.Token where

import Data.Monoid                           ((<>))
import Kubernetes.Client.Auth.Internal.Types
import Kubernetes.Client.KubeConfig          (AuthInfo (..))
import Kubernetes.OpenAPI.Core               (AnyAuthMethod (..),
                                              KubernetesClientConfig (..))
import Kubernetes.OpenAPI.Model              (AuthApiKeyBearerToken (..))

import qualified Data.Text    as T
import qualified Data.Text.IO as T

-- |Detects if token is specified in AuthConfig, if it is configures 'KubernetesClientConfig' with 'AuthApiKeyBearerToken'
tokenAuth :: DetectAuth
tokenAuth auth (tlsParams, cfg) = do
  t <- token auth
  return $ return (tlsParams, setTokenAuth t cfg)

-- |Detects if token-file is specified in AuthConfig, if it is configures 'KubernetesClientConfig' with 'AuthApiKeyBearerToken'
tokenFileAuth :: DetectAuth
tokenFileAuth auth (tlsParams, cfg) = do
  file <- tokenFile auth
  return $ do
    t <- T.readFile file
    return (tlsParams, setTokenAuth t cfg)

-- |Configures the 'KubernetesClientConfig' to use token authentication.
setTokenAuth
  :: T.Text                 -- ^Authentication token
  -> KubernetesClientConfig
  -> KubernetesClientConfig
setTokenAuth t kcfg = kcfg
    { configAuthMethods = [AnyAuthMethod (AuthApiKeyBearerToken $ "Bearer " <> t)]
    }

