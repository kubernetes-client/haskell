{-# LANGUAGE OverloadedStrings #-}
module Kubernetes.Client.Auth.Token where

import           Data.Monoid                    ( (<>) )
import           Kubernetes.Client.Auth.Internal.Types
import           Kubernetes.Client.KubeConfig   ( AuthInfo(..) )
import           Kubernetes.OpenAPI.Core        ( AnyAuthMethod(..)
                                                , KubernetesClientConfig(..)
                                                )
import           Kubernetes.OpenAPI.Model       ( AuthApiKeyBearerToken(..) )

import qualified Data.Text                     as T

-- |Detects if token is specified in AuthConfig, if it is configures 'KubernetesClientConfig' with 'AuthApiKeyBearerToken'
tokenAuth :: DetectAuth
tokenAuth auth (tlsParams, cfg) = do
  t <- token auth
  return $ return (tlsParams, setTokenAuth t cfg)

-- |Configures the 'KubernetesClientConfig' to use token authentication.
setTokenAuth
  :: T.Text                 -- ^Authentication token
  -> KubernetesClientConfig
  -> KubernetesClientConfig
setTokenAuth t kcfg = kcfg
  { configAuthMethods = [AnyAuthMethod (AuthApiKeyBearerToken $ "Bearer " <> t)]
  }
