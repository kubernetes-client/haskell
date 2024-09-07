{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Kubernetes.Client.Auth.Basic where

import           Data.ByteString.Base64         ( encode )
import           Data.Function                  ( (&) )
import           Data.Text                      ( Text )
import           Kubernetes.Client.Auth.Internal.Types
import           Kubernetes.Client.KubeConfig
import           Kubernetes.OpenAPI.Core

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid                              ((<>))
#endif

import qualified Data.Text.Encoding            as T
import qualified Lens.Micro                    as L


data BasicAuth = BasicAuth { basicAuthUsername :: Text
                           , basicAuthPassword :: Text
                           }

instance AuthMethod BasicAuth where
  applyAuthMethod _ BasicAuth{..} req =
    pure
      $           req
      `setHeader` toHeader ("authorization", "Basic " <> encodeBasicAuth)
      &           L.set rAuthTypesL []
    where
      encodeBasicAuth = T.decodeUtf8 $ encode $ T.encodeUtf8 $ basicAuthUsername <> ":" <> basicAuthPassword

-- |Detects if username and password is specified in AuthConfig, if it is configures 'KubernetesClientConfig' with 'BasicAuth'
basicAuth :: DetectAuth
basicAuth auth (tlsParams, cfg) = do
  u <- username auth
  p <- password auth
  return $ return (tlsParams, setBasicAuth u p cfg)

-- |Configures the 'KubernetesClientConfig' to use basic authentication.
setBasicAuth
  :: Text                 -- ^Username
  -> Text                 -- ^Password
  -> KubernetesClientConfig
  -> KubernetesClientConfig
setBasicAuth u p kcfg = kcfg
  { configAuthMethods = [AnyAuthMethod (BasicAuth u p)]
  }
