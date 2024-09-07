{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Kubernetes.Client.Auth.TokenFile where

import           Control.Concurrent.STM
import           Data.Function                  ( (&) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Time.Clock
import           Kubernetes.Client.Auth.Internal.Types
import           Kubernetes.Client.KubeConfig hiding ( token )
import           Kubernetes.OpenAPI.Core
import qualified Lens.Micro                    as L

#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid                    ( (<>) )
#endif


data TokenFileAuth = TokenFileAuth { token :: TVar(Maybe Text)
                                   , expiry :: TVar(Maybe UTCTime)
                                   , file :: FilePath
                                   , period :: NominalDiffTime
                                   }

instance AuthMethod TokenFileAuth where
  applyAuthMethod _ tokenFile req = do
    t <- getToken tokenFile
    pure
      $           req
      `setHeader` toHeader ("authorization", "Bearer " <> t)
      &           L.set rAuthTypesL []

-- |Detects if token-file is specified in AuthConfig.
tokenFileAuth :: DetectAuth
tokenFileAuth auth (tlsParams, cfg) = do
  file <- tokenFile auth
  return $ do
    c <- setTokenFileAuth file cfg
    return (tlsParams, c)

-- |Configures the 'KubernetesClientConfig' to use TokenFile authentication.
setTokenFileAuth
  :: FilePath -> KubernetesClientConfig -> IO KubernetesClientConfig
setTokenFileAuth f kcfg = atomically $ do
  t <- newTVar (Nothing :: Maybe Text)
  e <- newTVar (Nothing :: Maybe UTCTime)
  return kcfg
    { configAuthMethods =
      [ AnyAuthMethod
          (TokenFileAuth { token = t, expiry = e, file = f, period = 60 })
      ]
    }

getToken :: TokenFileAuth -> IO Text
getToken auth = getCurrentToken auth >>= maybe (reloadToken auth) return

getCurrentToken :: TokenFileAuth -> IO (Maybe Text)
getCurrentToken TokenFileAuth { token, expiry } = do
  now         <- getCurrentTime
  maybeExpiry <- readTVarIO expiry
  maybeToken  <- readTVarIO token
  return $ do
    e <- maybeExpiry
    if e > now then maybeToken else Nothing

reloadToken :: TokenFileAuth -> IO Text
reloadToken TokenFileAuth { token, expiry, file, period } = do
  content <- T.readFile file
  let t = T.strip content
  now <- getCurrentTime
  atomically $ do
    writeTVar token  (Just t)
    writeTVar expiry (Just (addUTCTime period now))
  return t
