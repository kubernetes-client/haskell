{-# LANGUAGE OverloadedStrings #-}

module Kubernetes.Client.Config
  ( KubeConfigSource(..)
  , addCACertData
  , addCACertFile
  , applyAuthSettings
  , applyDetectAuths
  , defaultDetectAuths
  , clientHooksL
  , defaultTLSClientParams
  , disableServerCertValidation
  , disableServerNameValidation
  , disableValidateAuthMethods
  , loadPEMCerts
  , mkInClusterClientConfig
  , mkKubeClientConfig
  , mkKubeClientConfigWith
  , newManager
  , onCertificateRequestL
  , onServerCertificateL
  , parsePEMCerts
  , serviceAccountDir
  , setCAStore
  , setClientCert
  , setMasterURI
  , setTokenAuth
  , tlsValidation
  )
where

import qualified Kubernetes.OpenAPI.Core       as K

import           Control.Applicative            ( (<|>) )
import           Control.Exception.Safe         ( MonadThrow
                                                , throwM
                                                )
import           Control.Monad                  ( msum )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Base64        as B64
import qualified Data.ByteString.Lazy          as LazyB
import           Data.Either.Combinators
import           Data.Function                  ( (&) )
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Data.Yaml
import           Kubernetes.Client.Auth.Basic
import           Kubernetes.Client.Auth.ClientCert
import           Kubernetes.Client.Auth.GCP
import           Kubernetes.Client.Auth.Internal.Types
import           Kubernetes.Client.Auth.OIDC
import           Kubernetes.Client.Auth.Token
import           Kubernetes.Client.Auth.TokenFile
import           Kubernetes.Client.Internal.TLSUtils
import           Kubernetes.Client.KubeConfig
import           Network.Connection             ( TLSSettings(..) )
import qualified Network.HTTP.Client           as NH
import           Network.HTTP.Client.TLS        ( mkManagerSettings )
import qualified Network.TLS                   as TLS
import           System.Environment             ( getEnv )
import           System.FilePath

data KubeConfigSource = KubeConfigFile FilePath
                      | KubeConfigCluster

{-|
  Creates 'NH.Manager' and 'K.KubernetesClientConfig' for a given
  'KubeConfigSource'. It is recommended that multiple 'kubeClient' invocations
  across an application share an 'OIDCCache', this makes sure updation of OAuth
  token is synchronized across all the different clients being used.
-}
mkKubeClientConfig
  :: OIDCCache -> KubeConfigSource -> IO (NH.Manager, K.KubernetesClientConfig)
mkKubeClientConfig oidcCache =
  mkKubeClientConfigWith (defaultDetectAuths oidcCache)

-- | A variation of @mkKubeClientConfig@ where authentication methods are
-- configurable.
mkKubeClientConfigWith
  :: [DetectAuth] -> KubeConfigSource -> IO (NH.Manager, K.KubernetesClientConfig)
mkKubeClientConfigWith auths (KubeConfigFile f) = do
  kubeConfig <- decodeFileThrow f
  masterURI  <-
    server
    <$> getCluster kubeConfig
    &   either (const $ pure "localhost:8080") return
  tlsParams <- configureTLSParams kubeConfig (takeDirectory f)
  clientConfig <- K.newConfig & fmap (setMasterURI masterURI)
  (tlsParamsWithAuth, clientConfigWithAuth) <- case getAuthInfo kubeConfig of
    Left _ -> return (tlsParams, clientConfig)
    Right (_, authInfo) ->
      fromMaybe (pure (tlsParams, clientConfig)) $
      applyDetectAuths auths authInfo (tlsParams, clientConfig)
  mgr <- newManager tlsParamsWithAuth
  return (mgr, clientConfigWithAuth)
mkKubeClientConfigAuth _ KubeConfigCluster = mkInClusterClientConfig

-- |Creates 'NH.Manager' and 'K.KubernetesClientConfig' assuming it is being executed in a pod
mkInClusterClientConfig
  :: (MonadIO m, MonadThrow m) => m (NH.Manager, K.KubernetesClientConfig)
mkInClusterClientConfig = do
  caStore <- loadPEMCerts $ serviceAccountDir ++ "/ca.crt"
  defTlsParams <- liftIO defaultTLSClientParams
  mgr <- liftIO . newManager . setCAStore caStore $ disableServerNameValidation
    defTlsParams
  host <- liftIO $ getEnv "KUBERNETES_SERVICE_HOST"
  port <- liftIO $ getEnv "KUBERNETES_SERVICE_PORT"
  cfg  <- setMasterURI (T.pack $ "https://" ++ host ++ ":" ++ port) <$> liftIO
    (K.newConfig >>= setTokenFileAuth (serviceAccountDir ++ "/token"))
  return (mgr, cfg)

-- |Sets the master URI in the 'K.KubernetesClientConfig'.
setMasterURI
  :: T.Text                -- ^ Master URI
  -> K.KubernetesClientConfig
  -> K.KubernetesClientConfig
setMasterURI masterURI kcfg =
  kcfg { K.configHost = (LazyB.fromStrict . T.encodeUtf8) masterURI }

-- |Creates a 'NH.Manager' that can handle TLS.
newManager :: TLS.ClientParams -> IO NH.Manager
newManager cp = NH.newManager (mkManagerSettings (TLSSettings cp) Nothing)

serviceAccountDir :: FilePath
serviceAccountDir = "/var/run/secrets/kubernetes.io/serviceaccount"

configureTLSParams :: Config -> FilePath -> IO TLS.ClientParams
configureTLSParams cfg dir = do
  defaultTLS     <- defaultTLSClientParams
  withCACertData <- addCACertData cfg defaultTLS
  withCACertFile <- addCACertFile cfg dir withCACertData
  return $ tlsValidation cfg withCACertFile

tlsValidation :: Config -> TLS.ClientParams -> TLS.ClientParams
tlsValidation cfg tlsParams = case getCluster cfg of
  Left  _ -> tlsParams
  Right c -> case insecureSkipTLSVerify c of
    Just True -> disableServerCertValidation tlsParams
    _         -> tlsParams

addCACertData
  :: (MonadThrow m) => Config -> TLS.ClientParams -> m TLS.ClientParams
addCACertData cfg tlsParams =
  let
    eitherCertText =
      getCluster cfg
        & (>>= (maybeToRight "cert data not provided" . certificateAuthorityData
               )
          )
  in  case eitherCertText of
        Left  _          -> pure tlsParams
        Right certBase64 -> do
          certText <-
            B64.decode (T.encodeUtf8 certBase64)
              & either (throwM . Base64ParsingFailed) pure
          updateClientParams tlsParams certText & either throwM return

addCACertFile :: Config -> FilePath -> TLS.ClientParams -> IO TLS.ClientParams
addCACertFile cfg dir tlsParams = do
  let eitherCertFile =
        getCluster cfg
          >>= maybeToRight "cert file not provided"
          .   certificateAuthority
          &   fmap T.unpack
          &   fmap (dir </>)
  case eitherCertFile of
    Left  _        -> return tlsParams
    Right certFile -> do
      certText <- B.readFile certFile
      return $ updateClientParams tlsParams certText & fromRight tlsParams

-- | Try all available auth methods, except for the @Exec@ one, in order,
-- falling back to no authentication if no method matches.
applyAuthSettings
  :: OIDCCache
  -> AuthInfo
  -> (TLS.ClientParams, K.KubernetesClientConfig)
  -> IO (TLS.ClientParams, K.KubernetesClientConfig)
applyAuthSettings oidcCache auth input =
  fromMaybe (pure input) $
  applyDetectAuths (defaultDetectAuths oidcCache) auth input

-- | All available auth methods, except for the @Exec@ one.
defaultDetectAuths :: OIDCCache -> [DetectAuth]
defaultDetectAuths oidcCache =
  [ clientCertFileAuth
  , clientCertDataAuth
  , tokenAuth
  , tokenFileAuth
  , gcpAuth
  , cachedOIDCAuth oidcCache
  , basicAuth
  ]

-- | Try a list of @DetectAuth@, in order, using the first one that works.
applyDetectAuths :: [DetectAuth] -> DetectAuth
applyDetectAuths detectAuths authInfo input =
  msum $
  fmap (\detectAuth -> detectAuth authInfo input) detectAuths
