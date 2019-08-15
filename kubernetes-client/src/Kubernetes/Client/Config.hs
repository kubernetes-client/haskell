{-# LANGUAGE OverloadedStrings #-}

module Kubernetes.Client.Config
  ( KubeConfigSource(..)
  , addCACertData
  , addCACertFile
  , applyAuthSettings
  , clientHooksL
  , Kubernetes.Client.Config.cluster
  , defaultTLSClientParams
  , disableServerCertValidation
  , disableServerNameValidation
  , disableValidateAuthMethods
  , kubeClient
  , loadPEMCerts
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

import qualified Kubernetes.OpenAPI.Core as K

import           Control.Applicative                 ((<|>))
import           Control.Exception.Safe              (MonadThrow, throwM)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Base64              as B64
import qualified Data.ByteString.Lazy                as LazyB
import           Data.Either.Combinators
import           Data.Function                       ((&))
import           Data.Maybe
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T
import qualified Data.Text.IO                        as T
import           Data.Yaml
import           Kubernetes.Client.Auth.ClientCert
import           Kubernetes.Client.Auth.GCP
import           Kubernetes.Client.Auth.OIDC
import           Kubernetes.Client.Auth.Token
import           Kubernetes.Client.Internal.TLSUtils
import           Kubernetes.Client.KubeConfig
import           Network.Connection                  (TLSSettings (..))
import qualified Network.HTTP.Client                 as NH
import           Network.HTTP.Client.TLS             (mkManagerSettings)
import qualified Network.TLS                         as TLS
import           System.Environment                  (getEnv)
import           System.FilePath

data KubeConfigSource = KubeConfigFile FilePath
                      | KubeConfigCluster

{-|
  Creates 'NH.Manager' and 'K.KubernetesClientConfig' for a given
  'KubeConfigSource'. It is recommended that multiple 'kubeClient' invocations
  across an application share an 'OIDCCache', this makes sure updation of OAuth
  token is synchronized across all the different clients being used.
-}
kubeClient
  :: OIDCCache
  -> KubeConfigSource
  -> IO (NH.Manager, K.KubernetesClientConfig)
kubeClient oidcCache (KubeConfigFile f) = do
  kubeConfigFile <- decodeFileThrow f
  uri <- getCluster kubeConfigFile
         & fmap server
         & either (const $ pure "localhost:8080") return
  t <- defaultTLSClientParams
       & fmap (tlsValidation kubeConfigFile)
       & (>>= (addCACertData kubeConfigFile))
       & (>>= addCACertFile kubeConfigFile (takeDirectory f))
  c <- K.newConfig & fmap (setMasterURI uri)
  (tlsParams, cfg) <-
    case getAuthInfo kubeConfigFile of
      Left _          -> return (t,c)
      Right (_, auth) -> applyAuthSettings oidcCache auth (t, c)
  mgr <- newManager tlsParams
  return (mgr, cfg)
kubeClient _ (KubeConfigCluster) = Kubernetes.Client.Config.cluster

-- |Creates 'NH.Manager' and 'K.KubernetesClientConfig' assuming it is being executed in a pod
cluster :: (MonadIO m, MonadThrow m) => m (NH.Manager, K.KubernetesClientConfig)
cluster = do
  caStore <- loadPEMCerts $ serviceAccountDir ++ "/ca.crt"
  defTlsParams <- liftIO defaultTLSClientParams
  mgr <- liftIO . newManager . setCAStore caStore $ disableServerNameValidation defTlsParams
  tok <- liftIO . T.readFile $ serviceAccountDir ++ "/token"
  host <- liftIO $ getEnv "KUBERNETES_SERVICE_HOST"
  port <- liftIO $ getEnv "KUBERNETES_SERVICE_PORT"
  cfg <- setTokenAuth tok . setMasterURI (T.pack $ "https://" ++ host ++ ":" ++ port) <$> liftIO K.newConfig
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

tlsValidation :: Config -> TLS.ClientParams -> TLS.ClientParams
tlsValidation cfg t = case getCluster cfg of
                        Left _ -> t
                        Right c -> case insecureSkipTLSVerify c of
                                     Just True -> disableServerCertValidation t
                                     _ -> t

addCACertData :: (MonadThrow m) => Config -> TLS.ClientParams -> m TLS.ClientParams
addCACertData cfg t =
  let eitherCertText = getCluster cfg
                       & (>>= (maybeToRight "cert data not provided" . certificateAuthorityData))
  in case eitherCertText of
       Left _ -> pure t
       Right certText ->
         (B64.decode $ T.encodeUtf8 certText)
         >>= updateClientParams t
         & either (throwM . ParsePEMCertsException) return

addCACertFile :: Config -> FilePath -> TLS.ClientParams -> IO TLS.ClientParams
addCACertFile cfg dir t = do
  let certFile = getCluster cfg
                 >>= maybeToRight "cert file not provided" . certificateAuthority
                 & fmap T.unpack
                 & fmap (dir </>)
  case certFile of
    Left _ -> return t
    Right f -> do
      certText <- B.readFile f
      return
        $ updateClientParams t certText
        & (fromRight t)

applyAuthSettings
  :: OIDCCache
  -> AuthInfo
  -> (TLS.ClientParams, K.KubernetesClientConfig)
  -> IO (TLS.ClientParams, K.KubernetesClientConfig)
applyAuthSettings oidcCache auth input = fromMaybe (pure input)
                                         $ clientCertFileAuth auth input
                                         <|> clientCertDataAuth auth input
                                         <|> tokenAuth auth input
                                         <|> tokenFileAuth auth input
                                         <|> gcpAuth auth input
                                         <|> cachedOIDCAuth oidcCache auth input
