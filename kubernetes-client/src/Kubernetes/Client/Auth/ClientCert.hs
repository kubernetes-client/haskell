module Kubernetes.Client.Auth.ClientCert where

import Data.Text.Encoding
import Kubernetes.Client.Auth.Internal.Types
import Kubernetes.Client.Internal.TLSUtils
import Kubernetes.Client.KubeConfig
import Kubernetes.OpenAPI                    (KubernetesClientConfig (..))
import Network.TLS

-- | Detects if kuebconfig file provides 'client-certificate', if it configures TLS client params with the client certificate
clientCertFileAuth :: DetectAuth
clientCertFileAuth auth (tlsParams, cfg) = do
  certFile <- clientCertificate auth
  keyFile <- clientKey auth
  return $ do
    cert <- credentialLoadX509 certFile keyFile >>= either error return
    let newParams = (setClientCert cert tlsParams)
        newCfg = (disableValidateAuthMethods cfg)
    return (newParams, newCfg)

-- | Detects if kuebconfig file provides 'client-certificate-data', if it configures TLS client params with the client certificate
clientCertDataAuth :: DetectAuth
clientCertDataAuth auth (tlsParams, cfg) = do
  certB64 <- encodeUtf8 <$> clientCertificateData auth
  keyB64 <- encodeUtf8 <$> clientKeyData auth
  Just $  do
    cert <- loadB64EncodedCert certB64 keyB64
    let newParams = (setClientCert cert tlsParams)
        newCfg = (disableValidateAuthMethods cfg)
    return (newParams, newCfg)

-- |Disables the client-side auth methods validation. This is necessary if you are using client cert authentication.
disableValidateAuthMethods :: KubernetesClientConfig -> KubernetesClientConfig
disableValidateAuthMethods kcfg = kcfg { configValidateAuthMethods = False }

