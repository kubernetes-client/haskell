{-# LANGUAGE OverloadedStrings     #-}

module Kubernetes.ClientHelper where

import qualified Kubernetes.Core            as K
import qualified Kubernetes.Model           as K

import           Control.Exception.Safe     (Exception, MonadThrow, throwM)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as LazyB
import           Data.Default.Class         (def)
import           Data.Either                (rights)
import           Data.PEM                   (pemContent, pemParseBS)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Typeable              (Typeable)
import           Data.X509                  (SignedCertificate,
                                             decodeSignedCertificate)
import qualified Data.X509                  as X509
import           Data.X509.CertificateStore (makeCertificateStore)
import qualified Data.X509.Validation       as X509
import           Network.Connection         (TLSSettings (..))
import qualified Network.HTTP.Client        as NH
import           Network.HTTP.Client.TLS    (mkManagerSettings)
import           Network.TLS                (Credential, defaultParamsClient)
import qualified Network.TLS                as TLS
import qualified Network.TLS.Extra          as TLS
import           System.X509                (getSystemCertificateStore)

-- |Sets the master URI in the 'K.KubernetesConfig'.
setMasterURI
    :: T.Text                -- ^ Master URI
    -> K.KubernetesConfig
    -> K.KubernetesConfig
setMasterURI server kcfg =
    kcfg { K.configHost = (LazyB.fromStrict . T.encodeUtf8) server }

-- |Disables the client-side auth methods validation. This is necessary if you are using client cert authentication.
disableValidateAuthMethods :: K.KubernetesConfig -> K.KubernetesConfig
disableValidateAuthMethods kcfg = kcfg { K.configValidateAuthMethods = False }

-- |Configures the 'K.KubernetesConfig' to use token authentication.
setTokenAuth
    :: T.Text             -- ^Authentication token
    -> K.KubernetesConfig
    -> K.KubernetesConfig
setTokenAuth token kcfg = kcfg
    { K.configAuthMethods = [K.AnyAuthMethod (K.AuthApiKeyBearerToken token)]
    }

-- |Creates a 'NH.Manager' that can handle TLS.
newManager :: TLS.ClientParams -> IO NH.Manager
newManager cp = NH.newManager (mkManagerSettings (TLSSettings cp) Nothing)

-- |Default TLS settings using the system CA store.
defaultTLSClientParams :: IO TLS.ClientParams
defaultTLSClientParams = do
    let defParams = defaultParamsClient "" ""
    systemCAStore <- getSystemCertificateStore
    return defParams
        { TLS.clientSupported = def
            { TLS.supportedCiphers = TLS.ciphersuite_strong
            }
        , TLS.clientShared    = (TLS.clientShared defParams)
            { TLS.sharedCAStore = systemCAStore
            }
        }

-- |Don't check whether the cert presented by the server matches the name of the server you are connecting to.
-- This is necessary if you specify the server host by its IP address.
disableServerNameValidation :: TLS.ClientParams -> TLS.ClientParams
disableServerNameValidation cp = cp
    { TLS.clientHooks = (TLS.clientHooks cp)
        { TLS.onServerCertificate = X509.validate
            X509.HashSHA256
            def
            def { X509.checkFQHN = False }
        }
    }

-- |Insecure mode. The client will not validate the server cert at all.
disableServerCertValidation :: TLS.ClientParams -> TLS.ClientParams
disableServerCertValidation cp = cp
    { TLS.clientHooks = (TLS.clientHooks cp)
        { TLS.onServerCertificate = (\_ _ _ _ -> return [])
        }
    }

-- |Use a custom CA store.
setCAStore :: [SignedCertificate] -> TLS.ClientParams -> TLS.ClientParams
setCAStore certs cp = cp
    { TLS.clientShared = (TLS.clientShared cp)
        { TLS.sharedCAStore = (makeCertificateStore certs)
        }
    }

-- |Use a client cert for authentication.
setClientCert :: Credential -> TLS.ClientParams -> TLS.ClientParams
setClientCert cred cp = cp
    { TLS.clientHooks = (TLS.clientHooks cp)
        { TLS.onCertificateRequest = (\_ -> return (Just cred))
        }
    }

-- |Parses a PEM-encoded @ByteString@ into a list of certificates.
parsePEMCerts :: B.ByteString -> Either String [SignedCertificate]
parsePEMCerts b = do
    pems <- pemParseBS b
    return $ rights $ map (decodeSignedCertificate . pemContent) pems

data ParsePEMCertsException = ParsePEMCertsException String deriving (Typeable, Show)

instance Exception ParsePEMCertsException

-- |Loads certificates from a PEM-encoded file.
loadPEMCerts :: (MonadIO m, MonadThrow m) => FilePath -> m [SignedCertificate]
loadPEMCerts p = do
    liftIO (B.readFile p)
        >>= either (throwM . ParsePEMCertsException) return
        .   parsePEMCerts
