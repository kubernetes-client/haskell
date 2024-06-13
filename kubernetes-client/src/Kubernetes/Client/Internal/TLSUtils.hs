{-# LANGUAGE OverloadedStrings #-}
module Kubernetes.Client.Internal.TLSUtils where

import Control.Exception.Safe     (Exception, MonadThrow, throwM)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Data.ByteString            (ByteString)
import Data.Default.Class         (def)
import Data.Either                (rights)
import Data.Either.Combinators    (mapLeft)
import Data.Function              ((&))
import Data.PEM                   (pemContent, pemParseBS)
import Data.X509                  (SignedCertificate, decodeSignedCertificate)
import Data.X509.CertificateStore (CertificateStore, makeCertificateStore)
import Lens.Micro
import Network.TLS                (Credential, credentialLoadX509FromMemory, defaultParamsClient)
import System.X509                (getSystemCertificateStore)

import qualified Data.ByteString        as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.X509              as X509
import qualified Data.X509.Validation   as X509
import qualified Network.TLS            as TLS
import qualified Network.TLS.Extra      as TLS

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

-- |Parses a PEM-encoded @ByteString@ into a list of certificates.
parsePEMCerts :: B.ByteString -> Either ParseCertException [SignedCertificate]
parsePEMCerts pemBS = do
    pems <- pemParseBS pemBS
            & mapLeft PEMParsingFailed
    return $ rights $ map (decodeSignedCertificate . pemContent) pems

-- | Updates client params, sets CA store to passed bytestring of CA certificates
updateClientParams :: TLS.ClientParams -> ByteString -> Either ParseCertException TLS.ClientParams
updateClientParams cp certText = parsePEMCerts certText
                                 & (fmap (flip setCAStore cp))

-- |Use a custom CA store.
setCAStore :: [SignedCertificate] -> TLS.ClientParams -> TLS.ClientParams
setCAStore certs tlsParams =
  tlsParams & clientSharedL . sharedCAStoreL .~ makeCertificateStore certs

-- |Use a client cert for authentication.
setClientCert :: Credential -> TLS.ClientParams -> TLS.ClientParams
setClientCert cred = set onCertificateRequestL (\_ -> return $ Just cred)

clientHooksL :: Lens' TLS.ClientParams TLS.ClientHooks
clientHooksL = lens TLS.clientHooks (\cp ch -> cp { TLS.clientHooks = ch })

onServerCertificateL :: Lens' TLS.ClientParams (CertificateStore -> TLS.ValidationCache -> X509.ServiceID -> X509.CertificateChain -> IO [X509.FailedReason])
onServerCertificateL =
  clientHooksL . lens TLS.onServerCertificate (\ch osc -> ch { TLS.onServerCertificate = osc })

clientSharedL :: Lens' TLS.ClientParams TLS.Shared
clientSharedL = lens TLS.clientShared (\tlsParams cs -> tlsParams {TLS.clientShared = cs} )

sharedCAStoreL :: Lens' TLS.Shared CertificateStore
sharedCAStoreL = lens TLS.sharedCAStore (\shared store -> shared{TLS.sharedCAStore = store})

-- |Don't check whether the cert presented by the server matches the name of the server you are connecting to.
-- This is necessary if you specify the server host by its IP address.
disableServerNameValidation :: TLS.ClientParams -> TLS.ClientParams
disableServerNameValidation =
  set onServerCertificateL (X509.validate X509.HashSHA256 def (def { X509.checkFQHN = False }))

-- |Insecure mode. The client will not validate the server cert at all.
disableServerCertValidation :: TLS.ClientParams -> TLS.ClientParams
disableServerCertValidation = set onServerCertificateL (\_ _ _ _ -> return [])

onCertificateRequestL :: Lens' TLS.ClientParams (([TLS.CertificateType], Maybe [TLS.HashAndSignatureAlgorithm], [X509.DistinguishedName]) -> IO (Maybe (X509.CertificateChain, TLS.PrivKey)))
onCertificateRequestL =
  clientHooksL . lens TLS.onCertificateRequest (\ch ocr -> ch { TLS.onCertificateRequest = ocr })

-- |Loads certificates from a PEM-encoded file.
loadPEMCerts :: (MonadIO m, MonadThrow m) => FilePath -> m [SignedCertificate]
loadPEMCerts pemFile = do
    liftIO (B.readFile pemFile)
        >>= (either throwM return)
        .   parsePEMCerts

-- |Loads Base64 encoded certificate and private key
loadB64EncodedCert :: (MonadThrow m) => B.ByteString -> B.ByteString -> m Credential
loadB64EncodedCert certB64 keyB64 = either throwM pure $ do
  certText <- B64.decode certB64
              & mapLeft Base64ParsingFailed
  keyText <- B64.decode keyB64
              & mapLeft Base64ParsingFailed
  credentialLoadX509FromMemory certText keyText
    & mapLeft FailedToLoadCredential

data ParseCertException = PEMParsingFailed String
                        | Base64ParsingFailed String
                        | FailedToLoadCredential String
  deriving Show

instance Exception ParseCertException
