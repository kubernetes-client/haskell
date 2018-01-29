# kubernetes-client-helper

Library of convenience functions for working with the `kubernetes` package.

## Example

Include the following packages as dependencies:
- kubernetes
- kubernetes-client-helper
- tls

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Function           ((&))
import qualified Kubernetes.API.CoreV1
import           Kubernetes.Client       (dispatchMime)
import           Kubernetes.ClientHelper
import           Kubernetes.Core         (newConfig)
import           Kubernetes.MimeTypes    (Accept (..), MimeJSON (..))
import           Network.TLS             (credentialLoadX509)

main :: IO ()
main = do
    -- We need to first create a Kubernetes.Core.KubernetesConfig and a Network.HTTP.Client.Manager.
    -- Currently we need to construct these objects manually. Work is underway to construct these
    -- objects automatically from a kubeconfig file. See https://github.com/kubernetes-client/haskell/issues/2.
    kcfg <-
        newConfig
        & fmap (setMasterURI "https://mycluster.example.com")    -- fill in master URI
        & fmap (setTokenAuth "mytoken")                          -- if using token auth
        & fmap disableValidateAuthMethods                        -- if using client cert auth
    myCAStore <- loadPEMCerts "/path/to/ca.crt"                  -- if using custom CA certs
    myCert    <-                                                 -- if using client cert
        credentialLoadX509 "/path/to/client.crt" "/path/to/client.key"
            >>= either error return
    tlsParams <-
        defaultTLSClientParams
        & fmap disableServerNameValidation -- if master address is specified as an IP address
        & fmap disableServerCertValidation -- if you don't want to validate the server cert at all (insecure)
        & fmap (setCAStore myCAStore)      -- if using custom CA certs
        & fmap (setClientCert myCert)      -- if using client cert
    manager <- newManager tlsParams
    dispatchMime
            manager
            kcfg
            (Kubernetes.API.CoreV1.listPodForAllNamespaces (Accept MimeJSON))
        >>= print
```
s
