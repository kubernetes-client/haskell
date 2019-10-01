{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM (atomically, newTVar)
import Data.Function          ((&))
import Kubernetes.Client      (KubeConfigSource (..), defaultTLSClientParams,
                               disableServerCertValidation,
                               disableServerNameValidation,
                               disableValidateAuthMethods, mkKubeClientConfig,
                               loadPEMCerts, newManager, setCAStore,
                               setClientCert, setMasterURI, setTokenAuth)
import Kubernetes.OpenAPI     (Accept (..), MimeJSON (..), dispatchMime,
                               newConfig)
import Network.TLS            (credentialLoadX509)

import qualified Data.Map                      as Map
import qualified Kubernetes.OpenAPI.API.CoreV1 as CoreV1

example :: IO ()
example = do
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
            (CoreV1.listPodForAllNamespaces (Accept MimeJSON))
        >>= print

exampleWithKubeConfig :: IO ()
exampleWithKubeConfig = do
    oidcCache <- atomically $ newTVar $ Map.fromList []
    (mgr, kcfg) <- mkKubeClientConfig oidcCache $ KubeConfigFile "/path/to/kubeconfig"
    dispatchMime
            mgr
            kcfg
            (CoreV1.listPodForAllNamespaces (Accept MimeJSON))
        >>= print

exampleWithInClusterConfig :: IO ()
exampleWithInClusterConfig = do
    oidcCache <- atomically $ newTVar $ Map.fromList []
    (mgr, kcfg) <- mkKubeClientConfig oidcCache KubeConfigCluster
    dispatchMime
            mgr
            kcfg
            (CoreV1.listPodForAllNamespaces (Accept MimeJSON))
        >>= print

main :: IO ()
main = return ()
