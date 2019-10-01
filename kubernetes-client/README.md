# kubernetes-client

## Example

### Load KubeConfig file

```haskell
import Control.Concurrent.STM (atomically, newTVar)
import Kubernetes.Client      (KubeConfigSource (..), mkKubeClientConfig)
import Kubernetes.OpenAPI     (Accept (..), MimeJSON (..), dispatchMime)

import qualified Data.Map                      as Map
import qualified Kubernetes.OpenAPI.API.CoreV1 as CoreV1

main :: IO ()
main = do
    oidcCache <- atomically $ newTVar $ Map.fromList []
    (mgr, kcfg) <- mkKubeClientConfig oidcCache $ KubeConfigFile "/path/to/kubeconfig"
    dispatchMime
            mgr
            kcfg
            (CoreV1.listPodForAllNamespaces (Accept MimeJSON))
        >>= print
```

### Load InCluster Config

```haskell
import Control.Concurrent.STM (atomically, newTVar)
import Data.Function          ((&))
import Kubernetes.Client      (KubeConfigSource (..), mkKubeClientConfig)
import Kubernetes.OpenAPI     (Accept (..), MimeJSON (..), dispatchMime)
import Network.TLS            (credentialLoadX509)

import qualified Data.Map                      as Map
import qualified Kubernetes.OpenAPI.API.CoreV1 as CoreV1

main :: IO ()
main = do
    oidcCache <- atomically $ newTVar $ Map.fromList []
    (mgr, kcfg) <- mkKubeClientConfig oidcCache KubeConfigCluster
    dispatchMime
            mgr
            kcfg
            (CoreV1.listPodForAllNamespaces (Accept MimeJSON))
        >>= print
```

### Load config from URL and paths

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Function                 ((&))
import           Kubernetes.Client             (defaultTLSClientParams,
                                                disableServerCertValidation,
                                                disableServerNameValidation,
                                                disableValidateAuthMethods,
                                                loadPEMCerts, newManager,
                                                setCAStore, setClientCert,
                                                setMasterURI, setTokenAuth)
import           Kubernetes.OpenAPI            (Accept (..), MimeJSON (..),
                                                dispatchMime, newConfig)
import qualified Kubernetes.OpenAPI.API.CoreV1 as CoreV1
import           Network.TLS                   (credentialLoadX509)

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
            (CoreV1.listPodForAllNamespaces (Accept MimeJSON))
        >>= print
```

## Watch Example
Following is a simple example which
just streams to stdout. First some setup - this assumes kubernetes is accessible
at http://localhost:8001, e.g. after running `kubectl proxy`:

```haskell
> import qualified Data.ByteString.Streaming.Char8 as Q

> manager <- newManager defaultManagerSettings
> defaultConfig <- newConfig
> config = defaultConfig { configHost = "http://localhost:8001", configValidateAuthMethods = False }
> request = listEndpointsForAllNamespaces (Accept MimeJSON)
```

Launching 'dispatchWatch' with the above we get a stream of endpoints data:

```haskell
 > dispatchWatch manager config request Q.stdout
 {"type":\"ADDED\","object":{"kind":\"Endpoints\","apiVersion":"v1","metadata":{"name":"heapster" ....
```

A more complex example involving some ggprocessing of the stream, the following
prints out the event types of each event. First, define functions to allow us apply
a parser to a stream:


```haskell
import Data.Aeson 
import qualified Data.ByteString.Streaming.Char8 as Q
import Data.JsonStream.Parser
import qualified Streaming.Prelude as S

-- | Parse the stream using the given parser.
streamParse ::
  FromJSON a =>
    Parser a
    -> Q.ByteString IO r
    -> Stream (Of [a]) IO r
streamParse parser byteStream = do
  byteStream & Q.lines & parseEvent parser

-- | Parse a single event from the stream.
parseEvent ::
  (FromJSON a, Monad m) =>
    Parser a
    -> Stream (Q.ByteString m) m r
    -> Stream (Of [a]) m r
parseEvent parser byteStream = S.map (parseByteString parser) (S.mapped Q.toStrict byteStream)
```

Next, define the parser and apply it to the stream:

```haskell 
> eventParser = value :: Parser (WatchEvent V1Endpoints)
> withResponseBody body = streamParse eventParser body & S.map (map eventType)
> dispatchWatch manager config request (S.print . withResponseBody)
[\"ADDED\"]
[\"ADDED\"]
[\"MODIFIED\"]
...
```

Packages in this example:
  * Data.Aeson -- from [aeson](https://hackage.haskell.org/package/aeson)
  * Data.ByteString.Streaming.Char8 from [streaming-bytestring](https://hackage.haskell.org/package/streaming-bytestring-0.1.5/docs/Data-ByteString-Streaming-Char8.html)
  * Data.JsonStream.Parser from [json-stream](https://hackage.haskell.org/package/json-stream-0.4.1.5/docs/Data-JsonStream-Parser.html)
  * Streaming.Prelude from [streaming](https://hackage.haskell.org/package/streaming-0.2.0.0/docs/Streaming-Prelude.html)
