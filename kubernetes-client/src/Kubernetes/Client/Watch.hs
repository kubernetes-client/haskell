{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Kubernetes.Client.Watch
  ( WatchEvent
  , eventType
  , eventObject
  , dispatchWatch
  ) where

import Control.Monad
import Control.Monad.Trans (lift)
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.Text as T
import Kubernetes.OpenAPI.Client
import Kubernetes.OpenAPI.Core
import Kubernetes.OpenAPI.MimeTypes
import Kubernetes.OpenAPI.Model (Watch(..))
import Network.HTTP.Client

#if MIN_VERSION_streaming_bytestring(0,1,7)
import qualified Streaming.ByteString.Char8 as Q
type ByteStream = Q.ByteStream
#else
import qualified Data.ByteString.Streaming.Char8 as Q
type ByteStream = Q.ByteString
#endif


data WatchEvent a = WatchEvent
  { _eventType :: T.Text
  , _eventObject :: a
  } deriving (Eq, Show)

instance FromJSON a => FromJSON (WatchEvent a) where
  parseJSON (Object x) = WatchEvent <$> x .: "type" <*> x .: "object"
  parseJSON _ = fail "Expected an object"

instance ToJSON a => ToJSON (WatchEvent a) where
  toJSON x = object
    [ "type"    .= _eventType x
    , "object"  .= _eventObject x
    ]

-- | Type of the 'WatchEvent'.
eventType :: WatchEvent a -> T.Text
eventType = _eventType

-- | Object within the 'WatchEvent'.
eventObject :: WatchEvent a -> a
eventObject = _eventObject

{-| Dispatch a request setting watch to true. Takes a consumer function
which consumes the 'Q.ByteString' stream. Following is a simple example which
just streams to stdout. First some setup - this assumes kubernetes is accessible
at http://localhost:8001, e.g. after running /kubectl proxy/:

@
import qualified Data.ByteString.Streaming.Char8 as Q

manager <- newManager defaultManagerSettings
defaultConfig <- newConfig
config = defaultConfig { configHost = "http://localhost:8001", configValidateAuthMethods = False }
request = listEndpointsForAllNamespaces (Accept MimeJSON)
@

Launching 'dispatchWatch' with the above we get a stream of endpoints data:

@
 > dispatchWatch manager config request Q.stdout
 {"type":\"ADDED\","object":{"kind":\"Endpoints\","apiVersion":"v1","metadata":{"name":"heapster" ....
@
-}
dispatchWatch ::
  (HasOptionalParam req Watch, MimeType accept, MimeType contentType) =>
    Manager
    -> KubernetesClientConfig
    -> KubernetesRequest req contentType resp accept
    -> (ByteStream IO () -> IO a)
    -> IO a
dispatchWatch manager config request apply = do
  let watchRequest = applyOptionalParam request (Watch True)
  (InitRequest req) <- _toInitRequest config watchRequest
  withHTTP req manager $ \resp -> apply $ responseBody resp

withHTTP ::
  Request
  -> Manager
  -> (Response (ByteStream IO ()) -> IO a)
  -> IO a
withHTTP request manager f = withResponse request manager f'
  where
    f' resp = do
      let p = (from . brRead . responseBody) resp
      f (resp {responseBody = p})
    from :: IO B.ByteString -> ByteStream IO ()
    from io = go
      where
        go = do
          bs <- lift io
          unless (B.null bs) $ do
            Q.chunk bs
            go
