{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Kubernetes.Watch.Client
  ( WatchEvent
  , eventType
  , eventObject
  , dispatchWatch
  ) where

import Control.Monad
import Control.Monad.Trans (lift)
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Streaming.Char8 as Q
import qualified Data.Text as T
import Kubernetes.Core
import Kubernetes.Client
import Kubernetes.MimeTypes
import Kubernetes.Model (Watch(..))
import Network.HTTP.Client

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
import qualified Data.ByteString.Streaming.Char8 as Q -- from <https://hackage.haskell.org/package/streaming-bytestring-0.1.5/docs/Data-ByteString-Streaming-Char8.html streaming-bytestring>

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

A more complex example involving some processing of the stream, the following
prints out the event types of each event. First, define functions to allow us apply
a parser to a stream:

@
import Data.Aeson -- from <https://hackage.haskell.org/package/aeson aeson>
import qualified Data.ByteString.Streaming.Char8 as Q -- from <https://hackage.haskell.org/package/streaming-bytestring-0.1.5/docs/Data-ByteString-Streaming-Char8.html streaming-bytestring>
import Data.JsonStream.Parser -- from <https://hackage.haskell.org/package/json-stream-0.4.1.5/docs/Data-JsonStream-Parser.html json-stream>
import qualified Streaming.Prelude as S -- from <https://hackage.haskell.org/package/streaming-0.2.0.0/docs/Streaming-Prelude.html streaming>

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
@

Next, define the parser and apply it to the stream:

@
> eventParser = value :: Parser (WatchEvent V1Endpoints)
> withResponseBody body = streamParse eventParser body & S.map (map eventType)
> dispatchWatch manager config request (S.print . withResponseBody)
[\"ADDED\"]
[\"ADDED\"]
[\"MODIFIED\"]
...
@

This prints the type of each event to stdout.
-}
dispatchWatch ::
  (HasOptionalParam req Watch, MimeType accept, MimeType contentType) =>
    Manager
    -> KubernetesConfig
    -> KubernetesRequest req contentType resp accept
    -> (Q.ByteString IO () -> IO a)
    -> IO a
dispatchWatch manager config request apply = do
  let watchRequest = applyOptionalParam request (Watch True)
  (InitRequest req) <- _toInitRequest config watchRequest
  withHTTP req manager $ \resp -> apply $ responseBody resp

withHTTP ::
  Request
  -> Manager
  -> (Response (Q.ByteString IO ()) -> IO a)
  -> IO a
withHTTP response manager f = withResponse response manager f'
  where
    f' resp = do
      let p = (from . brRead . responseBody) resp
      f (resp {responseBody = p})

from :: IO B.ByteString -> Q.ByteString IO ()
from io = go
  where
    go = do
      bs <- lift io
      unless (B.null bs) $ do
        Q.chunk bs
        go
