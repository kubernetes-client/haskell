# kubernetes-watch-client

Client for streaming events from watch enabled endpoints.

## Example
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
