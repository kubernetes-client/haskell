{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Kubernetes.Data.K8sJSONPath where

import Control.Applicative  ((<|>))
import Data.Aeson
import Data.Aeson.Text
import Data.Attoparsec.Text ( many1, char, takeWhile1, Parser )
import Data.JSONPath
import Data.Monoid     ((<>))
import Data.Text       as Text
import Data.Text.Lazy       (toStrict)


data K8sPathElement = PlainText Text
                    | JSONPath [JSONPathElement]
  deriving  (Show, Eq)

k8sJSONPath :: Parser [K8sPathElement]
k8sJSONPath = many1 pathElementParser

pathElementParser :: Parser K8sPathElement
pathElementParser = jsonpathParser <|> plainTextParser

plainTextParser :: Parser K8sPathElement
plainTextParser = PlainText <$> takeWhile1 (/= '{')

jsonpathParser :: Parser K8sPathElement
#if MIN_VERSION_jsonpath(0,3,0)
jsonpathParser = JSONPath <$> (char '{' *> jsonPath undefined <* char '}')
#else
jsonpathParser = JSONPath <$> (char '{' *> jsonPath <* char '}')
#endif

runJSONPath :: [K8sPathElement] -> Value -> Either String Text
runJSONPath [] _ = pure ""
runJSONPath (e:es) v = do
  res <- runPathElement e v
  rest <- runJSONPath es v
  pure $ res <> rest

runPathElement :: K8sPathElement -> Value -> Either String Text
runPathElement (PlainText t) _ = pure t
runPathElement (JSONPath p) v  = encodeResult $ executeJSONPath p v

#if MIN_VERSION_jsonpath(0,3,0)
encodeResult :: [Value] -> Either String Text
encodeResult  vals = return $ (intercalate " " $ Prelude.map jsonToText vals)
#else
encodeResult :: ExecutionResult Value -> Either String Text
encodeResult (ResultValue val) = return $ jsonToText val
encodeResult (ResultList vals) = return $ (intercalate " " $ Prelude.map jsonToText vals)
encodeResult (ResultError err) = Left err
#endif

jsonToText :: Value -> Text
jsonToText (String t) = t
jsonToText x          = toStrict $ encodeToLazyText x
