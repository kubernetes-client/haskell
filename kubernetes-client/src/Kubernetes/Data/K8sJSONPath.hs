{-# LANGUAGE OverloadedStrings #-}
module Kubernetes.Data.K8sJSONPath where

import Data.Aeson
import Data.Aeson.Text
import Data.JSONPath
import Data.Monoid     ((<>))
import Data.Text       as Text

import Control.Applicative  ((<|>))
import Data.Attoparsec.Text
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
jsonpathParser = JSONPath <$> (char '{' *> jsonPath <* char '}')

runJSONPath :: [K8sPathElement] -> Value -> Either String Text
runJSONPath [] _ = pure ""
runJSONPath (e:es) v = do
  res <- runPathElement e v
  rest <- runJSONPath es v
  pure $ res <> rest

runPathElement :: K8sPathElement -> Value -> Either String Text
runPathElement (PlainText t) _ = pure t
runPathElement (JSONPath p) v  = encodeResult $ executeJSONPath p v

encodeResult :: ExecutionResult Value -> Either String Text
encodeResult (ResultValue val) = return $ jsonToText val
encodeResult (ResultList vals) = return $ (intercalate " " $ Prelude.map jsonToText vals)
encodeResult (ResultError err) = Left err

jsonToText :: Value -> Text
jsonToText (String t) = t
jsonToText x          = toStrict $ encodeToLazyText x
