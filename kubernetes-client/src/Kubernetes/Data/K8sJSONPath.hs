{-# LANGUAGE OverloadedStrings #-}
module Kubernetes.Data.K8sJSONPath where

import Data.Aeson
import Data.Aeson.Text
import Data.JSONPath
import Data.Map        as Map
import Data.Text       as Text

import Control.Applicative  ((<|>))
import Data.Attoparsec.Text
import Data.Bifunctor       (bimap)
import Data.String          (IsString)
import Data.Text.Lazy       (toStrict)

data K8sPathElement = PlainText Text
                    | JSONPath [JSONPathElement]
  deriving  (Show, Eq)

k8sJSONPath :: Parser [K8sPathElement]
k8sJSONPath = many1 pathElementParser

pathElementParser :: Parser K8sPathElement
pathElementParser = curlsParser <|> plainTextParser

plainTextParser :: Parser K8sPathElement
plainTextParser = PlainText <$> takeWhile1 (/= '{')

curlsParser :: Parser K8sPathElement
curlsParser = JSONPath <$> (char '{' *> jsonPath <* char '}')

runJSONPath :: [K8sPathElement] -> Value -> Either Text Text
runJSONPath [] _ = pure ""
runJSONPath (e:es) v = do
  res <- runPathElement e v
  rest <- runJSONPath es v
  pure $ res <> rest

runPathElement :: K8sPathElement -> Value -> Either Text Text
runPathElement (PlainText t) _ = pure t
runPathElement (JSONPath p) v  = encodeResult $ executeJSONPath p v

readJSONPath :: Map Text Text -> Text -> [K8sPathElement] -> [K8sPathElement]
readJSONPath m key def = case Map.lookup key m of
                           Nothing -> def
                           Just str -> case parseOnly (k8sJSONPath <* endOfInput) str of
                                         Left e  -> error e
                                         Right p -> p

encodeResult :: ExecutionResult Value -> Either Text Text
encodeResult (ResultValue val) = return $ jsonToText val
encodeResult (ResultList vals) = return $ (intercalate " " $ Prelude.map jsonToText vals)
encodeResult (ResultError err) = Left $ pack err

jsonToText :: Value -> Text
jsonToText (String t) = t
jsonToText x          = toStrict $ encodeToLazyText x
