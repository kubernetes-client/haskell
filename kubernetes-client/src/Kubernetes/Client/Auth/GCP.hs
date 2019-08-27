{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Kubernetes.Client.Auth.GCP
  ( gcpAuth )
where

import Control.Concurrent.STM
import Control.Exception.Safe                (Exception, throwM)
import Data.Attoparsec.Text
import Data.Either.Combinators
import Data.Function                         ((&))
import Data.JSONPath
import Data.Map                              (Map)
import Data.Monoid                           ((<>))
import Data.Text                             (Text)
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.RFC3339
import Kubernetes.Client.Auth.Internal.Types
import Kubernetes.Client.KubeConfig
import Kubernetes.Data.K8sJSONPath
import Kubernetes.OpenAPI.Core
import System.Process.Typed

import qualified Data.Aeson         as Aeson
import qualified Data.Map           as Map
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import qualified Lens.Micro         as L

-- TODO: Add support for scopes based token fetching
data GCPAuth = GCPAuth { gcpAccessToken :: TVar(Maybe Text)
                       , gcpTokenExpiry :: TVar(Maybe UTCTime)
                       , gcpCmd         :: ProcessConfig () () ()
                       , gcpTokenKey    :: [K8sPathElement]
                       , gcpExpiryKey   :: [K8sPathElement]
                       }

instance AuthMethod GCPAuth where
  applyAuthMethod _ gcp req = do
    token <- getToken gcp
             >>= either throwM pure
    pure
      $ setHeader req [("Authorization", "Bearer " <> (Text.encodeUtf8 token))]
      & L.set rAuthTypesL []

-- |Detects if auth-provier name is gcp, if it is configures the 'KubernetesClientConfig' with GCPAuth 'AuthMethod'
gcpAuth :: DetectAuth
gcpAuth AuthInfo{authProvider = Just(AuthProviderConfig "gcp" (Just cfg))} (tls, kubecfg)
  = Just $ do
      configOrErr <- parseGCPAuthInfo cfg
      case configOrErr of
        Left e    -> throwM e
        Right gcp -> pure (tls, addAuthMethod kubecfg gcp)
gcpAuth _ _ = Nothing

data GCPAuthParsingException = GCPAuthMissingInformation String
                             | GCPAuthInvalidExpiry String
                             | GCPAuthInvalidTokenJSONPath String
                             | GCPAuthInvalidExpiryJSONPath String
  deriving Show
instance Exception GCPAuthParsingException

data GCPGetTokenException = GCPCmdProducedInvalidJSON String
                          | GCPTokenNotFound String
                          | GCPTokenExpiryNotFound String
                          | GCPTokenExpiryInvalid String
  deriving Show
instance Exception GCPGetTokenException

getToken :: GCPAuth -> IO (Either GCPGetTokenException Text)
getToken g@(GCPAuth{..}) = getCurrentToken g
                           >>= maybe (fetchToken g) (return . Right)

getCurrentToken :: GCPAuth -> IO (Maybe Text)
getCurrentToken (GCPAuth{..}) = do
  now <- getCurrentTime
  maybeExpiry <- readTVarIO gcpTokenExpiry
  maybeToken <- readTVarIO gcpAccessToken
  return $ do
    expiry <- maybeExpiry
    if expiry > now
      then maybeToken
      else Nothing

fetchToken :: GCPAuth -> IO (Either GCPGetTokenException Text)
fetchToken GCPAuth{..} = do
  (stdOut, _) <- readProcess_ gcpCmd
  case parseTokenAndExpiry stdOut of
    Right (token, expiry) -> do
      atomically $ do
        writeTVar gcpAccessToken (Just token)
        writeTVar gcpTokenExpiry (Just expiry)
      return $ Right token
    Left x -> return $ Left x
  where
    parseTokenAndExpiry credsStr = do
      credsJSON <- Aeson.eitherDecode credsStr
                   & mapLeft GCPCmdProducedInvalidJSON
      token <- runJSONPath gcpTokenKey credsJSON
               & mapLeft GCPTokenNotFound
      expText <- runJSONPath gcpExpiryKey credsJSON
                 & mapLeft GCPTokenExpiryNotFound
      expiry <- parseExpiryTime expText
                & mapLeft GCPTokenExpiryInvalid
      return (token, expiry)

parseGCPAuthInfo :: Map Text Text -> IO (Either GCPAuthParsingException GCPAuth)
parseGCPAuthInfo m = do
  gcpAccessToken <- atomically $ newTVar $ Map.lookup "access-token" m
  eitherGCPExpiryToken <- sequence $ fmap (atomically . newTVar) lookupAndParseExpiry
  return $ do
    gcpTokenExpiry <- mapLeft GCPAuthInvalidExpiry eitherGCPExpiryToken
    cmdPath <- Text.unpack <$> lookupEither "cmd-path"
    cmdArgs <- Text.splitOn " " <$> lookupEither "cmd-args"
    gcpTokenKey <- readJSONPath m "token-key" [JSONPath [KeyChild "token_expiry"]]
                   & mapLeft GCPAuthInvalidTokenJSONPath
    gcpExpiryKey <- readJSONPath m "expiry-key" [JSONPath [KeyChild "access_token"]]
                    & mapLeft GCPAuthInvalidExpiryJSONPath
    let gcpCmd = proc cmdPath (map Text.unpack cmdArgs)
    pure $ GCPAuth{..}
  where
    lookupAndParseExpiry =
      case Map.lookup "expiry" m of
        Nothing         -> Right Nothing
        Just expiryText -> Just <$> parseExpiryTime expiryText
    lookupEither key = Map.lookup key m
                       & maybeToRight (GCPAuthMissingInformation $ Text.unpack key)

parseExpiryTime :: Text -> Either String UTCTime
parseExpiryTime s = zonedTimeToUTC <$> parseTimeRFC3339 s
                    & maybeToRight ("failed to parse token expiry time " <> Text.unpack s)

readJSONPath :: Map Text Text
             -> Text
             -> [K8sPathElement]
             -> Either String [K8sPathElement]
readJSONPath m key def = case Map.lookup key m of
                           Nothing -> pure def
                           Just str -> parseOnly (k8sJSONPath <* endOfInput) str
