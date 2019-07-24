{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Kubernetes.Client.Auth.GCP
  ( gcpAuth )
where

import Control.Concurrent.STM
import Data.Attoparsec.Text
import Data.Either.Combinators
import Data.Function                         ((&))
import Data.JSONPath
import Data.Map                              (Map)
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
    token <- getToken gcp >>= exceptEither
    pure
      $ setHeader req [("Authorization", "Bearer " <> (Text.encodeUtf8 token))]
      & L.set rAuthTypesL []

-- |Detects if auth-provier name is gcp, if it is configures the 'KubernetesClientConfig' with GCPAuth 'AuthMethod'
gcpAuth :: DetectAuth
gcpAuth AuthInfo{authProvider = Just(AuthProviderConfig "gcp" (Just cfg))} (tls, kubecfg)
  = Just $ do
      configOfErr <- parseGCPAuthInfo cfg
      case configOfErr of
        Left e    -> error e
        Right gcp -> pure (tls, addAuthMethod kubecfg gcp)
gcpAuth _ _ = Nothing

exceptEither :: Either String a -> IO a
exceptEither (Right a) = pure a
exceptEither (Left t)  = error (show t)

getToken :: GCPAuth -> IO (Either String Text)
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

-- TODO: log if parsed expiry is invalid
fetchToken :: GCPAuth -> IO (Either String Text)
fetchToken GCPAuth{..} = do
  (stdOut, _) <- readProcess_ gcpCmd
  let credsJSON = Aeson.eitherDecode stdOut
      token =  runJSONPath gcpTokenKey =<< credsJSON
      expText = runJSONPath gcpExpiryKey =<< credsJSON
      expiry :: Either String (Maybe UTCTime)
      expiry = Just <$> (parseExpiryTime =<< expText)
  atomically $ do
    writeTVar gcpAccessToken (rightToMaybe token)
    writeTVar gcpTokenExpiry (either (const Nothing) id expiry)
  return token

parseGCPAuthInfo :: Map Text Text -> IO (Either String GCPAuth)
parseGCPAuthInfo m = do
  gcpAccessToken <- atomically $ newTVar $ Map.lookup "access-token" m
  case maybe (pure Nothing) ((Just <$>) . parseExpiryTime) $ Map.lookup "expiry" m of
    (Left e) -> return $ Left e
    Right t -> do
      gcpTokenExpiry <- atomically $ newTVar t
      return $ do
        cmdPath <- Text.unpack <$> lookupEither m "cmd-path"
        cmdArgs <- Text.splitOn " " <$> lookupEither m "cmd-args"
        gcpTokenKey <- readJSONPath m "token-key" [JSONPath [KeyChild "token_expiry"]]
        gcpExpiryKey <- readJSONPath m "expiry-key" [JSONPath [KeyChild "access_token"]]
        let gcpCmd = proc cmdPath (map Text.unpack cmdArgs)
        pure $ GCPAuth{..}

lookupEither :: (Show key, Ord key) => Map key val -> key -> Either String val
lookupEither m k = maybeToRight e $ Map.lookup k m
                   where e = "Couldn't find key: " <> show k <> " in GCP auth info"

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
