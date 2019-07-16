{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Kubernetes.Client.Auth.GCP
  ( gcpAuth )
where

import Control.Concurrent.STM
import Data.Bifunctor                        (first)
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
        Left e    -> error $ Text.unpack e
        Right gcp -> pure (tls, addAuthMethod kubecfg gcp)
gcpAuth _ _ = Nothing

exceptEither :: Either Text a -> IO a
exceptEither (Right a) = pure a
exceptEither (Left t)  = error (show t)

getToken :: GCPAuth -> IO (Either Text Text)
getToken g@(GCPAuth{..}) = getCurrentToken g
                           >>= maybe (fetchToken g) (return . Right)

getCurrentToken :: GCPAuth -> IO (Maybe Text)
getCurrentToken (GCPAuth{..}) = do
  now <- getCurrentTime
  maybeExpiry <- atomically $ readTVar gcpTokenExpiry
  maybeToken <- atomically $ readTVar gcpAccessToken
  return $ do
    expiry <- maybeExpiry
    if expiry > now
      then maybeToken
      else Nothing

-- TODO: log if parsed expiry is invalid
fetchToken :: GCPAuth -> IO (Either Text Text)
fetchToken GCPAuth{..} = do
  (stdOut, _) <- readProcess_ gcpCmd
  let credsJSON = Aeson.eitherDecode stdOut
                    & first Text.pack
      token =  runJSONPath gcpTokenKey =<< credsJSON
      expText = runJSONPath gcpExpiryKey =<< credsJSON
      expiry :: Either Text (Maybe UTCTime)
      expiry = Just <$> (parseExpiryTime =<< expText)
  atomically $ writeTVar gcpAccessToken (rightToMaybe token)
  atomically $ writeTVar gcpTokenExpiry (either (const Nothing) id expiry)
  return token

parseGCPAuthInfo :: Map Text Text -> IO (Either Text GCPAuth)
parseGCPAuthInfo m = do
  gcpAccessToken <- atomically $ newTVar $ Map.lookup "access-token" m
  case maybe (pure Nothing) ((Just <$>) . parseExpiryTime) $ Map.lookup "expiry" m of
    (Left e) -> return $ Left e
    Right t -> do
      gcpTokenExpiry <- atomically $ newTVar t
      return $ do
        cmdPath <- Text.unpack <$> lookupEither m "cmd-path"
        cmdArgs <- Text.splitOn " " <$> lookupEither m "cmd-args"
        let gcpCmd = proc cmdPath (map Text.unpack cmdArgs)
            gcpTokenKey = readJSONPath m "token-key" [JSONPath [KeyChild "token_expiry"]]
            gcpExpiryKey = readJSONPath m "expiry-key" [JSONPath [KeyChild "access_token"]]
        pure $ GCPAuth{..}

lookupEither :: (Show key, Ord key) => Map key val -> key -> Either Text val
lookupEither m k = maybeToRight e $ Map.lookup k m
                   where e = "Couldn't find key: " <> (Text.pack $ show k) <> " in GCP auth info"

parseExpiryTime :: Text -> Either Text UTCTime
parseExpiryTime s = zonedTimeToUTC <$> parseTimeRFC3339 s
                    & maybeToRight ("failed to parse token expiry time " <> s)
