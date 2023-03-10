{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kubernetes.Client.Auth.Exec
  ( -- * Authentication
    Exec
  , execAuth
  , ExecAuthError (..)
    -- * Token Info
  , TokenInfo (..)
  , Expiration (..)
    -- * Cache
  , Cache
  , newCache
    -- * gke-gcloud-auth-plugin
  , decodeGkeGcloudAuthPluginToken
  ) where

import Control.Monad (when, void, forM_)
import Control.Monad.STM (atomically)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import Control.Exception (Exception, catch, throwIO)
import Data.Aeson (FromJSON (..), eitherDecodeStrict)
import Data.Maybe (fromMaybe)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text, unpack)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Data.Typeable (typeOf)
import GHC.Generics (Generic)
import Kubernetes.Client.Auth.Internal.Types (DetectAuth)
import Kubernetes.Client.KubeConfig (AuthInfo (..), ExecConfig (..), ExecEnvVar (..))
import Kubernetes.OpenAPI.Core
import qualified Data.ByteString.Char8 as ByteString
import qualified System.Process as Process

-------------------------------------------------------------------------------
-- Authentication using Auth

-- | Information about the token, as well as its expiration.
data TokenInfo = TokenInfo
  { tokenValue :: Text
  , tokenExpiration :: Expiration
  } deriving (Show)

-- | What is the expiration time of the token.
data Expiration = NeverExpires | SingleUse | ExpiresAt UTCTime
    deriving (Show)

-- | The command that is to be run to obtain the token
data Exec = Exec
  { config :: ExecConfig
  , decodeToken :: String -> Either String TokenInfo
  , refreshBefore :: Maybe Int
  , cache :: Maybe Cache
  }

runExec :: Exec -> IO Text
runExec exec =
  case cache exec of
    Nothing ->
      tokenValue <$> runExecCommand exec
    Just cache ->
      lookupCache cache (refreshBefore exec) (config exec) (runExecCommand exec)

data ExecAuthError = ExecCommandError IOError | ExecTokenParseError String
  deriving (Show, Exception)

runExecCommand :: Exec -> IO TokenInfo
runExecCommand Exec { config = ExecConfig {..}, decodeToken } = do
  let
    environment =
      fmap (fmap (\ExecEnvVar {..} -> (unpack name, unpack value))) env

    process =
      (Process.proc (unpack command) (fmap unpack $ fromMaybe [] args))
        { Process.env = environment }

  output <-
    catch (Process.readCreateProcess process "") $ \err ->
      throwIO $ ExecCommandError err

  case decodeToken output of
    Left err ->
      throwIO $ ExecTokenParseError err
    Right ok ->
      pure ok

instance AuthMethod Exec where
  applyAuthMethod _ plugin req = do
    token <- runExec plugin
    pure $
      if (typeOf plugin `elem` rAuthTypes req)
        then
          (req `setHeader` toHeader ("Authorization", "Bearer " <> token))
          { rAuthTypes = [] }
        else
          req

-- | Defines the 'exec' authentication method.
execAuth ::
  -- | Whether to use a cache or not. The cache can be created with @newCache@.
  Maybe Cache ->
  -- | Amount of minutes before the expiration time where token refreshing
  -- should be retried in the background.
  Maybe Int ->
  -- | Function used to decode the output of the executed plugin.
  (String -> Either String TokenInfo) -> DetectAuth
execAuth mcache refreshBefore decodeToken AuthInfo {exec} (tlsParams, cfg) = do
  config <- exec
  pure $
    pure
      ( tlsParams
      , cfg
          { configAuthMethods = [AnyAuthMethod (Exec config decodeToken refreshBefore mcache)]
          }
      )

--------------------------------------------------------------------------------
-- Cache

-- The cache for token info. It is a TVar so it can be reused by multiple
-- threads, and each token is in a MVar so there are no multiple threads trying
-- to update the same key.
newtype Cache = Cache (TVar (HashMap ExecConfig (MVar TokenInfo)))

newCache :: IO Cache
newCache = Cache <$> TVar.newTVarIO HashMap.empty

lookupCache :: Cache -> Maybe Int -> ExecConfig -> IO TokenInfo -> IO Text
lookupCache (Cache tvar) refreshBefore config getTokenInfo = do

  let renewTVar possibleMVar isBlocking = do
        mvar <- case possibleMVar of
          -- If there is no MVar in scope, it needs to be created and added to
          -- the TVar.
          Nothing -> do
            mvar <- MVar.newEmptyMVar
            atomically $ TVar.modifyTVar tvar (HashMap.insert config mvar)
            pure mvar

          -- Otherwise, we can just reuse it.
          Just mvar -> do
            when isBlocking $ do
              void $ MVar.takeMVar mvar
            pure mvar

        -- Get the TokenInfo
        tokenInfo <- getTokenInfo

        -- Fill the mvar with the existing info
        if isBlocking
          then
            -- Here the MVar is empty
            MVar.putMVar mvar tokenInfo
          else
            -- Here the MVar is filled
            void $ MVar.swapMVar mvar tokenInfo

        -- Return the token
        pure (tokenValue tokenInfo)

  hashmap <- TVar.readTVarIO tvar

  case HashMap.lookup config hashmap of
    Nothing ->
      -- In this case, the MVar will be created for the first time, otherwise
      -- the value it holds will be updated
      renewTVar Nothing True
    Just mvar -> do
      let renew = renewTVar (Just mvar)
      TokenInfo value mexpiration <- MVar.readMVar mvar
      case mexpiration of
        NeverExpires ->
          -- We can just reuse the value
          pure value
        SingleUse ->
          -- No need to use the cache, just retrieve the token
          tokenValue <$> getTokenInfo
        ExpiresAt expiration -> do
          now <- getCurrentTime
          if now > expiration
          then
            -- Here the key is expired, so the renewal should be blocking
            renew True
          else do
            forM_ refreshBefore $ \minutes ->
              when (addUTCTime (realToFrac minutes * 60) now > expiration) $
                -- Here the key is not expired, but we are close enough to
                -- expiration to ask for a new one in the background, so the
                -- renewal should not be blocking
                void $ forkIO $ void $ renew False
            pure value

-------------------------------------------------------------------------------
-- gke-gcloud-auth-plugin

-- | The result that is written to stdout
data Result = Result
  { kind :: Text
  , apiVersion :: Text
  , status :: Status
  } deriving (Show, Generic)
    deriving (FromJSON)

data Status = Status
  { expirationTimestamp :: UTCTime
  , token :: Text
  } deriving (Show, Generic)
    deriving (FromJSON)

decodeGkeGcloudAuthPluginToken :: String -> Either String TokenInfo
decodeGkeGcloudAuthPluginToken str = do
  Result { status = Status { token, expirationTimestamp } } <-
    eitherDecodeStrict (ByteString.pack str)
  pure $ TokenInfo token (ExpiresAt expirationTimestamp)
