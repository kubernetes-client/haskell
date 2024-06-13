{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE CPP                   #-}

{-|
Module      : Kubernetes.KubeConfig
Description : Data model for the kubeconfig.

This module contains the definition of the data model of the kubeconfig.

The official definition of the kubeconfig is defined in https://github.com/kubernetes/client-go/blob/master/tools/clientcmd/api/v1/types.go.

This is a mostly straightforward translation into Haskell, with 'FromJSON' and 'ToJSON' instances defined.
-}
module Kubernetes.Client.KubeConfig where

import           Data.Aeson     (FromJSON (..), Options, ToJSON (..),
                                 Value (..), camelTo2, defaultOptions,
                                 fieldLabelModifier, genericParseJSON,
                                 genericToJSON, object, omitNothingFields,
                                 withObject, (.:), (.=))
import qualified Data.Map       as Map
import           Data.Proxy
import           Data.Semigroup ((<>))
import           Data.Text      (Text)
import qualified Data.Text      as T
import           Data.Typeable
import           GHC.Generics
import           GHC.TypeLits

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as A
#endif

camelToWithOverrides :: Char -> Map.Map String String -> Options
camelToWithOverrides c overrides = defaultOptions
    { fieldLabelModifier = modifier
    , omitNothingFields  = True
    }
    where modifier s = Map.findWithDefault (camelTo2 c s) s overrides

-- |Represents a kubeconfig.
data Config = Config
  { kind           :: Maybe Text
  , apiVersion     :: Maybe Text
  , preferences    :: Maybe Preferences
  , clusters       :: [NamedEntity Cluster "cluster"]
  , authInfos      :: [NamedEntity AuthInfo "user"]
  , contexts       :: [NamedEntity Context "context"]
  , currentContext :: Text
  } deriving (Eq, Generic, Show)

configJSONOptions :: Options
configJSONOptions = camelToWithOverrides
    '-'
    (Map.fromList [("apiVersion", "apiVersion"), ("authInfos", "users")])

instance ToJSON Config where
  toJSON = genericToJSON configJSONOptions

instance FromJSON Config where
  parseJSON = genericParseJSON configJSONOptions

newtype Preferences = Preferences
  { colors :: Maybe Bool
  } deriving (Eq, Generic, Show)

instance ToJSON Preferences where
  toJSON = genericToJSON $ camelToWithOverrides '-' Map.empty

instance FromJSON Preferences where
  parseJSON = genericParseJSON $ camelToWithOverrides '-' Map.empty

data Cluster = Cluster
  { server                   :: Text
  , insecureSkipTLSVerify    :: Maybe Bool
  , certificateAuthority     :: Maybe Text
  , certificateAuthorityData :: Maybe Text
  } deriving (Eq, Generic, Show, Typeable)

instance ToJSON Cluster where
  toJSON = genericToJSON $ camelToWithOverrides '-' Map.empty

instance FromJSON Cluster where
  parseJSON = genericParseJSON $ camelToWithOverrides '-' Map.empty

data NamedEntity a (typeKey :: Symbol) = NamedEntity
  { name   :: Text
  , entity :: a } deriving (Eq, Generic, Show)

instance (FromJSON a, Typeable a, KnownSymbol s) =>
         FromJSON (NamedEntity a s) where
  parseJSON = withObject ("Named" <> (show $ typeOf (undefined :: a))) $ \v ->
#if MIN_VERSION_aeson(2,0,0)
    NamedEntity <$> v .: "name" <*> v .: A.fromString (symbolVal (Proxy :: Proxy s))
#else
    NamedEntity <$> v .: "name" <*> v .: T.pack (symbolVal (Proxy :: Proxy s))
#endif

instance (ToJSON a, KnownSymbol s) =>
         ToJSON (NamedEntity a s) where
  toJSON (NamedEntity {..}) = object
#if MIN_VERSION_aeson(2,0,0)
      ["name" .= toJSON name, A.fromString (symbolVal (Proxy :: Proxy s)) .= toJSON entity]
#else
      ["name" .= toJSON name, T.pack (symbolVal (Proxy :: Proxy s)) .= toJSON entity]
#endif

toMap :: [NamedEntity a s] -> Map.Map Text a
toMap = Map.fromList . fmap (\NamedEntity {..} -> (name, entity))

data AuthInfo = AuthInfo
  { clientCertificate     :: Maybe FilePath
  , clientCertificateData :: Maybe Text
  , clientKey             :: Maybe FilePath
  , clientKeyData         :: Maybe Text
  , token                 :: Maybe Text
  , tokenFile             :: Maybe FilePath
  , impersonate           :: Maybe Text
  , impersonateGroups     :: Maybe [Text]
  , impersonateUserExtra  :: Maybe (Map.Map Text [Text])
  , username              :: Maybe Text
  , password              :: Maybe Text
  , authProvider          :: Maybe AuthProviderConfig
  } deriving (Eq, Generic, Show, Typeable)

authInfoJSONOptions :: Options
authInfoJSONOptions = camelToWithOverrides
    '-'
    ( Map.fromList
        [ ("tokenFile"           , "tokenFile")
        , ("impersonate"         , "as")
        , ("impersonateGroups"   , "as-groups")
        , ("impersonateUserExtra", "as-user-extra")
        ]
    )

instance ToJSON AuthInfo where
  toJSON = genericToJSON authInfoJSONOptions

instance FromJSON AuthInfo where
  parseJSON = genericParseJSON authInfoJSONOptions

data Context = Context
  { cluster   :: Text
  , authInfo  :: Text
  , namespace :: Maybe Text
  } deriving (Eq, Generic, Show, Typeable)

contextJSONOptions :: Options
contextJSONOptions =
    camelToWithOverrides '-' (Map.fromList [("authInfo", "user")])

instance ToJSON Context where
  toJSON = genericToJSON contextJSONOptions

instance FromJSON Context where
  parseJSON = genericParseJSON contextJSONOptions

data AuthProviderConfig = AuthProviderConfig
  { name   :: Text
  , config :: Maybe (Map.Map Text Text)
  } deriving (Eq, Generic, Show)

instance ToJSON AuthProviderConfig where
  toJSON = genericToJSON $ camelToWithOverrides '-' Map.empty

instance FromJSON AuthProviderConfig where
  parseJSON = genericParseJSON $ camelToWithOverrides '-' Map.empty

-- |Returns the currently active context.
getContext :: Config -> Either String Context
getContext Config {..} =
    let maybeContext = Map.lookup currentContext (toMap contexts)
    in  case maybeContext of
            Just ctx -> Right ctx
            Nothing  -> Left ("No context named " <> T.unpack currentContext)

-- |Returns the currently active user.
getAuthInfo :: Config -> Either String (Text, AuthInfo)
getAuthInfo cfg@Config {..} = do
    Context {..} <- getContext cfg
    let maybeAuth = Map.lookup authInfo (toMap authInfos)
    case maybeAuth of
        Just auth -> Right (authInfo, auth)
        Nothing   -> Left ("No user named " <> T.unpack authInfo)

-- |Returns the currently active cluster.
getCluster :: Config -> Either String Cluster
getCluster cfg@Config {clusters=clusters} = do
    Context {cluster=clusterName} <- getContext cfg
    let maybeCluster = Map.lookup clusterName (toMap clusters)
    case maybeCluster of
        Just cluster -> Right cluster
        Nothing      -> Left ("No cluster named " <> T.unpack clusterName)
