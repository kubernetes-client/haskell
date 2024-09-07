{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Kubernetes.Client.KubeConfigSpec where

import           Data.Aeson                   (decode, encode, parseJSON,
                                               toJSON)
import           Data.Either                  (fromRight)
import           Data.Yaml                    (decodeFileEither)
import           Kubernetes.Client.KubeConfig (AuthInfo (..), Cluster (..),
                                               Config, Context (..),
                                               getAuthInfo, getCluster,
                                               getContext)
import           Test.Hspec

spec :: Spec
spec = do
  let getConfig :: IO Config
      getConfig = fromRight (error "Couldn't decode config") <$> decodeFileEither "test/testdata/kubeconfig.yaml"

  describe "FromJSON and ToJSON instances" $ do
    it "roundtrips successfully" $ do
      config <- getConfig
      decode (encode (toJSON config)) `shouldBe` Just config

  describe "getContext" $ do
    it "returns the correct context" $ do
      config <- getConfig
      getContext config `shouldBe` (Right (Context "cluster-aaa" "user-aaa" Nothing))

  describe "getCluster" $ do
    it "returns the correct cluster" $ do
      config <- getConfig
      server <$> getCluster config `shouldBe` (Right "https://aaa.example.com")

  describe "getAuthInfo" $ do
    it "returns the correct authInfo" $ do
      config <- getConfig
      fst <$> getAuthInfo config `shouldBe` (Right "user-aaa")
