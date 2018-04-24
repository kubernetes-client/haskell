{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Aeson            (decode, encode, parseJSON, toJSON)
import           Data.Maybe            (fromJust)
import           Data.Yaml             (decodeFile)
import           Kubernetes.KubeConfig (AuthInfo (..), Cluster (..), Config,
                                        Context (..), getAuthInfo, getCluster,
                                        getContext)
import           Test.Hspec

main :: IO ()
main = do
  config :: Config <- fromJust <$> decodeFile "test/testdata/kubeconfig.yaml"
  hspec $ do
    describe "FromJSON and ToJSON instances" $ do
      it "roundtrips successfully" $ do
        decode (encode (toJSON config)) `shouldBe` Just config
    describe "getContext" $ do
      it "returns the correct context" $ do
        getContext config `shouldBe` (Right (Context "cluster-aaa" "user-aaa" Nothing))

    describe "getCluster" $ do
      it "returns the correct cluster" $ do
        server <$> getCluster config `shouldBe` (Right "https://aaa.example.com")

    describe "getAuthInfo" $ do
      it "returns the correct authInfo" $ do
        fst <$> getAuthInfo config `shouldBe` (Right "user-aaa")
