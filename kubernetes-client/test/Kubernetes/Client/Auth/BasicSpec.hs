{-# LANGUAGE OverloadedStrings #-}
module Kubernetes.Client.Auth.BasicSpec where

import           Test.Hspec
import           Data.Typeable
import           Data.Maybe                     ( isJust
                                                , isNothing
                                                , fromJust
                                                )
import           Kubernetes.Client.Auth.Basic
import           Kubernetes.Client.KubeConfig
import           Kubernetes.OpenAPI
import           Network.TLS                    ( defaultParamsClient )

emptyAuthInfo :: AuthInfo
emptyAuthInfo = AuthInfo Nothing
                         Nothing
                         Nothing
                         Nothing
                         Nothing
                         Nothing
                         Nothing
                         Nothing
                         Nothing
                         Nothing
                         Nothing
                         Nothing

spec :: Spec
spec = do
  let testTLSParams  = defaultParamsClient "" ""
      testUsername   = Just "testuser"
      testPassword   = Just "testpassword"
      basicAuthInfo  = emptyAuthInfo { username = testUsername, password = testPassword}
  describe "Basic Authentication" $ do
    it "should return Nothing if the username an d/or password is not provided" $ do
      testConfig <- newConfig
      isNothing (basicAuth emptyAuthInfo (testTLSParams, testConfig))
        `shouldBe` True
      isNothing (basicAuth emptyAuthInfo { username = testUsername} (testTLSParams, testConfig))
        `shouldBe` True
      isNothing (basicAuth emptyAuthInfo { password = testUsername} (testTLSParams, testConfig))
        `shouldBe` True

    context "when username and password are provided" $ do
      it "should return a configuration provider io" $ do
        testConfig <- newConfig
        isJust (basicAuth basicAuthInfo (testTLSParams, testConfig)) `shouldBe` True
      
      it "should configure basic auth" $ do
        testConfig <- newConfig
        (_, (KubernetesClientConfig { configAuthMethods = AnyAuthMethod (a) : as })) <- 
          fromJust $ basicAuth basicAuthInfo (testTLSParams, testConfig)
        null as `shouldBe` True
        isJust (cast a :: Maybe BasicAuth) `shouldBe` True
