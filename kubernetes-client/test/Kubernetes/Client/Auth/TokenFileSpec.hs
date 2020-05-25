{-# LANGUAGE OverloadedStrings #-}
module Kubernetes.Client.Auth.TokenFileSpec where

import           Test.Hspec
import           Control.Concurrent
import           Data.Function                  ( (&) )
import           Data.FileEmbed
import           Data.Typeable
import           Data.Maybe                     ( isJust
                                                , isNothing
                                                )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( decodeUtf8 )
import           Kubernetes.Client.Auth.TokenFile
import           Kubernetes.Client.KubeConfig
import           Kubernetes.OpenAPI
import           Kubernetes.OpenAPI.Core        ( _applyAuthMethods
                                                , _mkRequest
                                                )
import           Network.TLS                    ( defaultParamsClient )

import qualified Data.ByteString.Base64        as B64

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
      token1FilePath = "test/testdata/tokens/token1"
      token2FilePath = "test/testdata/tokens/token2"
  describe "TokenFile Authentication" $ do
    it "should return Nothing if the file is not provided" $ do
      testConfig <- newConfig
      isNothing (tokenFileAuth emptyAuthInfo (testTLSParams, testConfig))
        `shouldBe` True

    it "should reload token after expiry" $ do
      let auth = emptyAuthInfo { tokenFile = Just token1FilePath }
      testConfig <- newConfig
      case tokenFileAuth auth (testTLSParams, testConfig) of
        Nothing -> expectationFailure "expected to detect TokenFile auth"
        Just detectedAuth -> do
          (_, cfg@(KubernetesClientConfig { configAuthMethods = AnyAuthMethod (a) : as })) <-
            detectedAuth
          case cast a :: Maybe TokenFileAuth of
            Nothing -> expectationFailure "expected to be TokenFile auth"
            Just tf -> do
              let tfWithShorterPeriod = tf { period = 5 }
              x <- getToken tfWithShorterPeriod
              x `shouldBe` ("token1" :: Text)

              let tfWithShorterPeriod' =
                    tfWithShorterPeriod { file = token2FilePath }
              threadDelay 2000000 -- sleep 2 seconds
              x <- getToken tfWithShorterPeriod'
              x `shouldBe` ("token1" :: Text)

              threadDelay 3000000 -- sleep 3 seconds
              x <- getToken tfWithShorterPeriod'
              x `shouldBe` ("token2" :: Text)

