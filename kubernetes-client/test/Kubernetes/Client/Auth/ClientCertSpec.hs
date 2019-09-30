{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Kubernetes.Client.Auth.ClientCertSpec where

import Test.Hspec

import Data.FileEmbed
import Data.Maybe                        (isJust, isNothing)
import Data.Text.Encoding                (decodeUtf8)
import Kubernetes.Client.Auth.ClientCert
import Kubernetes.Client.KubeConfig
import Kubernetes.OpenAPI
import Network.TLS                       (defaultParamsClient)

import qualified Data.ByteString.Base64 as B64

emptyAuthInfo :: AuthInfo
emptyAuthInfo = AuthInfo Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

spec :: Spec
spec = do
  let inputTLSParams = defaultParamsClient "" ""
      certFilePath = "test/testdata/certs/certificate.pem"
      keyFilePath =  "test/testdata/certs/private-key.pem"
      base64EncodedCert = decodeUtf8 $ B64.encode $(embedFile $ "test/testdata/certs/certificate.pem")
      base64EncodedKey = decodeUtf8 $ B64.encode $(embedFile $ "test/testdata/certs/private-key.pem")
  describe "ClientCert File Authentication" $ do
    context "when cert and key file are provided in AuthInfo" $ do
      let auth = emptyAuthInfo { clientCertificate = Just $ certFilePath
                               , clientKey = Just $ keyFilePath }
      it "should detect client cert file auth" $ do
        inputConfig <- newConfig
        isJust (clientCertFileAuth auth (inputTLSParams, inputConfig)) `shouldBe` True

      it "should disable validate auth method" $ do
        inputConfig <- newConfig
        case clientCertFileAuth auth (inputTLSParams, inputConfig) of
          Nothing -> expectationFailure "expected to detect client cert file auth"
          Just detectedAuth -> do
            (_, cfg) <- detectedAuth
            configValidateAuthMethods cfg `shouldBe` False

    it "should return Nothing if the cert file is not provided" $ do
      let auth = emptyAuthInfo {clientKey = Just "/some/file"}
      inputConfig <- newConfig
      isNothing (clientCertFileAuth auth (inputTLSParams, inputConfig)) `shouldBe` True

    it "should return Nothing if the key file is not provided" $ do
      let auth = emptyAuthInfo {clientCertificate = Just "/some/file"}
      inputConfig <- newConfig
      isNothing (clientCertFileAuth auth (undefined, inputConfig)) `shouldBe` True

  describe "ClientCert Data Authentication" $ do
    context "when cert and key file are provided in AuthInfo" $ do
      let auth = emptyAuthInfo { clientCertificateData = Just base64EncodedCert
                               , clientKeyData = Just base64EncodedKey}
      it "should detect client cert data auth" $ do
        inputConfig <- newConfig
        isJust (clientCertDataAuth auth (inputTLSParams, inputConfig)) `shouldBe` True

      it "should disable validate auth method" $ do
        inputConfig <- newConfig
        case clientCertDataAuth auth (inputTLSParams, inputConfig) of
          Nothing -> expectationFailure "expected to detect client cert file auth"
          Just detectedAuth -> do
            (_, cfg) <- detectedAuth
            configValidateAuthMethods cfg `shouldBe` False

    it "should return Nothing if the cert file is not provided" $ do
      let auth = emptyAuthInfo {clientKeyData = Just base64EncodedKey}
      inputConfig <- newConfig
      isNothing (clientCertDataAuth auth (inputTLSParams, inputConfig)) `shouldBe` True

    it "should return Nothing if the key file is not provided" $ do
      let auth = emptyAuthInfo {clientCertificateData = Just base64EncodedCert}
      inputConfig <- newConfig
      isNothing (clientCertDataAuth auth (inputTLSParams, inputConfig)) `shouldBe` True
