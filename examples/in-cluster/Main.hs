{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.STM
import Control.Exception.Safe
import Kubernetes.Client
import Kubernetes.OpenAPI
import Kubernetes.OpenAPI.API.CoreV1
import Network.HTTP.Client
import Network.HTTP.Types.Status

import qualified Data.Map     as Map
import qualified Data.Text    as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  oidcCache <- newTVarIO $ Map.fromList []
  (manager, cfg) <- mkKubeClientConfig oidcCache KubeConfigCluster
  let createNamespaceRequest =
        createNamespace (ContentType MimeJSON) (Accept MimeJSON) testNamespace
  createdNS <- assertMimeSuccess =<< dispatchMime manager cfg createNamespaceRequest
  nsName <- assertJust "Expected K8s to generate name for namespace, but it didn't"
            $ (v1ObjectMetaName =<< v1NamespaceMetadata createdNS)
  T.putStrLn $ "Created Namespace: "  <> nsName

  -- NOTE: We cannot use dispatchMime due to this issue: https://github.com/kubernetes/kubernetes/issues/59501
  let deleteNamespaceRequest =
        deleteNamespace (ContentType MimeJSON) (Accept MimeJSON) (Name nsName)
  deleteNamespaceResponse <- dispatchLbs manager cfg deleteNamespaceRequest
  if responseStatus deleteNamespaceResponse /= status200
    then throwM $ AssertionFailure
    $ "Failed to cleanup namespace: " <> T.unpack nsName
    <> "\nStatus Code: " <> show (responseStatus deleteNamespaceResponse)
    <> "\nBody: " <> show (responseBody deleteNamespaceResponse)
    else return ()
  putStrLn "Cleanup complete!"

testDeployment :: V1Deployment
testDeployment =
  let labelSelector =
        mkV1LabelSelector
        { v1LabelSelectorMatchLabels =
            Just $ Map.fromList [("app", "test")] }
      container =
        (mkV1Container "container-name")
        { v1ContainerImage = Just $ "nginx" }
      podTemplate =
        mkV1PodTemplateSpec
        { v1PodTemplateSpecMetadata =
            Just $ mkV1ObjectMeta
            { v1ObjectMetaLabels = Just $ Map.fromList [("app", "test")] }
        , v1PodTemplateSpecSpec =
            Just $
            mkV1PodSpec [container]
        }
  in mkV1Deployment
  { v1DeploymentMetadata =
      Just $ mkV1ObjectMeta { v1ObjectMetaName = Just "test-deployment" }
  , v1DeploymentSpec =
      Just
      $ (mkV1DeploymentSpec labelSelector podTemplate)
  }

testNamespace :: V1Namespace
testNamespace =
  let nsMetadata =
        mkV1ObjectMeta
        { v1ObjectMetaGenerateName = Just "haskell-client-test-" }
  in mkV1Namespace
        { v1NamespaceMetadata = Just nsMetadata }

assertMimeSuccess :: MonadThrow m => MimeResult a -> m a
assertMimeSuccess (MimeResult (Right res) _) = pure res
assertMimeSuccess (MimeResult (Left err) _) =
  throwM $ AssertionFailure $ "Unexpected MimeError: " ++ show err

assertJust :: MonadThrow m => String -> Maybe a -> m a
assertJust err Nothing = throwM $ AssertionFailure err
assertJust _ (Just x)  = return x

data AssertionFailure = AssertionFailure String
  deriving Show

instance Exception AssertionFailure
