{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import Kubernetes.OpenAPI.Model
import Kubernetes.OpenAPI.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (T.pack k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays
    
-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)
    
arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models
 
instance Arbitrary AdmissionregistrationV1beta1ServiceReference where
  arbitrary = sized genAdmissionregistrationV1beta1ServiceReference

genAdmissionregistrationV1beta1ServiceReference :: Int -> Gen AdmissionregistrationV1beta1ServiceReference
genAdmissionregistrationV1beta1ServiceReference n =
  AdmissionregistrationV1beta1ServiceReference
    <$> arbitrary -- admissionregistrationV1beta1ServiceReferenceName :: Text
    <*> arbitrary -- admissionregistrationV1beta1ServiceReferenceNamespace :: Text
    <*> arbitraryReducedMaybe n -- admissionregistrationV1beta1ServiceReferencePath :: Maybe Text
  
instance Arbitrary ApiregistrationV1beta1ServiceReference where
  arbitrary = sized genApiregistrationV1beta1ServiceReference

genApiregistrationV1beta1ServiceReference :: Int -> Gen ApiregistrationV1beta1ServiceReference
genApiregistrationV1beta1ServiceReference n =
  ApiregistrationV1beta1ServiceReference
    <$> arbitraryReducedMaybe n -- apiregistrationV1beta1ServiceReferenceName :: Maybe Text
    <*> arbitraryReducedMaybe n -- apiregistrationV1beta1ServiceReferenceNamespace :: Maybe Text
  
instance Arbitrary AppsV1beta1Deployment where
  arbitrary = sized genAppsV1beta1Deployment

genAppsV1beta1Deployment :: Int -> Gen AppsV1beta1Deployment
genAppsV1beta1Deployment n =
  AppsV1beta1Deployment
    <$> arbitraryReducedMaybe n -- appsV1beta1DeploymentApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentSpec :: Maybe AppsV1beta1DeploymentSpec
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentStatus :: Maybe AppsV1beta1DeploymentStatus
  
instance Arbitrary AppsV1beta1DeploymentCondition where
  arbitrary = sized genAppsV1beta1DeploymentCondition

genAppsV1beta1DeploymentCondition :: Int -> Gen AppsV1beta1DeploymentCondition
genAppsV1beta1DeploymentCondition n =
  AppsV1beta1DeploymentCondition
    <$> arbitraryReducedMaybe n -- appsV1beta1DeploymentConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentConditionLastUpdateTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentConditionReason :: Maybe Text
    <*> arbitrary -- appsV1beta1DeploymentConditionStatus :: Text
    <*> arbitrary -- appsV1beta1DeploymentConditionType :: Text
  
instance Arbitrary AppsV1beta1DeploymentList where
  arbitrary = sized genAppsV1beta1DeploymentList

genAppsV1beta1DeploymentList :: Int -> Gen AppsV1beta1DeploymentList
genAppsV1beta1DeploymentList n =
  AppsV1beta1DeploymentList
    <$> arbitraryReducedMaybe n -- appsV1beta1DeploymentListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- appsV1beta1DeploymentListItems :: [AppsV1beta1Deployment]
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentListMetadata :: Maybe V1ListMeta
  
instance Arbitrary AppsV1beta1DeploymentRollback where
  arbitrary = sized genAppsV1beta1DeploymentRollback

genAppsV1beta1DeploymentRollback :: Int -> Gen AppsV1beta1DeploymentRollback
genAppsV1beta1DeploymentRollback n =
  AppsV1beta1DeploymentRollback
    <$> arbitraryReducedMaybe n -- appsV1beta1DeploymentRollbackApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentRollbackKind :: Maybe Text
    <*> arbitrary -- appsV1beta1DeploymentRollbackName :: Text
    <*> arbitraryReduced n -- appsV1beta1DeploymentRollbackRollbackTo :: AppsV1beta1RollbackConfig
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentRollbackUpdatedAnnotations :: Maybe (Map.Map String Text)
  
instance Arbitrary AppsV1beta1DeploymentSpec where
  arbitrary = sized genAppsV1beta1DeploymentSpec

genAppsV1beta1DeploymentSpec :: Int -> Gen AppsV1beta1DeploymentSpec
genAppsV1beta1DeploymentSpec n =
  AppsV1beta1DeploymentSpec
    <$> arbitraryReducedMaybe n -- appsV1beta1DeploymentSpecMinReadySeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentSpecPaused :: Maybe Bool
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentSpecProgressDeadlineSeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentSpecReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentSpecRevisionHistoryLimit :: Maybe Int
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentSpecRollbackTo :: Maybe AppsV1beta1RollbackConfig
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentSpecSelector :: Maybe V1LabelSelector
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentSpecStrategy :: Maybe AppsV1beta1DeploymentStrategy
    <*> arbitraryReduced n -- appsV1beta1DeploymentSpecTemplate :: V1PodTemplateSpec
  
instance Arbitrary AppsV1beta1DeploymentStatus where
  arbitrary = sized genAppsV1beta1DeploymentStatus

genAppsV1beta1DeploymentStatus :: Int -> Gen AppsV1beta1DeploymentStatus
genAppsV1beta1DeploymentStatus n =
  AppsV1beta1DeploymentStatus
    <$> arbitraryReducedMaybe n -- appsV1beta1DeploymentStatusAvailableReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentStatusCollisionCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentStatusConditions :: Maybe [AppsV1beta1DeploymentCondition]
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentStatusObservedGeneration :: Maybe Integer
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentStatusReadyReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentStatusReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentStatusUnavailableReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentStatusUpdatedReplicas :: Maybe Int
  
instance Arbitrary AppsV1beta1DeploymentStrategy where
  arbitrary = sized genAppsV1beta1DeploymentStrategy

genAppsV1beta1DeploymentStrategy :: Int -> Gen AppsV1beta1DeploymentStrategy
genAppsV1beta1DeploymentStrategy n =
  AppsV1beta1DeploymentStrategy
    <$> arbitraryReducedMaybe n -- appsV1beta1DeploymentStrategyRollingUpdate :: Maybe AppsV1beta1RollingUpdateDeployment
    <*> arbitraryReducedMaybe n -- appsV1beta1DeploymentStrategyType :: Maybe Text
  
instance Arbitrary AppsV1beta1RollbackConfig where
  arbitrary = sized genAppsV1beta1RollbackConfig

genAppsV1beta1RollbackConfig :: Int -> Gen AppsV1beta1RollbackConfig
genAppsV1beta1RollbackConfig n =
  AppsV1beta1RollbackConfig
    <$> arbitraryReducedMaybe n -- appsV1beta1RollbackConfigRevision :: Maybe Integer
  
instance Arbitrary AppsV1beta1RollingUpdateDeployment where
  arbitrary = sized genAppsV1beta1RollingUpdateDeployment

genAppsV1beta1RollingUpdateDeployment :: Int -> Gen AppsV1beta1RollingUpdateDeployment
genAppsV1beta1RollingUpdateDeployment n =
  AppsV1beta1RollingUpdateDeployment
    <$> arbitraryReducedMaybeValue n -- appsV1beta1RollingUpdateDeploymentMaxSurge :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- appsV1beta1RollingUpdateDeploymentMaxUnavailable :: Maybe A.Value
  
instance Arbitrary AppsV1beta1Scale where
  arbitrary = sized genAppsV1beta1Scale

genAppsV1beta1Scale :: Int -> Gen AppsV1beta1Scale
genAppsV1beta1Scale n =
  AppsV1beta1Scale
    <$> arbitraryReducedMaybe n -- appsV1beta1ScaleApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- appsV1beta1ScaleKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- appsV1beta1ScaleMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- appsV1beta1ScaleSpec :: Maybe AppsV1beta1ScaleSpec
    <*> arbitraryReducedMaybe n -- appsV1beta1ScaleStatus :: Maybe AppsV1beta1ScaleStatus
  
instance Arbitrary AppsV1beta1ScaleSpec where
  arbitrary = sized genAppsV1beta1ScaleSpec

genAppsV1beta1ScaleSpec :: Int -> Gen AppsV1beta1ScaleSpec
genAppsV1beta1ScaleSpec n =
  AppsV1beta1ScaleSpec
    <$> arbitraryReducedMaybe n -- appsV1beta1ScaleSpecReplicas :: Maybe Int
  
instance Arbitrary AppsV1beta1ScaleStatus where
  arbitrary = sized genAppsV1beta1ScaleStatus

genAppsV1beta1ScaleStatus :: Int -> Gen AppsV1beta1ScaleStatus
genAppsV1beta1ScaleStatus n =
  AppsV1beta1ScaleStatus
    <$> arbitrary -- appsV1beta1ScaleStatusReplicas :: Int
    <*> arbitraryReducedMaybe n -- appsV1beta1ScaleStatusSelector :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- appsV1beta1ScaleStatusTargetSelector :: Maybe Text
  
instance Arbitrary ExtensionsV1beta1Deployment where
  arbitrary = sized genExtensionsV1beta1Deployment

genExtensionsV1beta1Deployment :: Int -> Gen ExtensionsV1beta1Deployment
genExtensionsV1beta1Deployment n =
  ExtensionsV1beta1Deployment
    <$> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentSpec :: Maybe ExtensionsV1beta1DeploymentSpec
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentStatus :: Maybe ExtensionsV1beta1DeploymentStatus
  
instance Arbitrary ExtensionsV1beta1DeploymentCondition where
  arbitrary = sized genExtensionsV1beta1DeploymentCondition

genExtensionsV1beta1DeploymentCondition :: Int -> Gen ExtensionsV1beta1DeploymentCondition
genExtensionsV1beta1DeploymentCondition n =
  ExtensionsV1beta1DeploymentCondition
    <$> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentConditionLastUpdateTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentConditionReason :: Maybe Text
    <*> arbitrary -- extensionsV1beta1DeploymentConditionStatus :: Text
    <*> arbitrary -- extensionsV1beta1DeploymentConditionType :: Text
  
instance Arbitrary ExtensionsV1beta1DeploymentList where
  arbitrary = sized genExtensionsV1beta1DeploymentList

genExtensionsV1beta1DeploymentList :: Int -> Gen ExtensionsV1beta1DeploymentList
genExtensionsV1beta1DeploymentList n =
  ExtensionsV1beta1DeploymentList
    <$> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- extensionsV1beta1DeploymentListItems :: [ExtensionsV1beta1Deployment]
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentListMetadata :: Maybe V1ListMeta
  
instance Arbitrary ExtensionsV1beta1DeploymentRollback where
  arbitrary = sized genExtensionsV1beta1DeploymentRollback

genExtensionsV1beta1DeploymentRollback :: Int -> Gen ExtensionsV1beta1DeploymentRollback
genExtensionsV1beta1DeploymentRollback n =
  ExtensionsV1beta1DeploymentRollback
    <$> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentRollbackApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentRollbackKind :: Maybe Text
    <*> arbitrary -- extensionsV1beta1DeploymentRollbackName :: Text
    <*> arbitraryReduced n -- extensionsV1beta1DeploymentRollbackRollbackTo :: ExtensionsV1beta1RollbackConfig
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentRollbackUpdatedAnnotations :: Maybe (Map.Map String Text)
  
instance Arbitrary ExtensionsV1beta1DeploymentSpec where
  arbitrary = sized genExtensionsV1beta1DeploymentSpec

genExtensionsV1beta1DeploymentSpec :: Int -> Gen ExtensionsV1beta1DeploymentSpec
genExtensionsV1beta1DeploymentSpec n =
  ExtensionsV1beta1DeploymentSpec
    <$> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentSpecMinReadySeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentSpecPaused :: Maybe Bool
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentSpecProgressDeadlineSeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentSpecReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentSpecRevisionHistoryLimit :: Maybe Int
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentSpecRollbackTo :: Maybe ExtensionsV1beta1RollbackConfig
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentSpecSelector :: Maybe V1LabelSelector
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentSpecStrategy :: Maybe ExtensionsV1beta1DeploymentStrategy
    <*> arbitraryReduced n -- extensionsV1beta1DeploymentSpecTemplate :: V1PodTemplateSpec
  
instance Arbitrary ExtensionsV1beta1DeploymentStatus where
  arbitrary = sized genExtensionsV1beta1DeploymentStatus

genExtensionsV1beta1DeploymentStatus :: Int -> Gen ExtensionsV1beta1DeploymentStatus
genExtensionsV1beta1DeploymentStatus n =
  ExtensionsV1beta1DeploymentStatus
    <$> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentStatusAvailableReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentStatusCollisionCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentStatusConditions :: Maybe [ExtensionsV1beta1DeploymentCondition]
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentStatusObservedGeneration :: Maybe Integer
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentStatusReadyReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentStatusReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentStatusUnavailableReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentStatusUpdatedReplicas :: Maybe Int
  
instance Arbitrary ExtensionsV1beta1DeploymentStrategy where
  arbitrary = sized genExtensionsV1beta1DeploymentStrategy

genExtensionsV1beta1DeploymentStrategy :: Int -> Gen ExtensionsV1beta1DeploymentStrategy
genExtensionsV1beta1DeploymentStrategy n =
  ExtensionsV1beta1DeploymentStrategy
    <$> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentStrategyRollingUpdate :: Maybe ExtensionsV1beta1RollingUpdateDeployment
    <*> arbitraryReducedMaybe n -- extensionsV1beta1DeploymentStrategyType :: Maybe Text
  
instance Arbitrary ExtensionsV1beta1RollbackConfig where
  arbitrary = sized genExtensionsV1beta1RollbackConfig

genExtensionsV1beta1RollbackConfig :: Int -> Gen ExtensionsV1beta1RollbackConfig
genExtensionsV1beta1RollbackConfig n =
  ExtensionsV1beta1RollbackConfig
    <$> arbitraryReducedMaybe n -- extensionsV1beta1RollbackConfigRevision :: Maybe Integer
  
instance Arbitrary ExtensionsV1beta1RollingUpdateDeployment where
  arbitrary = sized genExtensionsV1beta1RollingUpdateDeployment

genExtensionsV1beta1RollingUpdateDeployment :: Int -> Gen ExtensionsV1beta1RollingUpdateDeployment
genExtensionsV1beta1RollingUpdateDeployment n =
  ExtensionsV1beta1RollingUpdateDeployment
    <$> arbitraryReducedMaybeValue n -- extensionsV1beta1RollingUpdateDeploymentMaxSurge :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- extensionsV1beta1RollingUpdateDeploymentMaxUnavailable :: Maybe A.Value
  
instance Arbitrary ExtensionsV1beta1Scale where
  arbitrary = sized genExtensionsV1beta1Scale

genExtensionsV1beta1Scale :: Int -> Gen ExtensionsV1beta1Scale
genExtensionsV1beta1Scale n =
  ExtensionsV1beta1Scale
    <$> arbitraryReducedMaybe n -- extensionsV1beta1ScaleApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- extensionsV1beta1ScaleKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- extensionsV1beta1ScaleMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- extensionsV1beta1ScaleSpec :: Maybe ExtensionsV1beta1ScaleSpec
    <*> arbitraryReducedMaybe n -- extensionsV1beta1ScaleStatus :: Maybe ExtensionsV1beta1ScaleStatus
  
instance Arbitrary ExtensionsV1beta1ScaleSpec where
  arbitrary = sized genExtensionsV1beta1ScaleSpec

genExtensionsV1beta1ScaleSpec :: Int -> Gen ExtensionsV1beta1ScaleSpec
genExtensionsV1beta1ScaleSpec n =
  ExtensionsV1beta1ScaleSpec
    <$> arbitraryReducedMaybe n -- extensionsV1beta1ScaleSpecReplicas :: Maybe Int
  
instance Arbitrary ExtensionsV1beta1ScaleStatus where
  arbitrary = sized genExtensionsV1beta1ScaleStatus

genExtensionsV1beta1ScaleStatus :: Int -> Gen ExtensionsV1beta1ScaleStatus
genExtensionsV1beta1ScaleStatus n =
  ExtensionsV1beta1ScaleStatus
    <$> arbitrary -- extensionsV1beta1ScaleStatusReplicas :: Int
    <*> arbitraryReducedMaybe n -- extensionsV1beta1ScaleStatusSelector :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- extensionsV1beta1ScaleStatusTargetSelector :: Maybe Text
  
instance Arbitrary RuntimeRawExtension where
  arbitrary = sized genRuntimeRawExtension

genRuntimeRawExtension :: Int -> Gen RuntimeRawExtension
genRuntimeRawExtension n =
  RuntimeRawExtension
    <$> arbitraryReduced n -- runtimeRawExtensionRaw :: ByteArray
  
instance Arbitrary V1APIGroup where
  arbitrary = sized genV1APIGroup

genV1APIGroup :: Int -> Gen V1APIGroup
genV1APIGroup n =
  V1APIGroup
    <$> arbitraryReducedMaybe n -- v1APIGroupApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1APIGroupKind :: Maybe Text
    <*> arbitrary -- v1APIGroupName :: Text
    <*> arbitraryReducedMaybe n -- v1APIGroupPreferredVersion :: Maybe V1GroupVersionForDiscovery
    <*> arbitraryReduced n -- v1APIGroupServerAddressByClientCidRs :: [V1ServerAddressByClientCIDR]
    <*> arbitraryReduced n -- v1APIGroupVersions :: [V1GroupVersionForDiscovery]
  
instance Arbitrary V1APIGroupList where
  arbitrary = sized genV1APIGroupList

genV1APIGroupList :: Int -> Gen V1APIGroupList
genV1APIGroupList n =
  V1APIGroupList
    <$> arbitraryReducedMaybe n -- v1APIGroupListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1APIGroupListGroups :: [V1APIGroup]
    <*> arbitraryReducedMaybe n -- v1APIGroupListKind :: Maybe Text
  
instance Arbitrary V1APIResource where
  arbitrary = sized genV1APIResource

genV1APIResource :: Int -> Gen V1APIResource
genV1APIResource n =
  V1APIResource
    <$> arbitraryReducedMaybe n -- v1APIResourceCategories :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1APIResourceGroup :: Maybe Text
    <*> arbitrary -- v1APIResourceKind :: Text
    <*> arbitrary -- v1APIResourceName :: Text
    <*> arbitrary -- v1APIResourceNamespaced :: Bool
    <*> arbitraryReducedMaybe n -- v1APIResourceShortNames :: Maybe [Text]
    <*> arbitrary -- v1APIResourceSingularName :: Text
    <*> arbitrary -- v1APIResourceVerbs :: [Text]
    <*> arbitraryReducedMaybe n -- v1APIResourceVersion :: Maybe Text
  
instance Arbitrary V1APIResourceList where
  arbitrary = sized genV1APIResourceList

genV1APIResourceList :: Int -> Gen V1APIResourceList
genV1APIResourceList n =
  V1APIResourceList
    <$> arbitraryReducedMaybe n -- v1APIResourceListApiVersion :: Maybe Text
    <*> arbitrary -- v1APIResourceListGroupVersion :: Text
    <*> arbitraryReducedMaybe n -- v1APIResourceListKind :: Maybe Text
    <*> arbitraryReduced n -- v1APIResourceListResources :: [V1APIResource]
  
instance Arbitrary V1APIVersions where
  arbitrary = sized genV1APIVersions

genV1APIVersions :: Int -> Gen V1APIVersions
genV1APIVersions n =
  V1APIVersions
    <$> arbitraryReducedMaybe n -- v1APIVersionsApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1APIVersionsKind :: Maybe Text
    <*> arbitraryReduced n -- v1APIVersionsServerAddressByClientCidRs :: [V1ServerAddressByClientCIDR]
    <*> arbitrary -- v1APIVersionsVersions :: [Text]
  
instance Arbitrary V1AWSElasticBlockStoreVolumeSource where
  arbitrary = sized genV1AWSElasticBlockStoreVolumeSource

genV1AWSElasticBlockStoreVolumeSource :: Int -> Gen V1AWSElasticBlockStoreVolumeSource
genV1AWSElasticBlockStoreVolumeSource n =
  V1AWSElasticBlockStoreVolumeSource
    <$> arbitraryReducedMaybe n -- v1AWSElasticBlockStoreVolumeSourceFsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1AWSElasticBlockStoreVolumeSourcePartition :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1AWSElasticBlockStoreVolumeSourceReadOnly :: Maybe Bool
    <*> arbitrary -- v1AWSElasticBlockStoreVolumeSourceVolumeId :: Text
  
instance Arbitrary V1Affinity where
  arbitrary = sized genV1Affinity

genV1Affinity :: Int -> Gen V1Affinity
genV1Affinity n =
  V1Affinity
    <$> arbitraryReducedMaybe n -- v1AffinityNodeAffinity :: Maybe V1NodeAffinity
    <*> arbitraryReducedMaybe n -- v1AffinityPodAffinity :: Maybe V1PodAffinity
    <*> arbitraryReducedMaybe n -- v1AffinityPodAntiAffinity :: Maybe V1PodAntiAffinity
  
instance Arbitrary V1AggregationRule where
  arbitrary = sized genV1AggregationRule

genV1AggregationRule :: Int -> Gen V1AggregationRule
genV1AggregationRule n =
  V1AggregationRule
    <$> arbitraryReducedMaybe n -- v1AggregationRuleClusterRoleSelectors :: Maybe [V1LabelSelector]
  
instance Arbitrary V1AttachedVolume where
  arbitrary = sized genV1AttachedVolume

genV1AttachedVolume :: Int -> Gen V1AttachedVolume
genV1AttachedVolume n =
  V1AttachedVolume
    <$> arbitrary -- v1AttachedVolumeDevicePath :: Text
    <*> arbitrary -- v1AttachedVolumeName :: Text
  
instance Arbitrary V1AzureDiskVolumeSource where
  arbitrary = sized genV1AzureDiskVolumeSource

genV1AzureDiskVolumeSource :: Int -> Gen V1AzureDiskVolumeSource
genV1AzureDiskVolumeSource n =
  V1AzureDiskVolumeSource
    <$> arbitraryReducedMaybe n -- v1AzureDiskVolumeSourceCachingMode :: Maybe Text
    <*> arbitrary -- v1AzureDiskVolumeSourceDiskName :: Text
    <*> arbitrary -- v1AzureDiskVolumeSourceDiskUri :: Text
    <*> arbitraryReducedMaybe n -- v1AzureDiskVolumeSourceFsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1AzureDiskVolumeSourceKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1AzureDiskVolumeSourceReadOnly :: Maybe Bool
  
instance Arbitrary V1AzureFilePersistentVolumeSource where
  arbitrary = sized genV1AzureFilePersistentVolumeSource

genV1AzureFilePersistentVolumeSource :: Int -> Gen V1AzureFilePersistentVolumeSource
genV1AzureFilePersistentVolumeSource n =
  V1AzureFilePersistentVolumeSource
    <$> arbitraryReducedMaybe n -- v1AzureFilePersistentVolumeSourceReadOnly :: Maybe Bool
    <*> arbitrary -- v1AzureFilePersistentVolumeSourceSecretName :: Text
    <*> arbitraryReducedMaybe n -- v1AzureFilePersistentVolumeSourceSecretNamespace :: Maybe Text
    <*> arbitrary -- v1AzureFilePersistentVolumeSourceShareName :: Text
  
instance Arbitrary V1AzureFileVolumeSource where
  arbitrary = sized genV1AzureFileVolumeSource

genV1AzureFileVolumeSource :: Int -> Gen V1AzureFileVolumeSource
genV1AzureFileVolumeSource n =
  V1AzureFileVolumeSource
    <$> arbitraryReducedMaybe n -- v1AzureFileVolumeSourceReadOnly :: Maybe Bool
    <*> arbitrary -- v1AzureFileVolumeSourceSecretName :: Text
    <*> arbitrary -- v1AzureFileVolumeSourceShareName :: Text
  
instance Arbitrary V1Binding where
  arbitrary = sized genV1Binding

genV1Binding :: Int -> Gen V1Binding
genV1Binding n =
  V1Binding
    <$> arbitraryReducedMaybe n -- v1BindingApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1BindingKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1BindingMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1BindingTarget :: V1ObjectReference
  
instance Arbitrary V1CSIPersistentVolumeSource where
  arbitrary = sized genV1CSIPersistentVolumeSource

genV1CSIPersistentVolumeSource :: Int -> Gen V1CSIPersistentVolumeSource
genV1CSIPersistentVolumeSource n =
  V1CSIPersistentVolumeSource
    <$> arbitrary -- v1CSIPersistentVolumeSourceDriver :: Text
    <*> arbitraryReducedMaybe n -- v1CSIPersistentVolumeSourceReadOnly :: Maybe Bool
    <*> arbitrary -- v1CSIPersistentVolumeSourceVolumeHandle :: Text
  
instance Arbitrary V1Capabilities where
  arbitrary = sized genV1Capabilities

genV1Capabilities :: Int -> Gen V1Capabilities
genV1Capabilities n =
  V1Capabilities
    <$> arbitraryReducedMaybe n -- v1CapabilitiesAdd :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1CapabilitiesDrop :: Maybe [Text]
  
instance Arbitrary V1CephFSPersistentVolumeSource where
  arbitrary = sized genV1CephFSPersistentVolumeSource

genV1CephFSPersistentVolumeSource :: Int -> Gen V1CephFSPersistentVolumeSource
genV1CephFSPersistentVolumeSource n =
  V1CephFSPersistentVolumeSource
    <$> arbitrary -- v1CephFSPersistentVolumeSourceMonitors :: [Text]
    <*> arbitraryReducedMaybe n -- v1CephFSPersistentVolumeSourcePath :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CephFSPersistentVolumeSourceReadOnly :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1CephFSPersistentVolumeSourceSecretFile :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CephFSPersistentVolumeSourceSecretRef :: Maybe V1SecretReference
    <*> arbitraryReducedMaybe n -- v1CephFSPersistentVolumeSourceUser :: Maybe Text
  
instance Arbitrary V1CephFSVolumeSource where
  arbitrary = sized genV1CephFSVolumeSource

genV1CephFSVolumeSource :: Int -> Gen V1CephFSVolumeSource
genV1CephFSVolumeSource n =
  V1CephFSVolumeSource
    <$> arbitrary -- v1CephFSVolumeSourceMonitors :: [Text]
    <*> arbitraryReducedMaybe n -- v1CephFSVolumeSourcePath :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CephFSVolumeSourceReadOnly :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1CephFSVolumeSourceSecretFile :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CephFSVolumeSourceSecretRef :: Maybe V1LocalObjectReference
    <*> arbitraryReducedMaybe n -- v1CephFSVolumeSourceUser :: Maybe Text
  
instance Arbitrary V1CinderVolumeSource where
  arbitrary = sized genV1CinderVolumeSource

genV1CinderVolumeSource :: Int -> Gen V1CinderVolumeSource
genV1CinderVolumeSource n =
  V1CinderVolumeSource
    <$> arbitraryReducedMaybe n -- v1CinderVolumeSourceFsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CinderVolumeSourceReadOnly :: Maybe Bool
    <*> arbitrary -- v1CinderVolumeSourceVolumeId :: Text
  
instance Arbitrary V1ClientIPConfig where
  arbitrary = sized genV1ClientIPConfig

genV1ClientIPConfig :: Int -> Gen V1ClientIPConfig
genV1ClientIPConfig n =
  V1ClientIPConfig
    <$> arbitraryReducedMaybe n -- v1ClientIPConfigTimeoutSeconds :: Maybe Int
  
instance Arbitrary V1ClusterRole where
  arbitrary = sized genV1ClusterRole

genV1ClusterRole :: Int -> Gen V1ClusterRole
genV1ClusterRole n =
  V1ClusterRole
    <$> arbitraryReducedMaybe n -- v1ClusterRoleAggregationRule :: Maybe V1AggregationRule
    <*> arbitraryReducedMaybe n -- v1ClusterRoleApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ClusterRoleKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ClusterRoleMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1ClusterRoleRules :: [V1PolicyRule]
  
instance Arbitrary V1ClusterRoleBinding where
  arbitrary = sized genV1ClusterRoleBinding

genV1ClusterRoleBinding :: Int -> Gen V1ClusterRoleBinding
genV1ClusterRoleBinding n =
  V1ClusterRoleBinding
    <$> arbitraryReducedMaybe n -- v1ClusterRoleBindingApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ClusterRoleBindingKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ClusterRoleBindingMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1ClusterRoleBindingRoleRef :: V1RoleRef
    <*> arbitraryReduced n -- v1ClusterRoleBindingSubjects :: [V1Subject]
  
instance Arbitrary V1ClusterRoleBindingList where
  arbitrary = sized genV1ClusterRoleBindingList

genV1ClusterRoleBindingList :: Int -> Gen V1ClusterRoleBindingList
genV1ClusterRoleBindingList n =
  V1ClusterRoleBindingList
    <$> arbitraryReducedMaybe n -- v1ClusterRoleBindingListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1ClusterRoleBindingListItems :: [V1ClusterRoleBinding]
    <*> arbitraryReducedMaybe n -- v1ClusterRoleBindingListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ClusterRoleBindingListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1ClusterRoleList where
  arbitrary = sized genV1ClusterRoleList

genV1ClusterRoleList :: Int -> Gen V1ClusterRoleList
genV1ClusterRoleList n =
  V1ClusterRoleList
    <$> arbitraryReducedMaybe n -- v1ClusterRoleListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1ClusterRoleListItems :: [V1ClusterRole]
    <*> arbitraryReducedMaybe n -- v1ClusterRoleListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ClusterRoleListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1ComponentCondition where
  arbitrary = sized genV1ComponentCondition

genV1ComponentCondition :: Int -> Gen V1ComponentCondition
genV1ComponentCondition n =
  V1ComponentCondition
    <$> arbitraryReducedMaybe n -- v1ComponentConditionError :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ComponentConditionMessage :: Maybe Text
    <*> arbitrary -- v1ComponentConditionStatus :: Text
    <*> arbitrary -- v1ComponentConditionType :: Text
  
instance Arbitrary V1ComponentStatus where
  arbitrary = sized genV1ComponentStatus

genV1ComponentStatus :: Int -> Gen V1ComponentStatus
genV1ComponentStatus n =
  V1ComponentStatus
    <$> arbitraryReducedMaybe n -- v1ComponentStatusApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ComponentStatusConditions :: Maybe [V1ComponentCondition]
    <*> arbitraryReducedMaybe n -- v1ComponentStatusKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ComponentStatusMetadata :: Maybe V1ObjectMeta
  
instance Arbitrary V1ComponentStatusList where
  arbitrary = sized genV1ComponentStatusList

genV1ComponentStatusList :: Int -> Gen V1ComponentStatusList
genV1ComponentStatusList n =
  V1ComponentStatusList
    <$> arbitraryReducedMaybe n -- v1ComponentStatusListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1ComponentStatusListItems :: [V1ComponentStatus]
    <*> arbitraryReducedMaybe n -- v1ComponentStatusListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ComponentStatusListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1ConfigMap where
  arbitrary = sized genV1ConfigMap

genV1ConfigMap :: Int -> Gen V1ConfigMap
genV1ConfigMap n =
  V1ConfigMap
    <$> arbitraryReducedMaybe n -- v1ConfigMapApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ConfigMapData :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1ConfigMapKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ConfigMapMetadata :: Maybe V1ObjectMeta
  
instance Arbitrary V1ConfigMapEnvSource where
  arbitrary = sized genV1ConfigMapEnvSource

genV1ConfigMapEnvSource :: Int -> Gen V1ConfigMapEnvSource
genV1ConfigMapEnvSource n =
  V1ConfigMapEnvSource
    <$> arbitraryReducedMaybe n -- v1ConfigMapEnvSourceName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ConfigMapEnvSourceOptional :: Maybe Bool
  
instance Arbitrary V1ConfigMapKeySelector where
  arbitrary = sized genV1ConfigMapKeySelector

genV1ConfigMapKeySelector :: Int -> Gen V1ConfigMapKeySelector
genV1ConfigMapKeySelector n =
  V1ConfigMapKeySelector
    <$> arbitrary -- v1ConfigMapKeySelectorKey :: Text
    <*> arbitraryReducedMaybe n -- v1ConfigMapKeySelectorName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ConfigMapKeySelectorOptional :: Maybe Bool
  
instance Arbitrary V1ConfigMapList where
  arbitrary = sized genV1ConfigMapList

genV1ConfigMapList :: Int -> Gen V1ConfigMapList
genV1ConfigMapList n =
  V1ConfigMapList
    <$> arbitraryReducedMaybe n -- v1ConfigMapListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1ConfigMapListItems :: [V1ConfigMap]
    <*> arbitraryReducedMaybe n -- v1ConfigMapListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ConfigMapListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1ConfigMapProjection where
  arbitrary = sized genV1ConfigMapProjection

genV1ConfigMapProjection :: Int -> Gen V1ConfigMapProjection
genV1ConfigMapProjection n =
  V1ConfigMapProjection
    <$> arbitraryReducedMaybe n -- v1ConfigMapProjectionItems :: Maybe [V1KeyToPath]
    <*> arbitraryReducedMaybe n -- v1ConfigMapProjectionName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ConfigMapProjectionOptional :: Maybe Bool
  
instance Arbitrary V1ConfigMapVolumeSource where
  arbitrary = sized genV1ConfigMapVolumeSource

genV1ConfigMapVolumeSource :: Int -> Gen V1ConfigMapVolumeSource
genV1ConfigMapVolumeSource n =
  V1ConfigMapVolumeSource
    <$> arbitraryReducedMaybe n -- v1ConfigMapVolumeSourceDefaultMode :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1ConfigMapVolumeSourceItems :: Maybe [V1KeyToPath]
    <*> arbitraryReducedMaybe n -- v1ConfigMapVolumeSourceName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ConfigMapVolumeSourceOptional :: Maybe Bool
  
instance Arbitrary V1Container where
  arbitrary = sized genV1Container

genV1Container :: Int -> Gen V1Container
genV1Container n =
  V1Container
    <$> arbitraryReducedMaybe n -- v1ContainerArgs :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1ContainerCommand :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1ContainerEnv :: Maybe [V1EnvVar]
    <*> arbitraryReducedMaybe n -- v1ContainerEnvFrom :: Maybe [V1EnvFromSource]
    <*> arbitraryReducedMaybe n -- v1ContainerImage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ContainerImagePullPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ContainerLifecycle :: Maybe V1Lifecycle
    <*> arbitraryReducedMaybe n -- v1ContainerLivenessProbe :: Maybe V1Probe
    <*> arbitrary -- v1ContainerName :: Text
    <*> arbitraryReducedMaybe n -- v1ContainerPorts :: Maybe [V1ContainerPort]
    <*> arbitraryReducedMaybe n -- v1ContainerReadinessProbe :: Maybe V1Probe
    <*> arbitraryReducedMaybe n -- v1ContainerResources :: Maybe V1ResourceRequirements
    <*> arbitraryReducedMaybe n -- v1ContainerSecurityContext :: Maybe V1SecurityContext
    <*> arbitraryReducedMaybe n -- v1ContainerStdin :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1ContainerStdinOnce :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1ContainerTerminationMessagePath :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ContainerTerminationMessagePolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ContainerTty :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1ContainerVolumeDevices :: Maybe [V1VolumeDevice]
    <*> arbitraryReducedMaybe n -- v1ContainerVolumeMounts :: Maybe [V1VolumeMount]
    <*> arbitraryReducedMaybe n -- v1ContainerWorkingDir :: Maybe Text
  
instance Arbitrary V1ContainerImage where
  arbitrary = sized genV1ContainerImage

genV1ContainerImage :: Int -> Gen V1ContainerImage
genV1ContainerImage n =
  V1ContainerImage
    <$> arbitrary -- v1ContainerImageNames :: [Text]
    <*> arbitraryReducedMaybe n -- v1ContainerImageSizeBytes :: Maybe Integer
  
instance Arbitrary V1ContainerPort where
  arbitrary = sized genV1ContainerPort

genV1ContainerPort :: Int -> Gen V1ContainerPort
genV1ContainerPort n =
  V1ContainerPort
    <$> arbitrary -- v1ContainerPortContainerPort :: Int
    <*> arbitraryReducedMaybe n -- v1ContainerPortHostIp :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ContainerPortHostPort :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1ContainerPortName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ContainerPortProtocol :: Maybe Text
  
instance Arbitrary V1ContainerState where
  arbitrary = sized genV1ContainerState

genV1ContainerState :: Int -> Gen V1ContainerState
genV1ContainerState n =
  V1ContainerState
    <$> arbitraryReducedMaybe n -- v1ContainerStateRunning :: Maybe V1ContainerStateRunning
    <*> arbitraryReducedMaybe n -- v1ContainerStateTerminated :: Maybe V1ContainerStateTerminated
    <*> arbitraryReducedMaybe n -- v1ContainerStateWaiting :: Maybe V1ContainerStateWaiting
  
instance Arbitrary V1ContainerStateRunning where
  arbitrary = sized genV1ContainerStateRunning

genV1ContainerStateRunning :: Int -> Gen V1ContainerStateRunning
genV1ContainerStateRunning n =
  V1ContainerStateRunning
    <$> arbitraryReducedMaybe n -- v1ContainerStateRunningStartedAt :: Maybe DateTime
  
instance Arbitrary V1ContainerStateTerminated where
  arbitrary = sized genV1ContainerStateTerminated

genV1ContainerStateTerminated :: Int -> Gen V1ContainerStateTerminated
genV1ContainerStateTerminated n =
  V1ContainerStateTerminated
    <$> arbitraryReducedMaybe n -- v1ContainerStateTerminatedContainerId :: Maybe Text
    <*> arbitrary -- v1ContainerStateTerminatedExitCode :: Int
    <*> arbitraryReducedMaybe n -- v1ContainerStateTerminatedFinishedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1ContainerStateTerminatedMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ContainerStateTerminatedReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ContainerStateTerminatedSignal :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1ContainerStateTerminatedStartedAt :: Maybe DateTime
  
instance Arbitrary V1ContainerStateWaiting where
  arbitrary = sized genV1ContainerStateWaiting

genV1ContainerStateWaiting :: Int -> Gen V1ContainerStateWaiting
genV1ContainerStateWaiting n =
  V1ContainerStateWaiting
    <$> arbitraryReducedMaybe n -- v1ContainerStateWaitingMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ContainerStateWaitingReason :: Maybe Text
  
instance Arbitrary V1ContainerStatus where
  arbitrary = sized genV1ContainerStatus

genV1ContainerStatus :: Int -> Gen V1ContainerStatus
genV1ContainerStatus n =
  V1ContainerStatus
    <$> arbitraryReducedMaybe n -- v1ContainerStatusContainerId :: Maybe Text
    <*> arbitrary -- v1ContainerStatusImage :: Text
    <*> arbitrary -- v1ContainerStatusImageId :: Text
    <*> arbitraryReducedMaybe n -- v1ContainerStatusLastState :: Maybe V1ContainerState
    <*> arbitrary -- v1ContainerStatusName :: Text
    <*> arbitrary -- v1ContainerStatusReady :: Bool
    <*> arbitrary -- v1ContainerStatusRestartCount :: Int
    <*> arbitraryReducedMaybe n -- v1ContainerStatusState :: Maybe V1ContainerState
  
instance Arbitrary V1ControllerRevision where
  arbitrary = sized genV1ControllerRevision

genV1ControllerRevision :: Int -> Gen V1ControllerRevision
genV1ControllerRevision n =
  V1ControllerRevision
    <$> arbitraryReducedMaybe n -- v1ControllerRevisionApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ControllerRevisionData :: Maybe RuntimeRawExtension
    <*> arbitraryReducedMaybe n -- v1ControllerRevisionKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ControllerRevisionMetadata :: Maybe V1ObjectMeta
    <*> arbitrary -- v1ControllerRevisionRevision :: Integer
  
instance Arbitrary V1ControllerRevisionList where
  arbitrary = sized genV1ControllerRevisionList

genV1ControllerRevisionList :: Int -> Gen V1ControllerRevisionList
genV1ControllerRevisionList n =
  V1ControllerRevisionList
    <$> arbitraryReducedMaybe n -- v1ControllerRevisionListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1ControllerRevisionListItems :: [V1ControllerRevision]
    <*> arbitraryReducedMaybe n -- v1ControllerRevisionListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ControllerRevisionListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1CrossVersionObjectReference where
  arbitrary = sized genV1CrossVersionObjectReference

genV1CrossVersionObjectReference :: Int -> Gen V1CrossVersionObjectReference
genV1CrossVersionObjectReference n =
  V1CrossVersionObjectReference
    <$> arbitraryReducedMaybe n -- v1CrossVersionObjectReferenceApiVersion :: Maybe Text
    <*> arbitrary -- v1CrossVersionObjectReferenceKind :: Text
    <*> arbitrary -- v1CrossVersionObjectReferenceName :: Text
  
instance Arbitrary V1DaemonEndpoint where
  arbitrary = sized genV1DaemonEndpoint

genV1DaemonEndpoint :: Int -> Gen V1DaemonEndpoint
genV1DaemonEndpoint n =
  V1DaemonEndpoint
    <$> arbitrary -- v1DaemonEndpointPort :: Int
  
instance Arbitrary V1DaemonSet where
  arbitrary = sized genV1DaemonSet

genV1DaemonSet :: Int -> Gen V1DaemonSet
genV1DaemonSet n =
  V1DaemonSet
    <$> arbitraryReducedMaybe n -- v1DaemonSetApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1DaemonSetKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1DaemonSetMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1DaemonSetSpec :: Maybe V1DaemonSetSpec
    <*> arbitraryReducedMaybe n -- v1DaemonSetStatus :: Maybe V1DaemonSetStatus
  
instance Arbitrary V1DaemonSetCondition where
  arbitrary = sized genV1DaemonSetCondition

genV1DaemonSetCondition :: Int -> Gen V1DaemonSetCondition
genV1DaemonSetCondition n =
  V1DaemonSetCondition
    <$> arbitraryReducedMaybe n -- v1DaemonSetConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1DaemonSetConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1DaemonSetConditionReason :: Maybe Text
    <*> arbitrary -- v1DaemonSetConditionStatus :: Text
    <*> arbitrary -- v1DaemonSetConditionType :: Text
  
instance Arbitrary V1DaemonSetList where
  arbitrary = sized genV1DaemonSetList

genV1DaemonSetList :: Int -> Gen V1DaemonSetList
genV1DaemonSetList n =
  V1DaemonSetList
    <$> arbitraryReducedMaybe n -- v1DaemonSetListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1DaemonSetListItems :: [V1DaemonSet]
    <*> arbitraryReducedMaybe n -- v1DaemonSetListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1DaemonSetListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1DaemonSetSpec where
  arbitrary = sized genV1DaemonSetSpec

genV1DaemonSetSpec :: Int -> Gen V1DaemonSetSpec
genV1DaemonSetSpec n =
  V1DaemonSetSpec
    <$> arbitraryReducedMaybe n -- v1DaemonSetSpecMinReadySeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1DaemonSetSpecRevisionHistoryLimit :: Maybe Int
    <*> arbitraryReduced n -- v1DaemonSetSpecSelector :: V1LabelSelector
    <*> arbitraryReduced n -- v1DaemonSetSpecTemplate :: V1PodTemplateSpec
    <*> arbitraryReducedMaybe n -- v1DaemonSetSpecUpdateStrategy :: Maybe V1DaemonSetUpdateStrategy
  
instance Arbitrary V1DaemonSetStatus where
  arbitrary = sized genV1DaemonSetStatus

genV1DaemonSetStatus :: Int -> Gen V1DaemonSetStatus
genV1DaemonSetStatus n =
  V1DaemonSetStatus
    <$> arbitraryReducedMaybe n -- v1DaemonSetStatusCollisionCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1DaemonSetStatusConditions :: Maybe [V1DaemonSetCondition]
    <*> arbitrary -- v1DaemonSetStatusCurrentNumberScheduled :: Int
    <*> arbitrary -- v1DaemonSetStatusDesiredNumberScheduled :: Int
    <*> arbitraryReducedMaybe n -- v1DaemonSetStatusNumberAvailable :: Maybe Int
    <*> arbitrary -- v1DaemonSetStatusNumberMisscheduled :: Int
    <*> arbitrary -- v1DaemonSetStatusNumberReady :: Int
    <*> arbitraryReducedMaybe n -- v1DaemonSetStatusNumberUnavailable :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1DaemonSetStatusObservedGeneration :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1DaemonSetStatusUpdatedNumberScheduled :: Maybe Int
  
instance Arbitrary V1DaemonSetUpdateStrategy where
  arbitrary = sized genV1DaemonSetUpdateStrategy

genV1DaemonSetUpdateStrategy :: Int -> Gen V1DaemonSetUpdateStrategy
genV1DaemonSetUpdateStrategy n =
  V1DaemonSetUpdateStrategy
    <$> arbitraryReducedMaybe n -- v1DaemonSetUpdateStrategyRollingUpdate :: Maybe V1RollingUpdateDaemonSet
    <*> arbitraryReducedMaybe n -- v1DaemonSetUpdateStrategyType :: Maybe Text
  
instance Arbitrary V1DeleteOptions where
  arbitrary = sized genV1DeleteOptions

genV1DeleteOptions :: Int -> Gen V1DeleteOptions
genV1DeleteOptions n =
  V1DeleteOptions
    <$> arbitraryReducedMaybe n -- v1DeleteOptionsApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1DeleteOptionsGracePeriodSeconds :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1DeleteOptionsKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1DeleteOptionsOrphanDependents :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1DeleteOptionsPreconditions :: Maybe V1Preconditions
    <*> arbitraryReducedMaybe n -- v1DeleteOptionsPropagationPolicy :: Maybe Text
  
instance Arbitrary V1Deployment where
  arbitrary = sized genV1Deployment

genV1Deployment :: Int -> Gen V1Deployment
genV1Deployment n =
  V1Deployment
    <$> arbitraryReducedMaybe n -- v1DeploymentApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1DeploymentKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1DeploymentMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1DeploymentSpec :: Maybe V1DeploymentSpec
    <*> arbitraryReducedMaybe n -- v1DeploymentStatus :: Maybe V1DeploymentStatus
  
instance Arbitrary V1DeploymentCondition where
  arbitrary = sized genV1DeploymentCondition

genV1DeploymentCondition :: Int -> Gen V1DeploymentCondition
genV1DeploymentCondition n =
  V1DeploymentCondition
    <$> arbitraryReducedMaybe n -- v1DeploymentConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1DeploymentConditionLastUpdateTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1DeploymentConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1DeploymentConditionReason :: Maybe Text
    <*> arbitrary -- v1DeploymentConditionStatus :: Text
    <*> arbitrary -- v1DeploymentConditionType :: Text
  
instance Arbitrary V1DeploymentList where
  arbitrary = sized genV1DeploymentList

genV1DeploymentList :: Int -> Gen V1DeploymentList
genV1DeploymentList n =
  V1DeploymentList
    <$> arbitraryReducedMaybe n -- v1DeploymentListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1DeploymentListItems :: [V1Deployment]
    <*> arbitraryReducedMaybe n -- v1DeploymentListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1DeploymentListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1DeploymentSpec where
  arbitrary = sized genV1DeploymentSpec

genV1DeploymentSpec :: Int -> Gen V1DeploymentSpec
genV1DeploymentSpec n =
  V1DeploymentSpec
    <$> arbitraryReducedMaybe n -- v1DeploymentSpecMinReadySeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1DeploymentSpecPaused :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1DeploymentSpecProgressDeadlineSeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1DeploymentSpecReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1DeploymentSpecRevisionHistoryLimit :: Maybe Int
    <*> arbitraryReduced n -- v1DeploymentSpecSelector :: V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1DeploymentSpecStrategy :: Maybe V1DeploymentStrategy
    <*> arbitraryReduced n -- v1DeploymentSpecTemplate :: V1PodTemplateSpec
  
instance Arbitrary V1DeploymentStatus where
  arbitrary = sized genV1DeploymentStatus

genV1DeploymentStatus :: Int -> Gen V1DeploymentStatus
genV1DeploymentStatus n =
  V1DeploymentStatus
    <$> arbitraryReducedMaybe n -- v1DeploymentStatusAvailableReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1DeploymentStatusCollisionCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1DeploymentStatusConditions :: Maybe [V1DeploymentCondition]
    <*> arbitraryReducedMaybe n -- v1DeploymentStatusObservedGeneration :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1DeploymentStatusReadyReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1DeploymentStatusReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1DeploymentStatusUnavailableReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1DeploymentStatusUpdatedReplicas :: Maybe Int
  
instance Arbitrary V1DeploymentStrategy where
  arbitrary = sized genV1DeploymentStrategy

genV1DeploymentStrategy :: Int -> Gen V1DeploymentStrategy
genV1DeploymentStrategy n =
  V1DeploymentStrategy
    <$> arbitraryReducedMaybe n -- v1DeploymentStrategyRollingUpdate :: Maybe V1RollingUpdateDeployment
    <*> arbitraryReducedMaybe n -- v1DeploymentStrategyType :: Maybe Text
  
instance Arbitrary V1DownwardAPIProjection where
  arbitrary = sized genV1DownwardAPIProjection

genV1DownwardAPIProjection :: Int -> Gen V1DownwardAPIProjection
genV1DownwardAPIProjection n =
  V1DownwardAPIProjection
    <$> arbitraryReducedMaybe n -- v1DownwardAPIProjectionItems :: Maybe [V1DownwardAPIVolumeFile]
  
instance Arbitrary V1DownwardAPIVolumeFile where
  arbitrary = sized genV1DownwardAPIVolumeFile

genV1DownwardAPIVolumeFile :: Int -> Gen V1DownwardAPIVolumeFile
genV1DownwardAPIVolumeFile n =
  V1DownwardAPIVolumeFile
    <$> arbitraryReducedMaybe n -- v1DownwardAPIVolumeFileFieldRef :: Maybe V1ObjectFieldSelector
    <*> arbitraryReducedMaybe n -- v1DownwardAPIVolumeFileMode :: Maybe Int
    <*> arbitrary -- v1DownwardAPIVolumeFilePath :: Text
    <*> arbitraryReducedMaybe n -- v1DownwardAPIVolumeFileResourceFieldRef :: Maybe V1ResourceFieldSelector
  
instance Arbitrary V1DownwardAPIVolumeSource where
  arbitrary = sized genV1DownwardAPIVolumeSource

genV1DownwardAPIVolumeSource :: Int -> Gen V1DownwardAPIVolumeSource
genV1DownwardAPIVolumeSource n =
  V1DownwardAPIVolumeSource
    <$> arbitraryReducedMaybe n -- v1DownwardAPIVolumeSourceDefaultMode :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1DownwardAPIVolumeSourceItems :: Maybe [V1DownwardAPIVolumeFile]
  
instance Arbitrary V1EmptyDirVolumeSource where
  arbitrary = sized genV1EmptyDirVolumeSource

genV1EmptyDirVolumeSource :: Int -> Gen V1EmptyDirVolumeSource
genV1EmptyDirVolumeSource n =
  V1EmptyDirVolumeSource
    <$> arbitraryReducedMaybe n -- v1EmptyDirVolumeSourceMedium :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EmptyDirVolumeSourceSizeLimit :: Maybe Text
  
instance Arbitrary V1EndpointAddress where
  arbitrary = sized genV1EndpointAddress

genV1EndpointAddress :: Int -> Gen V1EndpointAddress
genV1EndpointAddress n =
  V1EndpointAddress
    <$> arbitraryReducedMaybe n -- v1EndpointAddressHostname :: Maybe Text
    <*> arbitrary -- v1EndpointAddressIp :: Text
    <*> arbitraryReducedMaybe n -- v1EndpointAddressNodeName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EndpointAddressTargetRef :: Maybe V1ObjectReference
  
instance Arbitrary V1EndpointPort where
  arbitrary = sized genV1EndpointPort

genV1EndpointPort :: Int -> Gen V1EndpointPort
genV1EndpointPort n =
  V1EndpointPort
    <$> arbitraryReducedMaybe n -- v1EndpointPortName :: Maybe Text
    <*> arbitrary -- v1EndpointPortPort :: Int
    <*> arbitraryReducedMaybe n -- v1EndpointPortProtocol :: Maybe Text
  
instance Arbitrary V1EndpointSubset where
  arbitrary = sized genV1EndpointSubset

genV1EndpointSubset :: Int -> Gen V1EndpointSubset
genV1EndpointSubset n =
  V1EndpointSubset
    <$> arbitraryReducedMaybe n -- v1EndpointSubsetAddresses :: Maybe [V1EndpointAddress]
    <*> arbitraryReducedMaybe n -- v1EndpointSubsetNotReadyAddresses :: Maybe [V1EndpointAddress]
    <*> arbitraryReducedMaybe n -- v1EndpointSubsetPorts :: Maybe [V1EndpointPort]
  
instance Arbitrary V1Endpoints where
  arbitrary = sized genV1Endpoints

genV1Endpoints :: Int -> Gen V1Endpoints
genV1Endpoints n =
  V1Endpoints
    <$> arbitraryReducedMaybe n -- v1EndpointsApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EndpointsKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EndpointsMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1EndpointsSubsets :: [V1EndpointSubset]
  
instance Arbitrary V1EndpointsList where
  arbitrary = sized genV1EndpointsList

genV1EndpointsList :: Int -> Gen V1EndpointsList
genV1EndpointsList n =
  V1EndpointsList
    <$> arbitraryReducedMaybe n -- v1EndpointsListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1EndpointsListItems :: [V1Endpoints]
    <*> arbitraryReducedMaybe n -- v1EndpointsListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EndpointsListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1EnvFromSource where
  arbitrary = sized genV1EnvFromSource

genV1EnvFromSource :: Int -> Gen V1EnvFromSource
genV1EnvFromSource n =
  V1EnvFromSource
    <$> arbitraryReducedMaybe n -- v1EnvFromSourceConfigMapRef :: Maybe V1ConfigMapEnvSource
    <*> arbitraryReducedMaybe n -- v1EnvFromSourcePrefix :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EnvFromSourceSecretRef :: Maybe V1SecretEnvSource
  
instance Arbitrary V1EnvVar where
  arbitrary = sized genV1EnvVar

genV1EnvVar :: Int -> Gen V1EnvVar
genV1EnvVar n =
  V1EnvVar
    <$> arbitrary -- v1EnvVarName :: Text
    <*> arbitraryReducedMaybe n -- v1EnvVarValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EnvVarValueFrom :: Maybe V1EnvVarSource
  
instance Arbitrary V1EnvVarSource where
  arbitrary = sized genV1EnvVarSource

genV1EnvVarSource :: Int -> Gen V1EnvVarSource
genV1EnvVarSource n =
  V1EnvVarSource
    <$> arbitraryReducedMaybe n -- v1EnvVarSourceConfigMapKeyRef :: Maybe V1ConfigMapKeySelector
    <*> arbitraryReducedMaybe n -- v1EnvVarSourceFieldRef :: Maybe V1ObjectFieldSelector
    <*> arbitraryReducedMaybe n -- v1EnvVarSourceResourceFieldRef :: Maybe V1ResourceFieldSelector
    <*> arbitraryReducedMaybe n -- v1EnvVarSourceSecretKeyRef :: Maybe V1SecretKeySelector
  
instance Arbitrary V1Event where
  arbitrary = sized genV1Event

genV1Event :: Int -> Gen V1Event
genV1Event n =
  V1Event
    <$> arbitraryReducedMaybe n -- v1EventAction :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EventApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EventCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1EventEventTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1EventFirstTimestamp :: Maybe DateTime
    <*> arbitraryReduced n -- v1EventInvolvedObject :: V1ObjectReference
    <*> arbitraryReducedMaybe n -- v1EventKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EventLastTimestamp :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1EventMessage :: Maybe Text
    <*> arbitraryReduced n -- v1EventMetadata :: V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1EventReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EventRelated :: Maybe V1ObjectReference
    <*> arbitraryReducedMaybe n -- v1EventReportingComponent :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EventReportingInstance :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EventSeries :: Maybe V1EventSeries
    <*> arbitraryReducedMaybe n -- v1EventSource :: Maybe V1EventSource
    <*> arbitraryReducedMaybe n -- v1EventType :: Maybe Text
  
instance Arbitrary V1EventList where
  arbitrary = sized genV1EventList

genV1EventList :: Int -> Gen V1EventList
genV1EventList n =
  V1EventList
    <$> arbitraryReducedMaybe n -- v1EventListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1EventListItems :: [V1Event]
    <*> arbitraryReducedMaybe n -- v1EventListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EventListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1EventSeries where
  arbitrary = sized genV1EventSeries

genV1EventSeries :: Int -> Gen V1EventSeries
genV1EventSeries n =
  V1EventSeries
    <$> arbitraryReducedMaybe n -- v1EventSeriesCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1EventSeriesLastObservedTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1EventSeriesState :: Maybe Text
  
instance Arbitrary V1EventSource where
  arbitrary = sized genV1EventSource

genV1EventSource :: Int -> Gen V1EventSource
genV1EventSource n =
  V1EventSource
    <$> arbitraryReducedMaybe n -- v1EventSourceComponent :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EventSourceHost :: Maybe Text
  
instance Arbitrary V1ExecAction where
  arbitrary = sized genV1ExecAction

genV1ExecAction :: Int -> Gen V1ExecAction
genV1ExecAction n =
  V1ExecAction
    <$> arbitraryReducedMaybe n -- v1ExecActionCommand :: Maybe [Text]
  
instance Arbitrary V1FCVolumeSource where
  arbitrary = sized genV1FCVolumeSource

genV1FCVolumeSource :: Int -> Gen V1FCVolumeSource
genV1FCVolumeSource n =
  V1FCVolumeSource
    <$> arbitraryReducedMaybe n -- v1FCVolumeSourceFsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1FCVolumeSourceLun :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1FCVolumeSourceReadOnly :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1FCVolumeSourceTargetWwNs :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1FCVolumeSourceWwids :: Maybe [Text]
  
instance Arbitrary V1FlexVolumeSource where
  arbitrary = sized genV1FlexVolumeSource

genV1FlexVolumeSource :: Int -> Gen V1FlexVolumeSource
genV1FlexVolumeSource n =
  V1FlexVolumeSource
    <$> arbitrary -- v1FlexVolumeSourceDriver :: Text
    <*> arbitraryReducedMaybe n -- v1FlexVolumeSourceFsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1FlexVolumeSourceOptions :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1FlexVolumeSourceReadOnly :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1FlexVolumeSourceSecretRef :: Maybe V1LocalObjectReference
  
instance Arbitrary V1FlockerVolumeSource where
  arbitrary = sized genV1FlockerVolumeSource

genV1FlockerVolumeSource :: Int -> Gen V1FlockerVolumeSource
genV1FlockerVolumeSource n =
  V1FlockerVolumeSource
    <$> arbitraryReducedMaybe n -- v1FlockerVolumeSourceDatasetName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1FlockerVolumeSourceDatasetUuid :: Maybe Text
  
instance Arbitrary V1GCEPersistentDiskVolumeSource where
  arbitrary = sized genV1GCEPersistentDiskVolumeSource

genV1GCEPersistentDiskVolumeSource :: Int -> Gen V1GCEPersistentDiskVolumeSource
genV1GCEPersistentDiskVolumeSource n =
  V1GCEPersistentDiskVolumeSource
    <$> arbitraryReducedMaybe n -- v1GCEPersistentDiskVolumeSourceFsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1GCEPersistentDiskVolumeSourcePartition :: Maybe Int
    <*> arbitrary -- v1GCEPersistentDiskVolumeSourcePdName :: Text
    <*> arbitraryReducedMaybe n -- v1GCEPersistentDiskVolumeSourceReadOnly :: Maybe Bool
  
instance Arbitrary V1GitRepoVolumeSource where
  arbitrary = sized genV1GitRepoVolumeSource

genV1GitRepoVolumeSource :: Int -> Gen V1GitRepoVolumeSource
genV1GitRepoVolumeSource n =
  V1GitRepoVolumeSource
    <$> arbitraryReducedMaybe n -- v1GitRepoVolumeSourceDirectory :: Maybe Text
    <*> arbitrary -- v1GitRepoVolumeSourceRepository :: Text
    <*> arbitraryReducedMaybe n -- v1GitRepoVolumeSourceRevision :: Maybe Text
  
instance Arbitrary V1GlusterfsVolumeSource where
  arbitrary = sized genV1GlusterfsVolumeSource

genV1GlusterfsVolumeSource :: Int -> Gen V1GlusterfsVolumeSource
genV1GlusterfsVolumeSource n =
  V1GlusterfsVolumeSource
    <$> arbitrary -- v1GlusterfsVolumeSourceEndpoints :: Text
    <*> arbitrary -- v1GlusterfsVolumeSourcePath :: Text
    <*> arbitraryReducedMaybe n -- v1GlusterfsVolumeSourceReadOnly :: Maybe Bool
  
instance Arbitrary V1GroupVersionForDiscovery where
  arbitrary = sized genV1GroupVersionForDiscovery

genV1GroupVersionForDiscovery :: Int -> Gen V1GroupVersionForDiscovery
genV1GroupVersionForDiscovery n =
  V1GroupVersionForDiscovery
    <$> arbitrary -- v1GroupVersionForDiscoveryGroupVersion :: Text
    <*> arbitrary -- v1GroupVersionForDiscoveryVersion :: Text
  
instance Arbitrary V1HTTPGetAction where
  arbitrary = sized genV1HTTPGetAction

genV1HTTPGetAction :: Int -> Gen V1HTTPGetAction
genV1HTTPGetAction n =
  V1HTTPGetAction
    <$> arbitraryReducedMaybe n -- v1HTTPGetActionHost :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HTTPGetActionHttpHeaders :: Maybe [V1HTTPHeader]
    <*> arbitraryReducedMaybe n -- v1HTTPGetActionPath :: Maybe Text
    <*> arbitraryReduced n -- v1HTTPGetActionPort :: A.Value
    <*> arbitraryReducedMaybe n -- v1HTTPGetActionScheme :: Maybe Text
  
instance Arbitrary V1HTTPHeader where
  arbitrary = sized genV1HTTPHeader

genV1HTTPHeader :: Int -> Gen V1HTTPHeader
genV1HTTPHeader n =
  V1HTTPHeader
    <$> arbitrary -- v1HTTPHeaderName :: Text
    <*> arbitrary -- v1HTTPHeaderValue :: Text
  
instance Arbitrary V1Handler where
  arbitrary = sized genV1Handler

genV1Handler :: Int -> Gen V1Handler
genV1Handler n =
  V1Handler
    <$> arbitraryReducedMaybe n -- v1HandlerExec :: Maybe V1ExecAction
    <*> arbitraryReducedMaybe n -- v1HandlerHttpGet :: Maybe V1HTTPGetAction
    <*> arbitraryReducedMaybe n -- v1HandlerTcpSocket :: Maybe V1TCPSocketAction
  
instance Arbitrary V1HorizontalPodAutoscaler where
  arbitrary = sized genV1HorizontalPodAutoscaler

genV1HorizontalPodAutoscaler :: Int -> Gen V1HorizontalPodAutoscaler
genV1HorizontalPodAutoscaler n =
  V1HorizontalPodAutoscaler
    <$> arbitraryReducedMaybe n -- v1HorizontalPodAutoscalerApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HorizontalPodAutoscalerKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HorizontalPodAutoscalerMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1HorizontalPodAutoscalerSpec :: Maybe V1HorizontalPodAutoscalerSpec
    <*> arbitraryReducedMaybe n -- v1HorizontalPodAutoscalerStatus :: Maybe V1HorizontalPodAutoscalerStatus
  
instance Arbitrary V1HorizontalPodAutoscalerList where
  arbitrary = sized genV1HorizontalPodAutoscalerList

genV1HorizontalPodAutoscalerList :: Int -> Gen V1HorizontalPodAutoscalerList
genV1HorizontalPodAutoscalerList n =
  V1HorizontalPodAutoscalerList
    <$> arbitraryReducedMaybe n -- v1HorizontalPodAutoscalerListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1HorizontalPodAutoscalerListItems :: [V1HorizontalPodAutoscaler]
    <*> arbitraryReducedMaybe n -- v1HorizontalPodAutoscalerListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1HorizontalPodAutoscalerListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1HorizontalPodAutoscalerSpec where
  arbitrary = sized genV1HorizontalPodAutoscalerSpec

genV1HorizontalPodAutoscalerSpec :: Int -> Gen V1HorizontalPodAutoscalerSpec
genV1HorizontalPodAutoscalerSpec n =
  V1HorizontalPodAutoscalerSpec
    <$> arbitrary -- v1HorizontalPodAutoscalerSpecMaxReplicas :: Int
    <*> arbitraryReducedMaybe n -- v1HorizontalPodAutoscalerSpecMinReplicas :: Maybe Int
    <*> arbitraryReduced n -- v1HorizontalPodAutoscalerSpecScaleTargetRef :: V1CrossVersionObjectReference
    <*> arbitraryReducedMaybe n -- v1HorizontalPodAutoscalerSpecTargetCpuUtilizationPercentage :: Maybe Int
  
instance Arbitrary V1HorizontalPodAutoscalerStatus where
  arbitrary = sized genV1HorizontalPodAutoscalerStatus

genV1HorizontalPodAutoscalerStatus :: Int -> Gen V1HorizontalPodAutoscalerStatus
genV1HorizontalPodAutoscalerStatus n =
  V1HorizontalPodAutoscalerStatus
    <$> arbitraryReducedMaybe n -- v1HorizontalPodAutoscalerStatusCurrentCpuUtilizationPercentage :: Maybe Int
    <*> arbitrary -- v1HorizontalPodAutoscalerStatusCurrentReplicas :: Int
    <*> arbitrary -- v1HorizontalPodAutoscalerStatusDesiredReplicas :: Int
    <*> arbitraryReducedMaybe n -- v1HorizontalPodAutoscalerStatusLastScaleTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1HorizontalPodAutoscalerStatusObservedGeneration :: Maybe Integer
  
instance Arbitrary V1HostAlias where
  arbitrary = sized genV1HostAlias

genV1HostAlias :: Int -> Gen V1HostAlias
genV1HostAlias n =
  V1HostAlias
    <$> arbitraryReducedMaybe n -- v1HostAliasHostnames :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1HostAliasIp :: Maybe Text
  
instance Arbitrary V1HostPathVolumeSource where
  arbitrary = sized genV1HostPathVolumeSource

genV1HostPathVolumeSource :: Int -> Gen V1HostPathVolumeSource
genV1HostPathVolumeSource n =
  V1HostPathVolumeSource
    <$> arbitrary -- v1HostPathVolumeSourcePath :: Text
    <*> arbitraryReducedMaybe n -- v1HostPathVolumeSourceType :: Maybe Text
  
instance Arbitrary V1IPBlock where
  arbitrary = sized genV1IPBlock

genV1IPBlock :: Int -> Gen V1IPBlock
genV1IPBlock n =
  V1IPBlock
    <$> arbitrary -- v1IPBlockCidr :: Text
    <*> arbitraryReducedMaybe n -- v1IPBlockExcept :: Maybe [Text]
  
instance Arbitrary V1ISCSIPersistentVolumeSource where
  arbitrary = sized genV1ISCSIPersistentVolumeSource

genV1ISCSIPersistentVolumeSource :: Int -> Gen V1ISCSIPersistentVolumeSource
genV1ISCSIPersistentVolumeSource n =
  V1ISCSIPersistentVolumeSource
    <$> arbitraryReducedMaybe n -- v1ISCSIPersistentVolumeSourceChapAuthDiscovery :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1ISCSIPersistentVolumeSourceChapAuthSession :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1ISCSIPersistentVolumeSourceFsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ISCSIPersistentVolumeSourceInitiatorName :: Maybe Text
    <*> arbitrary -- v1ISCSIPersistentVolumeSourceIqn :: Text
    <*> arbitraryReducedMaybe n -- v1ISCSIPersistentVolumeSourceIscsiInterface :: Maybe Text
    <*> arbitrary -- v1ISCSIPersistentVolumeSourceLun :: Int
    <*> arbitraryReducedMaybe n -- v1ISCSIPersistentVolumeSourcePortals :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1ISCSIPersistentVolumeSourceReadOnly :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1ISCSIPersistentVolumeSourceSecretRef :: Maybe V1SecretReference
    <*> arbitrary -- v1ISCSIPersistentVolumeSourceTargetPortal :: Text
  
instance Arbitrary V1ISCSIVolumeSource where
  arbitrary = sized genV1ISCSIVolumeSource

genV1ISCSIVolumeSource :: Int -> Gen V1ISCSIVolumeSource
genV1ISCSIVolumeSource n =
  V1ISCSIVolumeSource
    <$> arbitraryReducedMaybe n -- v1ISCSIVolumeSourceChapAuthDiscovery :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1ISCSIVolumeSourceChapAuthSession :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1ISCSIVolumeSourceFsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ISCSIVolumeSourceInitiatorName :: Maybe Text
    <*> arbitrary -- v1ISCSIVolumeSourceIqn :: Text
    <*> arbitraryReducedMaybe n -- v1ISCSIVolumeSourceIscsiInterface :: Maybe Text
    <*> arbitrary -- v1ISCSIVolumeSourceLun :: Int
    <*> arbitraryReducedMaybe n -- v1ISCSIVolumeSourcePortals :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1ISCSIVolumeSourceReadOnly :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1ISCSIVolumeSourceSecretRef :: Maybe V1LocalObjectReference
    <*> arbitrary -- v1ISCSIVolumeSourceTargetPortal :: Text
  
instance Arbitrary V1Initializer where
  arbitrary = sized genV1Initializer

genV1Initializer :: Int -> Gen V1Initializer
genV1Initializer n =
  V1Initializer
    <$> arbitrary -- v1InitializerName :: Text
  
instance Arbitrary V1Initializers where
  arbitrary = sized genV1Initializers

genV1Initializers :: Int -> Gen V1Initializers
genV1Initializers n =
  V1Initializers
    <$> arbitraryReduced n -- v1InitializersPending :: [V1Initializer]
    <*> arbitraryReducedMaybe n -- v1InitializersResult :: Maybe V1Status
  
instance Arbitrary V1Job where
  arbitrary = sized genV1Job

genV1Job :: Int -> Gen V1Job
genV1Job n =
  V1Job
    <$> arbitraryReducedMaybe n -- v1JobApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1JobKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1JobMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1JobSpec :: Maybe V1JobSpec
    <*> arbitraryReducedMaybe n -- v1JobStatus :: Maybe V1JobStatus
  
instance Arbitrary V1JobCondition where
  arbitrary = sized genV1JobCondition

genV1JobCondition :: Int -> Gen V1JobCondition
genV1JobCondition n =
  V1JobCondition
    <$> arbitraryReducedMaybe n -- v1JobConditionLastProbeTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1JobConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1JobConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1JobConditionReason :: Maybe Text
    <*> arbitrary -- v1JobConditionStatus :: Text
    <*> arbitrary -- v1JobConditionType :: Text
  
instance Arbitrary V1JobList where
  arbitrary = sized genV1JobList

genV1JobList :: Int -> Gen V1JobList
genV1JobList n =
  V1JobList
    <$> arbitraryReducedMaybe n -- v1JobListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1JobListItems :: [V1Job]
    <*> arbitraryReducedMaybe n -- v1JobListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1JobListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1JobSpec where
  arbitrary = sized genV1JobSpec

genV1JobSpec :: Int -> Gen V1JobSpec
genV1JobSpec n =
  V1JobSpec
    <$> arbitraryReducedMaybe n -- v1JobSpecActiveDeadlineSeconds :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1JobSpecBackoffLimit :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1JobSpecCompletions :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1JobSpecManualSelector :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1JobSpecParallelism :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1JobSpecSelector :: Maybe V1LabelSelector
    <*> arbitraryReduced n -- v1JobSpecTemplate :: V1PodTemplateSpec
  
instance Arbitrary V1JobStatus where
  arbitrary = sized genV1JobStatus

genV1JobStatus :: Int -> Gen V1JobStatus
genV1JobStatus n =
  V1JobStatus
    <$> arbitraryReducedMaybe n -- v1JobStatusActive :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1JobStatusCompletionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1JobStatusConditions :: Maybe [V1JobCondition]
    <*> arbitraryReducedMaybe n -- v1JobStatusFailed :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1JobStatusStartTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1JobStatusSucceeded :: Maybe Int
  
instance Arbitrary V1KeyToPath where
  arbitrary = sized genV1KeyToPath

genV1KeyToPath :: Int -> Gen V1KeyToPath
genV1KeyToPath n =
  V1KeyToPath
    <$> arbitrary -- v1KeyToPathKey :: Text
    <*> arbitraryReducedMaybe n -- v1KeyToPathMode :: Maybe Int
    <*> arbitrary -- v1KeyToPathPath :: Text
  
instance Arbitrary V1LabelSelector where
  arbitrary = sized genV1LabelSelector

genV1LabelSelector :: Int -> Gen V1LabelSelector
genV1LabelSelector n =
  V1LabelSelector
    <$> arbitraryReducedMaybe n -- v1LabelSelectorMatchExpressions :: Maybe [V1LabelSelectorRequirement]
    <*> arbitraryReducedMaybe n -- v1LabelSelectorMatchLabels :: Maybe (Map.Map String Text)
  
instance Arbitrary V1LabelSelectorRequirement where
  arbitrary = sized genV1LabelSelectorRequirement

genV1LabelSelectorRequirement :: Int -> Gen V1LabelSelectorRequirement
genV1LabelSelectorRequirement n =
  V1LabelSelectorRequirement
    <$> arbitrary -- v1LabelSelectorRequirementKey :: Text
    <*> arbitrary -- v1LabelSelectorRequirementOperator :: Text
    <*> arbitraryReducedMaybe n -- v1LabelSelectorRequirementValues :: Maybe [Text]
  
instance Arbitrary V1Lifecycle where
  arbitrary = sized genV1Lifecycle

genV1Lifecycle :: Int -> Gen V1Lifecycle
genV1Lifecycle n =
  V1Lifecycle
    <$> arbitraryReducedMaybe n -- v1LifecyclePostStart :: Maybe V1Handler
    <*> arbitraryReducedMaybe n -- v1LifecyclePreStop :: Maybe V1Handler
  
instance Arbitrary V1LimitRange where
  arbitrary = sized genV1LimitRange

genV1LimitRange :: Int -> Gen V1LimitRange
genV1LimitRange n =
  V1LimitRange
    <$> arbitraryReducedMaybe n -- v1LimitRangeApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1LimitRangeKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1LimitRangeMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1LimitRangeSpec :: Maybe V1LimitRangeSpec
  
instance Arbitrary V1LimitRangeItem where
  arbitrary = sized genV1LimitRangeItem

genV1LimitRangeItem :: Int -> Gen V1LimitRangeItem
genV1LimitRangeItem n =
  V1LimitRangeItem
    <$> arbitraryReducedMaybe n -- v1LimitRangeItemDefault :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1LimitRangeItemDefaultRequest :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1LimitRangeItemMax :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1LimitRangeItemMaxLimitRequestRatio :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1LimitRangeItemMin :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1LimitRangeItemType :: Maybe Text
  
instance Arbitrary V1LimitRangeList where
  arbitrary = sized genV1LimitRangeList

genV1LimitRangeList :: Int -> Gen V1LimitRangeList
genV1LimitRangeList n =
  V1LimitRangeList
    <$> arbitraryReducedMaybe n -- v1LimitRangeListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1LimitRangeListItems :: [V1LimitRange]
    <*> arbitraryReducedMaybe n -- v1LimitRangeListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1LimitRangeListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1LimitRangeSpec where
  arbitrary = sized genV1LimitRangeSpec

genV1LimitRangeSpec :: Int -> Gen V1LimitRangeSpec
genV1LimitRangeSpec n =
  V1LimitRangeSpec
    <$> arbitraryReduced n -- v1LimitRangeSpecLimits :: [V1LimitRangeItem]
  
instance Arbitrary V1ListMeta where
  arbitrary = sized genV1ListMeta

genV1ListMeta :: Int -> Gen V1ListMeta
genV1ListMeta n =
  V1ListMeta
    <$> arbitraryReducedMaybe n -- v1ListMetaContinue :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ListMetaResourceVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ListMetaSelfLink :: Maybe Text
  
instance Arbitrary V1LoadBalancerIngress where
  arbitrary = sized genV1LoadBalancerIngress

genV1LoadBalancerIngress :: Int -> Gen V1LoadBalancerIngress
genV1LoadBalancerIngress n =
  V1LoadBalancerIngress
    <$> arbitraryReducedMaybe n -- v1LoadBalancerIngressHostname :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1LoadBalancerIngressIp :: Maybe Text
  
instance Arbitrary V1LoadBalancerStatus where
  arbitrary = sized genV1LoadBalancerStatus

genV1LoadBalancerStatus :: Int -> Gen V1LoadBalancerStatus
genV1LoadBalancerStatus n =
  V1LoadBalancerStatus
    <$> arbitraryReducedMaybe n -- v1LoadBalancerStatusIngress :: Maybe [V1LoadBalancerIngress]
  
instance Arbitrary V1LocalObjectReference where
  arbitrary = sized genV1LocalObjectReference

genV1LocalObjectReference :: Int -> Gen V1LocalObjectReference
genV1LocalObjectReference n =
  V1LocalObjectReference
    <$> arbitraryReducedMaybe n -- v1LocalObjectReferenceName :: Maybe Text
  
instance Arbitrary V1LocalSubjectAccessReview where
  arbitrary = sized genV1LocalSubjectAccessReview

genV1LocalSubjectAccessReview :: Int -> Gen V1LocalSubjectAccessReview
genV1LocalSubjectAccessReview n =
  V1LocalSubjectAccessReview
    <$> arbitraryReducedMaybe n -- v1LocalSubjectAccessReviewApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1LocalSubjectAccessReviewKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1LocalSubjectAccessReviewMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1LocalSubjectAccessReviewSpec :: V1SubjectAccessReviewSpec
    <*> arbitraryReducedMaybe n -- v1LocalSubjectAccessReviewStatus :: Maybe V1SubjectAccessReviewStatus
  
instance Arbitrary V1LocalVolumeSource where
  arbitrary = sized genV1LocalVolumeSource

genV1LocalVolumeSource :: Int -> Gen V1LocalVolumeSource
genV1LocalVolumeSource n =
  V1LocalVolumeSource
    <$> arbitrary -- v1LocalVolumeSourcePath :: Text
  
instance Arbitrary V1NFSVolumeSource where
  arbitrary = sized genV1NFSVolumeSource

genV1NFSVolumeSource :: Int -> Gen V1NFSVolumeSource
genV1NFSVolumeSource n =
  V1NFSVolumeSource
    <$> arbitrary -- v1NFSVolumeSourcePath :: Text
    <*> arbitraryReducedMaybe n -- v1NFSVolumeSourceReadOnly :: Maybe Bool
    <*> arbitrary -- v1NFSVolumeSourceServer :: Text
  
instance Arbitrary V1Namespace where
  arbitrary = sized genV1Namespace

genV1Namespace :: Int -> Gen V1Namespace
genV1Namespace n =
  V1Namespace
    <$> arbitraryReducedMaybe n -- v1NamespaceApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1NamespaceKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1NamespaceMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1NamespaceSpec :: Maybe V1NamespaceSpec
    <*> arbitraryReducedMaybe n -- v1NamespaceStatus :: Maybe V1NamespaceStatus
  
instance Arbitrary V1NamespaceList where
  arbitrary = sized genV1NamespaceList

genV1NamespaceList :: Int -> Gen V1NamespaceList
genV1NamespaceList n =
  V1NamespaceList
    <$> arbitraryReducedMaybe n -- v1NamespaceListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1NamespaceListItems :: [V1Namespace]
    <*> arbitraryReducedMaybe n -- v1NamespaceListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1NamespaceListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1NamespaceSpec where
  arbitrary = sized genV1NamespaceSpec

genV1NamespaceSpec :: Int -> Gen V1NamespaceSpec
genV1NamespaceSpec n =
  V1NamespaceSpec
    <$> arbitraryReducedMaybe n -- v1NamespaceSpecFinalizers :: Maybe [Text]
  
instance Arbitrary V1NamespaceStatus where
  arbitrary = sized genV1NamespaceStatus

genV1NamespaceStatus :: Int -> Gen V1NamespaceStatus
genV1NamespaceStatus n =
  V1NamespaceStatus
    <$> arbitraryReducedMaybe n -- v1NamespaceStatusPhase :: Maybe Text
  
instance Arbitrary V1NetworkPolicy where
  arbitrary = sized genV1NetworkPolicy

genV1NetworkPolicy :: Int -> Gen V1NetworkPolicy
genV1NetworkPolicy n =
  V1NetworkPolicy
    <$> arbitraryReducedMaybe n -- v1NetworkPolicyApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1NetworkPolicyKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1NetworkPolicyMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1NetworkPolicySpec :: Maybe V1NetworkPolicySpec
  
instance Arbitrary V1NetworkPolicyEgressRule where
  arbitrary = sized genV1NetworkPolicyEgressRule

genV1NetworkPolicyEgressRule :: Int -> Gen V1NetworkPolicyEgressRule
genV1NetworkPolicyEgressRule n =
  V1NetworkPolicyEgressRule
    <$> arbitraryReducedMaybe n -- v1NetworkPolicyEgressRulePorts :: Maybe [V1NetworkPolicyPort]
    <*> arbitraryReducedMaybe n -- v1NetworkPolicyEgressRuleTo :: Maybe [V1NetworkPolicyPeer]
  
instance Arbitrary V1NetworkPolicyIngressRule where
  arbitrary = sized genV1NetworkPolicyIngressRule

genV1NetworkPolicyIngressRule :: Int -> Gen V1NetworkPolicyIngressRule
genV1NetworkPolicyIngressRule n =
  V1NetworkPolicyIngressRule
    <$> arbitraryReducedMaybe n -- v1NetworkPolicyIngressRuleFrom :: Maybe [V1NetworkPolicyPeer]
    <*> arbitraryReducedMaybe n -- v1NetworkPolicyIngressRulePorts :: Maybe [V1NetworkPolicyPort]
  
instance Arbitrary V1NetworkPolicyList where
  arbitrary = sized genV1NetworkPolicyList

genV1NetworkPolicyList :: Int -> Gen V1NetworkPolicyList
genV1NetworkPolicyList n =
  V1NetworkPolicyList
    <$> arbitraryReducedMaybe n -- v1NetworkPolicyListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1NetworkPolicyListItems :: [V1NetworkPolicy]
    <*> arbitraryReducedMaybe n -- v1NetworkPolicyListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1NetworkPolicyListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1NetworkPolicyPeer where
  arbitrary = sized genV1NetworkPolicyPeer

genV1NetworkPolicyPeer :: Int -> Gen V1NetworkPolicyPeer
genV1NetworkPolicyPeer n =
  V1NetworkPolicyPeer
    <$> arbitraryReducedMaybe n -- v1NetworkPolicyPeerIpBlock :: Maybe V1IPBlock
    <*> arbitraryReducedMaybe n -- v1NetworkPolicyPeerNamespaceSelector :: Maybe V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1NetworkPolicyPeerPodSelector :: Maybe V1LabelSelector
  
instance Arbitrary V1NetworkPolicyPort where
  arbitrary = sized genV1NetworkPolicyPort

genV1NetworkPolicyPort :: Int -> Gen V1NetworkPolicyPort
genV1NetworkPolicyPort n =
  V1NetworkPolicyPort
    <$> arbitraryReducedMaybeValue n -- v1NetworkPolicyPortPort :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- v1NetworkPolicyPortProtocol :: Maybe Text
  
instance Arbitrary V1NetworkPolicySpec where
  arbitrary = sized genV1NetworkPolicySpec

genV1NetworkPolicySpec :: Int -> Gen V1NetworkPolicySpec
genV1NetworkPolicySpec n =
  V1NetworkPolicySpec
    <$> arbitraryReducedMaybe n -- v1NetworkPolicySpecEgress :: Maybe [V1NetworkPolicyEgressRule]
    <*> arbitraryReducedMaybe n -- v1NetworkPolicySpecIngress :: Maybe [V1NetworkPolicyIngressRule]
    <*> arbitraryReduced n -- v1NetworkPolicySpecPodSelector :: V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1NetworkPolicySpecPolicyTypes :: Maybe [Text]
  
instance Arbitrary V1Node where
  arbitrary = sized genV1Node

genV1Node :: Int -> Gen V1Node
genV1Node n =
  V1Node
    <$> arbitraryReducedMaybe n -- v1NodeApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1NodeKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1NodeMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1NodeSpec :: Maybe V1NodeSpec
    <*> arbitraryReducedMaybe n -- v1NodeStatus :: Maybe V1NodeStatus
  
instance Arbitrary V1NodeAddress where
  arbitrary = sized genV1NodeAddress

genV1NodeAddress :: Int -> Gen V1NodeAddress
genV1NodeAddress n =
  V1NodeAddress
    <$> arbitrary -- v1NodeAddressAddress :: Text
    <*> arbitrary -- v1NodeAddressType :: Text
  
instance Arbitrary V1NodeAffinity where
  arbitrary = sized genV1NodeAffinity

genV1NodeAffinity :: Int -> Gen V1NodeAffinity
genV1NodeAffinity n =
  V1NodeAffinity
    <$> arbitraryReducedMaybe n -- v1NodeAffinityPreferredDuringSchedulingIgnoredDuringExecution :: Maybe [V1PreferredSchedulingTerm]
    <*> arbitraryReducedMaybe n -- v1NodeAffinityRequiredDuringSchedulingIgnoredDuringExecution :: Maybe V1NodeSelector
  
instance Arbitrary V1NodeCondition where
  arbitrary = sized genV1NodeCondition

genV1NodeCondition :: Int -> Gen V1NodeCondition
genV1NodeCondition n =
  V1NodeCondition
    <$> arbitraryReducedMaybe n -- v1NodeConditionLastHeartbeatTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1NodeConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1NodeConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1NodeConditionReason :: Maybe Text
    <*> arbitrary -- v1NodeConditionStatus :: Text
    <*> arbitrary -- v1NodeConditionType :: Text
  
instance Arbitrary V1NodeConfigSource where
  arbitrary = sized genV1NodeConfigSource

genV1NodeConfigSource :: Int -> Gen V1NodeConfigSource
genV1NodeConfigSource n =
  V1NodeConfigSource
    <$> arbitraryReducedMaybe n -- v1NodeConfigSourceApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1NodeConfigSourceConfigMapRef :: Maybe V1ObjectReference
    <*> arbitraryReducedMaybe n -- v1NodeConfigSourceKind :: Maybe Text
  
instance Arbitrary V1NodeDaemonEndpoints where
  arbitrary = sized genV1NodeDaemonEndpoints

genV1NodeDaemonEndpoints :: Int -> Gen V1NodeDaemonEndpoints
genV1NodeDaemonEndpoints n =
  V1NodeDaemonEndpoints
    <$> arbitraryReducedMaybe n -- v1NodeDaemonEndpointsKubeletEndpoint :: Maybe V1DaemonEndpoint
  
instance Arbitrary V1NodeList where
  arbitrary = sized genV1NodeList

genV1NodeList :: Int -> Gen V1NodeList
genV1NodeList n =
  V1NodeList
    <$> arbitraryReducedMaybe n -- v1NodeListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1NodeListItems :: [V1Node]
    <*> arbitraryReducedMaybe n -- v1NodeListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1NodeListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1NodeSelector where
  arbitrary = sized genV1NodeSelector

genV1NodeSelector :: Int -> Gen V1NodeSelector
genV1NodeSelector n =
  V1NodeSelector
    <$> arbitraryReduced n -- v1NodeSelectorNodeSelectorTerms :: [V1NodeSelectorTerm]
  
instance Arbitrary V1NodeSelectorRequirement where
  arbitrary = sized genV1NodeSelectorRequirement

genV1NodeSelectorRequirement :: Int -> Gen V1NodeSelectorRequirement
genV1NodeSelectorRequirement n =
  V1NodeSelectorRequirement
    <$> arbitrary -- v1NodeSelectorRequirementKey :: Text
    <*> arbitrary -- v1NodeSelectorRequirementOperator :: Text
    <*> arbitraryReducedMaybe n -- v1NodeSelectorRequirementValues :: Maybe [Text]
  
instance Arbitrary V1NodeSelectorTerm where
  arbitrary = sized genV1NodeSelectorTerm

genV1NodeSelectorTerm :: Int -> Gen V1NodeSelectorTerm
genV1NodeSelectorTerm n =
  V1NodeSelectorTerm
    <$> arbitraryReduced n -- v1NodeSelectorTermMatchExpressions :: [V1NodeSelectorRequirement]
  
instance Arbitrary V1NodeSpec where
  arbitrary = sized genV1NodeSpec

genV1NodeSpec :: Int -> Gen V1NodeSpec
genV1NodeSpec n =
  V1NodeSpec
    <$> arbitraryReducedMaybe n -- v1NodeSpecConfigSource :: Maybe V1NodeConfigSource
    <*> arbitraryReducedMaybe n -- v1NodeSpecExternalId :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1NodeSpecPodCidr :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1NodeSpecProviderId :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1NodeSpecTaints :: Maybe [V1Taint]
    <*> arbitraryReducedMaybe n -- v1NodeSpecUnschedulable :: Maybe Bool
  
instance Arbitrary V1NodeStatus where
  arbitrary = sized genV1NodeStatus

genV1NodeStatus :: Int -> Gen V1NodeStatus
genV1NodeStatus n =
  V1NodeStatus
    <$> arbitraryReducedMaybe n -- v1NodeStatusAddresses :: Maybe [V1NodeAddress]
    <*> arbitraryReducedMaybe n -- v1NodeStatusAllocatable :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1NodeStatusCapacity :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1NodeStatusConditions :: Maybe [V1NodeCondition]
    <*> arbitraryReducedMaybe n -- v1NodeStatusDaemonEndpoints :: Maybe V1NodeDaemonEndpoints
    <*> arbitraryReducedMaybe n -- v1NodeStatusImages :: Maybe [V1ContainerImage]
    <*> arbitraryReducedMaybe n -- v1NodeStatusNodeInfo :: Maybe V1NodeSystemInfo
    <*> arbitraryReducedMaybe n -- v1NodeStatusPhase :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1NodeStatusVolumesAttached :: Maybe [V1AttachedVolume]
    <*> arbitraryReducedMaybe n -- v1NodeStatusVolumesInUse :: Maybe [Text]
  
instance Arbitrary V1NodeSystemInfo where
  arbitrary = sized genV1NodeSystemInfo

genV1NodeSystemInfo :: Int -> Gen V1NodeSystemInfo
genV1NodeSystemInfo n =
  V1NodeSystemInfo
    <$> arbitrary -- v1NodeSystemInfoArchitecture :: Text
    <*> arbitrary -- v1NodeSystemInfoBootId :: Text
    <*> arbitrary -- v1NodeSystemInfoContainerRuntimeVersion :: Text
    <*> arbitrary -- v1NodeSystemInfoKernelVersion :: Text
    <*> arbitrary -- v1NodeSystemInfoKubeProxyVersion :: Text
    <*> arbitrary -- v1NodeSystemInfoKubeletVersion :: Text
    <*> arbitrary -- v1NodeSystemInfoMachineId :: Text
    <*> arbitrary -- v1NodeSystemInfoOperatingSystem :: Text
    <*> arbitrary -- v1NodeSystemInfoOsImage :: Text
    <*> arbitrary -- v1NodeSystemInfoSystemUuid :: Text
  
instance Arbitrary V1NonResourceAttributes where
  arbitrary = sized genV1NonResourceAttributes

genV1NonResourceAttributes :: Int -> Gen V1NonResourceAttributes
genV1NonResourceAttributes n =
  V1NonResourceAttributes
    <$> arbitraryReducedMaybe n -- v1NonResourceAttributesPath :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1NonResourceAttributesVerb :: Maybe Text
  
instance Arbitrary V1NonResourceRule where
  arbitrary = sized genV1NonResourceRule

genV1NonResourceRule :: Int -> Gen V1NonResourceRule
genV1NonResourceRule n =
  V1NonResourceRule
    <$> arbitraryReducedMaybe n -- v1NonResourceRuleNonResourceUrLs :: Maybe [Text]
    <*> arbitrary -- v1NonResourceRuleVerbs :: [Text]
  
instance Arbitrary V1ObjectFieldSelector where
  arbitrary = sized genV1ObjectFieldSelector

genV1ObjectFieldSelector :: Int -> Gen V1ObjectFieldSelector
genV1ObjectFieldSelector n =
  V1ObjectFieldSelector
    <$> arbitraryReducedMaybe n -- v1ObjectFieldSelectorApiVersion :: Maybe Text
    <*> arbitrary -- v1ObjectFieldSelectorFieldPath :: Text
  
instance Arbitrary V1ObjectMeta where
  arbitrary = sized genV1ObjectMeta

genV1ObjectMeta :: Int -> Gen V1ObjectMeta
genV1ObjectMeta n =
  V1ObjectMeta
    <$> arbitraryReducedMaybe n -- v1ObjectMetaAnnotations :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1ObjectMetaClusterName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ObjectMetaCreationTimestamp :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1ObjectMetaDeletionGracePeriodSeconds :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1ObjectMetaDeletionTimestamp :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1ObjectMetaFinalizers :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1ObjectMetaGenerateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ObjectMetaGeneration :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1ObjectMetaInitializers :: Maybe V1Initializers
    <*> arbitraryReducedMaybe n -- v1ObjectMetaLabels :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1ObjectMetaName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ObjectMetaNamespace :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ObjectMetaOwnerReferences :: Maybe [V1OwnerReference]
    <*> arbitraryReducedMaybe n -- v1ObjectMetaResourceVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ObjectMetaSelfLink :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ObjectMetaUid :: Maybe Text
  
instance Arbitrary V1ObjectReference where
  arbitrary = sized genV1ObjectReference

genV1ObjectReference :: Int -> Gen V1ObjectReference
genV1ObjectReference n =
  V1ObjectReference
    <$> arbitraryReducedMaybe n -- v1ObjectReferenceApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ObjectReferenceFieldPath :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ObjectReferenceKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ObjectReferenceName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ObjectReferenceNamespace :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ObjectReferenceResourceVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ObjectReferenceUid :: Maybe Text
  
instance Arbitrary V1OwnerReference where
  arbitrary = sized genV1OwnerReference

genV1OwnerReference :: Int -> Gen V1OwnerReference
genV1OwnerReference n =
  V1OwnerReference
    <$> arbitrary -- v1OwnerReferenceApiVersion :: Text
    <*> arbitraryReducedMaybe n -- v1OwnerReferenceBlockOwnerDeletion :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1OwnerReferenceController :: Maybe Bool
    <*> arbitrary -- v1OwnerReferenceKind :: Text
    <*> arbitrary -- v1OwnerReferenceName :: Text
    <*> arbitrary -- v1OwnerReferenceUid :: Text
  
instance Arbitrary V1PersistentVolume where
  arbitrary = sized genV1PersistentVolume

genV1PersistentVolume :: Int -> Gen V1PersistentVolume
genV1PersistentVolume n =
  V1PersistentVolume
    <$> arbitraryReducedMaybe n -- v1PersistentVolumeApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpec :: Maybe V1PersistentVolumeSpec
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeStatus :: Maybe V1PersistentVolumeStatus
  
instance Arbitrary V1PersistentVolumeClaim where
  arbitrary = sized genV1PersistentVolumeClaim

genV1PersistentVolumeClaim :: Int -> Gen V1PersistentVolumeClaim
genV1PersistentVolumeClaim n =
  V1PersistentVolumeClaim
    <$> arbitraryReducedMaybe n -- v1PersistentVolumeClaimApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimSpec :: Maybe V1PersistentVolumeClaimSpec
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimStatus :: Maybe V1PersistentVolumeClaimStatus
  
instance Arbitrary V1PersistentVolumeClaimCondition where
  arbitrary = sized genV1PersistentVolumeClaimCondition

genV1PersistentVolumeClaimCondition :: Int -> Gen V1PersistentVolumeClaimCondition
genV1PersistentVolumeClaimCondition n =
  V1PersistentVolumeClaimCondition
    <$> arbitraryReducedMaybe n -- v1PersistentVolumeClaimConditionLastProbeTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimConditionReason :: Maybe Text
    <*> arbitrary -- v1PersistentVolumeClaimConditionStatus :: Text
    <*> arbitrary -- v1PersistentVolumeClaimConditionType :: Text
  
instance Arbitrary V1PersistentVolumeClaimList where
  arbitrary = sized genV1PersistentVolumeClaimList

genV1PersistentVolumeClaimList :: Int -> Gen V1PersistentVolumeClaimList
genV1PersistentVolumeClaimList n =
  V1PersistentVolumeClaimList
    <$> arbitraryReducedMaybe n -- v1PersistentVolumeClaimListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1PersistentVolumeClaimListItems :: [V1PersistentVolumeClaim]
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1PersistentVolumeClaimSpec where
  arbitrary = sized genV1PersistentVolumeClaimSpec

genV1PersistentVolumeClaimSpec :: Int -> Gen V1PersistentVolumeClaimSpec
genV1PersistentVolumeClaimSpec n =
  V1PersistentVolumeClaimSpec
    <$> arbitraryReducedMaybe n -- v1PersistentVolumeClaimSpecAccessModes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimSpecResources :: Maybe V1ResourceRequirements
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimSpecSelector :: Maybe V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimSpecStorageClassName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimSpecVolumeMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimSpecVolumeName :: Maybe Text
  
instance Arbitrary V1PersistentVolumeClaimStatus where
  arbitrary = sized genV1PersistentVolumeClaimStatus

genV1PersistentVolumeClaimStatus :: Int -> Gen V1PersistentVolumeClaimStatus
genV1PersistentVolumeClaimStatus n =
  V1PersistentVolumeClaimStatus
    <$> arbitraryReducedMaybe n -- v1PersistentVolumeClaimStatusAccessModes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimStatusCapacity :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimStatusConditions :: Maybe [V1PersistentVolumeClaimCondition]
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimStatusPhase :: Maybe Text
  
instance Arbitrary V1PersistentVolumeClaimVolumeSource where
  arbitrary = sized genV1PersistentVolumeClaimVolumeSource

genV1PersistentVolumeClaimVolumeSource :: Int -> Gen V1PersistentVolumeClaimVolumeSource
genV1PersistentVolumeClaimVolumeSource n =
  V1PersistentVolumeClaimVolumeSource
    <$> arbitrary -- v1PersistentVolumeClaimVolumeSourceClaimName :: Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimVolumeSourceReadOnly :: Maybe Bool
  
instance Arbitrary V1PersistentVolumeList where
  arbitrary = sized genV1PersistentVolumeList

genV1PersistentVolumeList :: Int -> Gen V1PersistentVolumeList
genV1PersistentVolumeList n =
  V1PersistentVolumeList
    <$> arbitraryReducedMaybe n -- v1PersistentVolumeListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1PersistentVolumeListItems :: [V1PersistentVolume]
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1PersistentVolumeSpec where
  arbitrary = sized genV1PersistentVolumeSpec

genV1PersistentVolumeSpec :: Int -> Gen V1PersistentVolumeSpec
genV1PersistentVolumeSpec n =
  V1PersistentVolumeSpec
    <$> arbitraryReducedMaybe n -- v1PersistentVolumeSpecAccessModes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecAwsElasticBlockStore :: Maybe V1AWSElasticBlockStoreVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecAzureDisk :: Maybe V1AzureDiskVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecAzureFile :: Maybe V1AzureFilePersistentVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecCapacity :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecCephfs :: Maybe V1CephFSPersistentVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecCinder :: Maybe V1CinderVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecClaimRef :: Maybe V1ObjectReference
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecCsi :: Maybe V1CSIPersistentVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecFc :: Maybe V1FCVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecFlexVolume :: Maybe V1FlexVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecFlocker :: Maybe V1FlockerVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecGcePersistentDisk :: Maybe V1GCEPersistentDiskVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecGlusterfs :: Maybe V1GlusterfsVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecHostPath :: Maybe V1HostPathVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecIscsi :: Maybe V1ISCSIPersistentVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecLocal :: Maybe V1LocalVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecMountOptions :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecNfs :: Maybe V1NFSVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecPersistentVolumeReclaimPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecPhotonPersistentDisk :: Maybe V1PhotonPersistentDiskVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecPortworxVolume :: Maybe V1PortworxVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecQuobyte :: Maybe V1QuobyteVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecRbd :: Maybe V1RBDPersistentVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecScaleIo :: Maybe V1ScaleIOPersistentVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecStorageClassName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecStorageos :: Maybe V1StorageOSPersistentVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecVolumeMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecVsphereVolume :: Maybe V1VsphereVirtualDiskVolumeSource
  
instance Arbitrary V1PersistentVolumeStatus where
  arbitrary = sized genV1PersistentVolumeStatus

genV1PersistentVolumeStatus :: Int -> Gen V1PersistentVolumeStatus
genV1PersistentVolumeStatus n =
  V1PersistentVolumeStatus
    <$> arbitraryReducedMaybe n -- v1PersistentVolumeStatusMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeStatusPhase :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeStatusReason :: Maybe Text
  
instance Arbitrary V1PhotonPersistentDiskVolumeSource where
  arbitrary = sized genV1PhotonPersistentDiskVolumeSource

genV1PhotonPersistentDiskVolumeSource :: Int -> Gen V1PhotonPersistentDiskVolumeSource
genV1PhotonPersistentDiskVolumeSource n =
  V1PhotonPersistentDiskVolumeSource
    <$> arbitraryReducedMaybe n -- v1PhotonPersistentDiskVolumeSourceFsType :: Maybe Text
    <*> arbitrary -- v1PhotonPersistentDiskVolumeSourcePdId :: Text
  
instance Arbitrary V1Pod where
  arbitrary = sized genV1Pod

genV1Pod :: Int -> Gen V1Pod
genV1Pod n =
  V1Pod
    <$> arbitraryReducedMaybe n -- v1PodApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1PodSpec :: Maybe V1PodSpec
    <*> arbitraryReducedMaybe n -- v1PodStatus :: Maybe V1PodStatus
  
instance Arbitrary V1PodAffinity where
  arbitrary = sized genV1PodAffinity

genV1PodAffinity :: Int -> Gen V1PodAffinity
genV1PodAffinity n =
  V1PodAffinity
    <$> arbitraryReducedMaybe n -- v1PodAffinityPreferredDuringSchedulingIgnoredDuringExecution :: Maybe [V1WeightedPodAffinityTerm]
    <*> arbitraryReducedMaybe n -- v1PodAffinityRequiredDuringSchedulingIgnoredDuringExecution :: Maybe [V1PodAffinityTerm]
  
instance Arbitrary V1PodAffinityTerm where
  arbitrary = sized genV1PodAffinityTerm

genV1PodAffinityTerm :: Int -> Gen V1PodAffinityTerm
genV1PodAffinityTerm n =
  V1PodAffinityTerm
    <$> arbitraryReducedMaybe n -- v1PodAffinityTermLabelSelector :: Maybe V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1PodAffinityTermNamespaces :: Maybe [Text]
    <*> arbitrary -- v1PodAffinityTermTopologyKey :: Text
  
instance Arbitrary V1PodAntiAffinity where
  arbitrary = sized genV1PodAntiAffinity

genV1PodAntiAffinity :: Int -> Gen V1PodAntiAffinity
genV1PodAntiAffinity n =
  V1PodAntiAffinity
    <$> arbitraryReducedMaybe n -- v1PodAntiAffinityPreferredDuringSchedulingIgnoredDuringExecution :: Maybe [V1WeightedPodAffinityTerm]
    <*> arbitraryReducedMaybe n -- v1PodAntiAffinityRequiredDuringSchedulingIgnoredDuringExecution :: Maybe [V1PodAffinityTerm]
  
instance Arbitrary V1PodCondition where
  arbitrary = sized genV1PodCondition

genV1PodCondition :: Int -> Gen V1PodCondition
genV1PodCondition n =
  V1PodCondition
    <$> arbitraryReducedMaybe n -- v1PodConditionLastProbeTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1PodConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1PodConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodConditionReason :: Maybe Text
    <*> arbitrary -- v1PodConditionStatus :: Text
    <*> arbitrary -- v1PodConditionType :: Text
  
instance Arbitrary V1PodDNSConfig where
  arbitrary = sized genV1PodDNSConfig

genV1PodDNSConfig :: Int -> Gen V1PodDNSConfig
genV1PodDNSConfig n =
  V1PodDNSConfig
    <$> arbitraryReducedMaybe n -- v1PodDNSConfigNameservers :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1PodDNSConfigOptions :: Maybe [V1PodDNSConfigOption]
    <*> arbitraryReducedMaybe n -- v1PodDNSConfigSearches :: Maybe [Text]
  
instance Arbitrary V1PodDNSConfigOption where
  arbitrary = sized genV1PodDNSConfigOption

genV1PodDNSConfigOption :: Int -> Gen V1PodDNSConfigOption
genV1PodDNSConfigOption n =
  V1PodDNSConfigOption
    <$> arbitraryReducedMaybe n -- v1PodDNSConfigOptionName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodDNSConfigOptionValue :: Maybe Text
  
instance Arbitrary V1PodList where
  arbitrary = sized genV1PodList

genV1PodList :: Int -> Gen V1PodList
genV1PodList n =
  V1PodList
    <$> arbitraryReducedMaybe n -- v1PodListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1PodListItems :: [V1Pod]
    <*> arbitraryReducedMaybe n -- v1PodListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1PodSecurityContext where
  arbitrary = sized genV1PodSecurityContext

genV1PodSecurityContext :: Int -> Gen V1PodSecurityContext
genV1PodSecurityContext n =
  V1PodSecurityContext
    <$> arbitraryReducedMaybe n -- v1PodSecurityContextFsGroup :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1PodSecurityContextRunAsNonRoot :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1PodSecurityContextRunAsUser :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1PodSecurityContextSeLinuxOptions :: Maybe V1SELinuxOptions
    <*> arbitraryReducedMaybe n -- v1PodSecurityContextSupplementalGroups :: Maybe [Integer]
  
instance Arbitrary V1PodSpec where
  arbitrary = sized genV1PodSpec

genV1PodSpec :: Int -> Gen V1PodSpec
genV1PodSpec n =
  V1PodSpec
    <$> arbitraryReducedMaybe n -- v1PodSpecActiveDeadlineSeconds :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1PodSpecAffinity :: Maybe V1Affinity
    <*> arbitraryReducedMaybe n -- v1PodSpecAutomountServiceAccountToken :: Maybe Bool
    <*> arbitraryReduced n -- v1PodSpecContainers :: [V1Container]
    <*> arbitraryReducedMaybe n -- v1PodSpecDnsConfig :: Maybe V1PodDNSConfig
    <*> arbitraryReducedMaybe n -- v1PodSpecDnsPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodSpecHostAliases :: Maybe [V1HostAlias]
    <*> arbitraryReducedMaybe n -- v1PodSpecHostIpc :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1PodSpecHostNetwork :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1PodSpecHostPid :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1PodSpecHostname :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodSpecImagePullSecrets :: Maybe [V1LocalObjectReference]
    <*> arbitraryReducedMaybe n -- v1PodSpecInitContainers :: Maybe [V1Container]
    <*> arbitraryReducedMaybe n -- v1PodSpecNodeName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodSpecNodeSelector :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1PodSpecPriority :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1PodSpecPriorityClassName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodSpecRestartPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodSpecSchedulerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodSpecSecurityContext :: Maybe V1PodSecurityContext
    <*> arbitraryReducedMaybe n -- v1PodSpecServiceAccount :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodSpecServiceAccountName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodSpecSubdomain :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodSpecTerminationGracePeriodSeconds :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1PodSpecTolerations :: Maybe [V1Toleration]
    <*> arbitraryReducedMaybe n -- v1PodSpecVolumes :: Maybe [V1Volume]
  
instance Arbitrary V1PodStatus where
  arbitrary = sized genV1PodStatus

genV1PodStatus :: Int -> Gen V1PodStatus
genV1PodStatus n =
  V1PodStatus
    <$> arbitraryReducedMaybe n -- v1PodStatusConditions :: Maybe [V1PodCondition]
    <*> arbitraryReducedMaybe n -- v1PodStatusContainerStatuses :: Maybe [V1ContainerStatus]
    <*> arbitraryReducedMaybe n -- v1PodStatusHostIp :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodStatusInitContainerStatuses :: Maybe [V1ContainerStatus]
    <*> arbitraryReducedMaybe n -- v1PodStatusMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodStatusPhase :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodStatusPodIp :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodStatusQosClass :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodStatusReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodStatusStartTime :: Maybe DateTime
  
instance Arbitrary V1PodTemplate where
  arbitrary = sized genV1PodTemplate

genV1PodTemplate :: Int -> Gen V1PodTemplate
genV1PodTemplate n =
  V1PodTemplate
    <$> arbitraryReducedMaybe n -- v1PodTemplateApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodTemplateKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodTemplateMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1PodTemplateTemplate :: Maybe V1PodTemplateSpec
  
instance Arbitrary V1PodTemplateList where
  arbitrary = sized genV1PodTemplateList

genV1PodTemplateList :: Int -> Gen V1PodTemplateList
genV1PodTemplateList n =
  V1PodTemplateList
    <$> arbitraryReducedMaybe n -- v1PodTemplateListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1PodTemplateListItems :: [V1PodTemplate]
    <*> arbitraryReducedMaybe n -- v1PodTemplateListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodTemplateListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1PodTemplateSpec where
  arbitrary = sized genV1PodTemplateSpec

genV1PodTemplateSpec :: Int -> Gen V1PodTemplateSpec
genV1PodTemplateSpec n =
  V1PodTemplateSpec
    <$> arbitraryReducedMaybe n -- v1PodTemplateSpecMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1PodTemplateSpecSpec :: Maybe V1PodSpec
  
instance Arbitrary V1PolicyRule where
  arbitrary = sized genV1PolicyRule

genV1PolicyRule :: Int -> Gen V1PolicyRule
genV1PolicyRule n =
  V1PolicyRule
    <$> arbitraryReducedMaybe n -- v1PolicyRuleApiGroups :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1PolicyRuleNonResourceUrLs :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1PolicyRuleResourceNames :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1PolicyRuleResources :: Maybe [Text]
    <*> arbitrary -- v1PolicyRuleVerbs :: [Text]
  
instance Arbitrary V1PortworxVolumeSource where
  arbitrary = sized genV1PortworxVolumeSource

genV1PortworxVolumeSource :: Int -> Gen V1PortworxVolumeSource
genV1PortworxVolumeSource n =
  V1PortworxVolumeSource
    <$> arbitraryReducedMaybe n -- v1PortworxVolumeSourceFsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PortworxVolumeSourceReadOnly :: Maybe Bool
    <*> arbitrary -- v1PortworxVolumeSourceVolumeId :: Text
  
instance Arbitrary V1Preconditions where
  arbitrary = sized genV1Preconditions

genV1Preconditions :: Int -> Gen V1Preconditions
genV1Preconditions n =
  V1Preconditions
    <$> arbitraryReducedMaybe n -- v1PreconditionsUid :: Maybe Text
  
instance Arbitrary V1PreferredSchedulingTerm where
  arbitrary = sized genV1PreferredSchedulingTerm

genV1PreferredSchedulingTerm :: Int -> Gen V1PreferredSchedulingTerm
genV1PreferredSchedulingTerm n =
  V1PreferredSchedulingTerm
    <$> arbitraryReduced n -- v1PreferredSchedulingTermPreference :: V1NodeSelectorTerm
    <*> arbitrary -- v1PreferredSchedulingTermWeight :: Int
  
instance Arbitrary V1Probe where
  arbitrary = sized genV1Probe

genV1Probe :: Int -> Gen V1Probe
genV1Probe n =
  V1Probe
    <$> arbitraryReducedMaybe n -- v1ProbeExec :: Maybe V1ExecAction
    <*> arbitraryReducedMaybe n -- v1ProbeFailureThreshold :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1ProbeHttpGet :: Maybe V1HTTPGetAction
    <*> arbitraryReducedMaybe n -- v1ProbeInitialDelaySeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1ProbePeriodSeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1ProbeSuccessThreshold :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1ProbeTcpSocket :: Maybe V1TCPSocketAction
    <*> arbitraryReducedMaybe n -- v1ProbeTimeoutSeconds :: Maybe Int
  
instance Arbitrary V1ProjectedVolumeSource where
  arbitrary = sized genV1ProjectedVolumeSource

genV1ProjectedVolumeSource :: Int -> Gen V1ProjectedVolumeSource
genV1ProjectedVolumeSource n =
  V1ProjectedVolumeSource
    <$> arbitraryReducedMaybe n -- v1ProjectedVolumeSourceDefaultMode :: Maybe Int
    <*> arbitraryReduced n -- v1ProjectedVolumeSourceSources :: [V1VolumeProjection]
  
instance Arbitrary V1QuobyteVolumeSource where
  arbitrary = sized genV1QuobyteVolumeSource

genV1QuobyteVolumeSource :: Int -> Gen V1QuobyteVolumeSource
genV1QuobyteVolumeSource n =
  V1QuobyteVolumeSource
    <$> arbitraryReducedMaybe n -- v1QuobyteVolumeSourceGroup :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1QuobyteVolumeSourceReadOnly :: Maybe Bool
    <*> arbitrary -- v1QuobyteVolumeSourceRegistry :: Text
    <*> arbitraryReducedMaybe n -- v1QuobyteVolumeSourceUser :: Maybe Text
    <*> arbitrary -- v1QuobyteVolumeSourceVolume :: Text
  
instance Arbitrary V1RBDPersistentVolumeSource where
  arbitrary = sized genV1RBDPersistentVolumeSource

genV1RBDPersistentVolumeSource :: Int -> Gen V1RBDPersistentVolumeSource
genV1RBDPersistentVolumeSource n =
  V1RBDPersistentVolumeSource
    <$> arbitraryReducedMaybe n -- v1RBDPersistentVolumeSourceFsType :: Maybe Text
    <*> arbitrary -- v1RBDPersistentVolumeSourceImage :: Text
    <*> arbitraryReducedMaybe n -- v1RBDPersistentVolumeSourceKeyring :: Maybe Text
    <*> arbitrary -- v1RBDPersistentVolumeSourceMonitors :: [Text]
    <*> arbitraryReducedMaybe n -- v1RBDPersistentVolumeSourcePool :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1RBDPersistentVolumeSourceReadOnly :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1RBDPersistentVolumeSourceSecretRef :: Maybe V1SecretReference
    <*> arbitraryReducedMaybe n -- v1RBDPersistentVolumeSourceUser :: Maybe Text
  
instance Arbitrary V1RBDVolumeSource where
  arbitrary = sized genV1RBDVolumeSource

genV1RBDVolumeSource :: Int -> Gen V1RBDVolumeSource
genV1RBDVolumeSource n =
  V1RBDVolumeSource
    <$> arbitraryReducedMaybe n -- v1RBDVolumeSourceFsType :: Maybe Text
    <*> arbitrary -- v1RBDVolumeSourceImage :: Text
    <*> arbitraryReducedMaybe n -- v1RBDVolumeSourceKeyring :: Maybe Text
    <*> arbitrary -- v1RBDVolumeSourceMonitors :: [Text]
    <*> arbitraryReducedMaybe n -- v1RBDVolumeSourcePool :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1RBDVolumeSourceReadOnly :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1RBDVolumeSourceSecretRef :: Maybe V1LocalObjectReference
    <*> arbitraryReducedMaybe n -- v1RBDVolumeSourceUser :: Maybe Text
  
instance Arbitrary V1ReplicaSet where
  arbitrary = sized genV1ReplicaSet

genV1ReplicaSet :: Int -> Gen V1ReplicaSet
genV1ReplicaSet n =
  V1ReplicaSet
    <$> arbitraryReducedMaybe n -- v1ReplicaSetApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ReplicaSetKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ReplicaSetMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1ReplicaSetSpec :: Maybe V1ReplicaSetSpec
    <*> arbitraryReducedMaybe n -- v1ReplicaSetStatus :: Maybe V1ReplicaSetStatus
  
instance Arbitrary V1ReplicaSetCondition where
  arbitrary = sized genV1ReplicaSetCondition

genV1ReplicaSetCondition :: Int -> Gen V1ReplicaSetCondition
genV1ReplicaSetCondition n =
  V1ReplicaSetCondition
    <$> arbitraryReducedMaybe n -- v1ReplicaSetConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1ReplicaSetConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ReplicaSetConditionReason :: Maybe Text
    <*> arbitrary -- v1ReplicaSetConditionStatus :: Text
    <*> arbitrary -- v1ReplicaSetConditionType :: Text
  
instance Arbitrary V1ReplicaSetList where
  arbitrary = sized genV1ReplicaSetList

genV1ReplicaSetList :: Int -> Gen V1ReplicaSetList
genV1ReplicaSetList n =
  V1ReplicaSetList
    <$> arbitraryReducedMaybe n -- v1ReplicaSetListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1ReplicaSetListItems :: [V1ReplicaSet]
    <*> arbitraryReducedMaybe n -- v1ReplicaSetListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ReplicaSetListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1ReplicaSetSpec where
  arbitrary = sized genV1ReplicaSetSpec

genV1ReplicaSetSpec :: Int -> Gen V1ReplicaSetSpec
genV1ReplicaSetSpec n =
  V1ReplicaSetSpec
    <$> arbitraryReducedMaybe n -- v1ReplicaSetSpecMinReadySeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1ReplicaSetSpecReplicas :: Maybe Int
    <*> arbitraryReduced n -- v1ReplicaSetSpecSelector :: V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1ReplicaSetSpecTemplate :: Maybe V1PodTemplateSpec
  
instance Arbitrary V1ReplicaSetStatus where
  arbitrary = sized genV1ReplicaSetStatus

genV1ReplicaSetStatus :: Int -> Gen V1ReplicaSetStatus
genV1ReplicaSetStatus n =
  V1ReplicaSetStatus
    <$> arbitraryReducedMaybe n -- v1ReplicaSetStatusAvailableReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1ReplicaSetStatusConditions :: Maybe [V1ReplicaSetCondition]
    <*> arbitraryReducedMaybe n -- v1ReplicaSetStatusFullyLabeledReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1ReplicaSetStatusObservedGeneration :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1ReplicaSetStatusReadyReplicas :: Maybe Int
    <*> arbitrary -- v1ReplicaSetStatusReplicas :: Int
  
instance Arbitrary V1ReplicationController where
  arbitrary = sized genV1ReplicationController

genV1ReplicationController :: Int -> Gen V1ReplicationController
genV1ReplicationController n =
  V1ReplicationController
    <$> arbitraryReducedMaybe n -- v1ReplicationControllerApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ReplicationControllerKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ReplicationControllerMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1ReplicationControllerSpec :: Maybe V1ReplicationControllerSpec
    <*> arbitraryReducedMaybe n -- v1ReplicationControllerStatus :: Maybe V1ReplicationControllerStatus
  
instance Arbitrary V1ReplicationControllerCondition where
  arbitrary = sized genV1ReplicationControllerCondition

genV1ReplicationControllerCondition :: Int -> Gen V1ReplicationControllerCondition
genV1ReplicationControllerCondition n =
  V1ReplicationControllerCondition
    <$> arbitraryReducedMaybe n -- v1ReplicationControllerConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1ReplicationControllerConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ReplicationControllerConditionReason :: Maybe Text
    <*> arbitrary -- v1ReplicationControllerConditionStatus :: Text
    <*> arbitrary -- v1ReplicationControllerConditionType :: Text
  
instance Arbitrary V1ReplicationControllerList where
  arbitrary = sized genV1ReplicationControllerList

genV1ReplicationControllerList :: Int -> Gen V1ReplicationControllerList
genV1ReplicationControllerList n =
  V1ReplicationControllerList
    <$> arbitraryReducedMaybe n -- v1ReplicationControllerListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1ReplicationControllerListItems :: [V1ReplicationController]
    <*> arbitraryReducedMaybe n -- v1ReplicationControllerListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ReplicationControllerListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1ReplicationControllerSpec where
  arbitrary = sized genV1ReplicationControllerSpec

genV1ReplicationControllerSpec :: Int -> Gen V1ReplicationControllerSpec
genV1ReplicationControllerSpec n =
  V1ReplicationControllerSpec
    <$> arbitraryReducedMaybe n -- v1ReplicationControllerSpecMinReadySeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1ReplicationControllerSpecReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1ReplicationControllerSpecSelector :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1ReplicationControllerSpecTemplate :: Maybe V1PodTemplateSpec
  
instance Arbitrary V1ReplicationControllerStatus where
  arbitrary = sized genV1ReplicationControllerStatus

genV1ReplicationControllerStatus :: Int -> Gen V1ReplicationControllerStatus
genV1ReplicationControllerStatus n =
  V1ReplicationControllerStatus
    <$> arbitraryReducedMaybe n -- v1ReplicationControllerStatusAvailableReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1ReplicationControllerStatusConditions :: Maybe [V1ReplicationControllerCondition]
    <*> arbitraryReducedMaybe n -- v1ReplicationControllerStatusFullyLabeledReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1ReplicationControllerStatusObservedGeneration :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1ReplicationControllerStatusReadyReplicas :: Maybe Int
    <*> arbitrary -- v1ReplicationControllerStatusReplicas :: Int
  
instance Arbitrary V1ResourceAttributes where
  arbitrary = sized genV1ResourceAttributes

genV1ResourceAttributes :: Int -> Gen V1ResourceAttributes
genV1ResourceAttributes n =
  V1ResourceAttributes
    <$> arbitraryReducedMaybe n -- v1ResourceAttributesGroup :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ResourceAttributesName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ResourceAttributesNamespace :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ResourceAttributesResource :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ResourceAttributesSubresource :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ResourceAttributesVerb :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ResourceAttributesVersion :: Maybe Text
  
instance Arbitrary V1ResourceFieldSelector where
  arbitrary = sized genV1ResourceFieldSelector

genV1ResourceFieldSelector :: Int -> Gen V1ResourceFieldSelector
genV1ResourceFieldSelector n =
  V1ResourceFieldSelector
    <$> arbitraryReducedMaybe n -- v1ResourceFieldSelectorContainerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ResourceFieldSelectorDivisor :: Maybe Text
    <*> arbitrary -- v1ResourceFieldSelectorResource :: Text
  
instance Arbitrary V1ResourceQuota where
  arbitrary = sized genV1ResourceQuota

genV1ResourceQuota :: Int -> Gen V1ResourceQuota
genV1ResourceQuota n =
  V1ResourceQuota
    <$> arbitraryReducedMaybe n -- v1ResourceQuotaApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ResourceQuotaKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ResourceQuotaMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1ResourceQuotaSpec :: Maybe V1ResourceQuotaSpec
    <*> arbitraryReducedMaybe n -- v1ResourceQuotaStatus :: Maybe V1ResourceQuotaStatus
  
instance Arbitrary V1ResourceQuotaList where
  arbitrary = sized genV1ResourceQuotaList

genV1ResourceQuotaList :: Int -> Gen V1ResourceQuotaList
genV1ResourceQuotaList n =
  V1ResourceQuotaList
    <$> arbitraryReducedMaybe n -- v1ResourceQuotaListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1ResourceQuotaListItems :: [V1ResourceQuota]
    <*> arbitraryReducedMaybe n -- v1ResourceQuotaListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ResourceQuotaListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1ResourceQuotaSpec where
  arbitrary = sized genV1ResourceQuotaSpec

genV1ResourceQuotaSpec :: Int -> Gen V1ResourceQuotaSpec
genV1ResourceQuotaSpec n =
  V1ResourceQuotaSpec
    <$> arbitraryReducedMaybe n -- v1ResourceQuotaSpecHard :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1ResourceQuotaSpecScopes :: Maybe [Text]
  
instance Arbitrary V1ResourceQuotaStatus where
  arbitrary = sized genV1ResourceQuotaStatus

genV1ResourceQuotaStatus :: Int -> Gen V1ResourceQuotaStatus
genV1ResourceQuotaStatus n =
  V1ResourceQuotaStatus
    <$> arbitraryReducedMaybe n -- v1ResourceQuotaStatusHard :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1ResourceQuotaStatusUsed :: Maybe (Map.Map String Text)
  
instance Arbitrary V1ResourceRequirements where
  arbitrary = sized genV1ResourceRequirements

genV1ResourceRequirements :: Int -> Gen V1ResourceRequirements
genV1ResourceRequirements n =
  V1ResourceRequirements
    <$> arbitraryReducedMaybe n -- v1ResourceRequirementsLimits :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1ResourceRequirementsRequests :: Maybe (Map.Map String Text)
  
instance Arbitrary V1ResourceRule where
  arbitrary = sized genV1ResourceRule

genV1ResourceRule :: Int -> Gen V1ResourceRule
genV1ResourceRule n =
  V1ResourceRule
    <$> arbitraryReducedMaybe n -- v1ResourceRuleApiGroups :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1ResourceRuleResourceNames :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1ResourceRuleResources :: Maybe [Text]
    <*> arbitrary -- v1ResourceRuleVerbs :: [Text]
  
instance Arbitrary V1Role where
  arbitrary = sized genV1Role

genV1Role :: Int -> Gen V1Role
genV1Role n =
  V1Role
    <$> arbitraryReducedMaybe n -- v1RoleApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1RoleKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1RoleMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1RoleRules :: [V1PolicyRule]
  
instance Arbitrary V1RoleBinding where
  arbitrary = sized genV1RoleBinding

genV1RoleBinding :: Int -> Gen V1RoleBinding
genV1RoleBinding n =
  V1RoleBinding
    <$> arbitraryReducedMaybe n -- v1RoleBindingApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1RoleBindingKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1RoleBindingMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1RoleBindingRoleRef :: V1RoleRef
    <*> arbitraryReduced n -- v1RoleBindingSubjects :: [V1Subject]
  
instance Arbitrary V1RoleBindingList where
  arbitrary = sized genV1RoleBindingList

genV1RoleBindingList :: Int -> Gen V1RoleBindingList
genV1RoleBindingList n =
  V1RoleBindingList
    <$> arbitraryReducedMaybe n -- v1RoleBindingListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1RoleBindingListItems :: [V1RoleBinding]
    <*> arbitraryReducedMaybe n -- v1RoleBindingListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1RoleBindingListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1RoleList where
  arbitrary = sized genV1RoleList

genV1RoleList :: Int -> Gen V1RoleList
genV1RoleList n =
  V1RoleList
    <$> arbitraryReducedMaybe n -- v1RoleListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1RoleListItems :: [V1Role]
    <*> arbitraryReducedMaybe n -- v1RoleListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1RoleListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1RoleRef where
  arbitrary = sized genV1RoleRef

genV1RoleRef :: Int -> Gen V1RoleRef
genV1RoleRef n =
  V1RoleRef
    <$> arbitrary -- v1RoleRefApiGroup :: Text
    <*> arbitrary -- v1RoleRefKind :: Text
    <*> arbitrary -- v1RoleRefName :: Text
  
instance Arbitrary V1RollingUpdateDaemonSet where
  arbitrary = sized genV1RollingUpdateDaemonSet

genV1RollingUpdateDaemonSet :: Int -> Gen V1RollingUpdateDaemonSet
genV1RollingUpdateDaemonSet n =
  V1RollingUpdateDaemonSet
    <$> arbitraryReducedMaybeValue n -- v1RollingUpdateDaemonSetMaxUnavailable :: Maybe A.Value
  
instance Arbitrary V1RollingUpdateDeployment where
  arbitrary = sized genV1RollingUpdateDeployment

genV1RollingUpdateDeployment :: Int -> Gen V1RollingUpdateDeployment
genV1RollingUpdateDeployment n =
  V1RollingUpdateDeployment
    <$> arbitraryReducedMaybeValue n -- v1RollingUpdateDeploymentMaxSurge :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- v1RollingUpdateDeploymentMaxUnavailable :: Maybe A.Value
  
instance Arbitrary V1RollingUpdateStatefulSetStrategy where
  arbitrary = sized genV1RollingUpdateStatefulSetStrategy

genV1RollingUpdateStatefulSetStrategy :: Int -> Gen V1RollingUpdateStatefulSetStrategy
genV1RollingUpdateStatefulSetStrategy n =
  V1RollingUpdateStatefulSetStrategy
    <$> arbitraryReducedMaybe n -- v1RollingUpdateStatefulSetStrategyPartition :: Maybe Int
  
instance Arbitrary V1SELinuxOptions where
  arbitrary = sized genV1SELinuxOptions

genV1SELinuxOptions :: Int -> Gen V1SELinuxOptions
genV1SELinuxOptions n =
  V1SELinuxOptions
    <$> arbitraryReducedMaybe n -- v1SELinuxOptionsLevel :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1SELinuxOptionsRole :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1SELinuxOptionsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1SELinuxOptionsUser :: Maybe Text
  
instance Arbitrary V1Scale where
  arbitrary = sized genV1Scale

genV1Scale :: Int -> Gen V1Scale
genV1Scale n =
  V1Scale
    <$> arbitraryReducedMaybe n -- v1ScaleApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ScaleKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ScaleMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1ScaleSpec :: Maybe V1ScaleSpec
    <*> arbitraryReducedMaybe n -- v1ScaleStatus :: Maybe V1ScaleStatus
  
instance Arbitrary V1ScaleIOPersistentVolumeSource where
  arbitrary = sized genV1ScaleIOPersistentVolumeSource

genV1ScaleIOPersistentVolumeSource :: Int -> Gen V1ScaleIOPersistentVolumeSource
genV1ScaleIOPersistentVolumeSource n =
  V1ScaleIOPersistentVolumeSource
    <$> arbitraryReducedMaybe n -- v1ScaleIOPersistentVolumeSourceFsType :: Maybe Text
    <*> arbitrary -- v1ScaleIOPersistentVolumeSourceGateway :: Text
    <*> arbitraryReducedMaybe n -- v1ScaleIOPersistentVolumeSourceProtectionDomain :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ScaleIOPersistentVolumeSourceReadOnly :: Maybe Bool
    <*> arbitraryReduced n -- v1ScaleIOPersistentVolumeSourceSecretRef :: V1SecretReference
    <*> arbitraryReducedMaybe n -- v1ScaleIOPersistentVolumeSourceSslEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1ScaleIOPersistentVolumeSourceStorageMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ScaleIOPersistentVolumeSourceStoragePool :: Maybe Text
    <*> arbitrary -- v1ScaleIOPersistentVolumeSourceSystem :: Text
    <*> arbitraryReducedMaybe n -- v1ScaleIOPersistentVolumeSourceVolumeName :: Maybe Text
  
instance Arbitrary V1ScaleIOVolumeSource where
  arbitrary = sized genV1ScaleIOVolumeSource

genV1ScaleIOVolumeSource :: Int -> Gen V1ScaleIOVolumeSource
genV1ScaleIOVolumeSource n =
  V1ScaleIOVolumeSource
    <$> arbitraryReducedMaybe n -- v1ScaleIOVolumeSourceFsType :: Maybe Text
    <*> arbitrary -- v1ScaleIOVolumeSourceGateway :: Text
    <*> arbitraryReducedMaybe n -- v1ScaleIOVolumeSourceProtectionDomain :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ScaleIOVolumeSourceReadOnly :: Maybe Bool
    <*> arbitraryReduced n -- v1ScaleIOVolumeSourceSecretRef :: V1LocalObjectReference
    <*> arbitraryReducedMaybe n -- v1ScaleIOVolumeSourceSslEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1ScaleIOVolumeSourceStorageMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ScaleIOVolumeSourceStoragePool :: Maybe Text
    <*> arbitrary -- v1ScaleIOVolumeSourceSystem :: Text
    <*> arbitraryReducedMaybe n -- v1ScaleIOVolumeSourceVolumeName :: Maybe Text
  
instance Arbitrary V1ScaleSpec where
  arbitrary = sized genV1ScaleSpec

genV1ScaleSpec :: Int -> Gen V1ScaleSpec
genV1ScaleSpec n =
  V1ScaleSpec
    <$> arbitraryReducedMaybe n -- v1ScaleSpecReplicas :: Maybe Int
  
instance Arbitrary V1ScaleStatus where
  arbitrary = sized genV1ScaleStatus

genV1ScaleStatus :: Int -> Gen V1ScaleStatus
genV1ScaleStatus n =
  V1ScaleStatus
    <$> arbitrary -- v1ScaleStatusReplicas :: Int
    <*> arbitraryReducedMaybe n -- v1ScaleStatusSelector :: Maybe Text
  
instance Arbitrary V1Secret where
  arbitrary = sized genV1Secret

genV1Secret :: Int -> Gen V1Secret
genV1Secret n =
  V1Secret
    <$> arbitraryReducedMaybe n -- v1SecretApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1SecretData :: Maybe (Map.Map String ByteArray)
    <*> arbitraryReducedMaybe n -- v1SecretKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1SecretMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1SecretStringData :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1SecretType :: Maybe Text
  
instance Arbitrary V1SecretEnvSource where
  arbitrary = sized genV1SecretEnvSource

genV1SecretEnvSource :: Int -> Gen V1SecretEnvSource
genV1SecretEnvSource n =
  V1SecretEnvSource
    <$> arbitraryReducedMaybe n -- v1SecretEnvSourceName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1SecretEnvSourceOptional :: Maybe Bool
  
instance Arbitrary V1SecretKeySelector where
  arbitrary = sized genV1SecretKeySelector

genV1SecretKeySelector :: Int -> Gen V1SecretKeySelector
genV1SecretKeySelector n =
  V1SecretKeySelector
    <$> arbitrary -- v1SecretKeySelectorKey :: Text
    <*> arbitraryReducedMaybe n -- v1SecretKeySelectorName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1SecretKeySelectorOptional :: Maybe Bool
  
instance Arbitrary V1SecretList where
  arbitrary = sized genV1SecretList

genV1SecretList :: Int -> Gen V1SecretList
genV1SecretList n =
  V1SecretList
    <$> arbitraryReducedMaybe n -- v1SecretListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1SecretListItems :: [V1Secret]
    <*> arbitraryReducedMaybe n -- v1SecretListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1SecretListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1SecretProjection where
  arbitrary = sized genV1SecretProjection

genV1SecretProjection :: Int -> Gen V1SecretProjection
genV1SecretProjection n =
  V1SecretProjection
    <$> arbitraryReducedMaybe n -- v1SecretProjectionItems :: Maybe [V1KeyToPath]
    <*> arbitraryReducedMaybe n -- v1SecretProjectionName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1SecretProjectionOptional :: Maybe Bool
  
instance Arbitrary V1SecretReference where
  arbitrary = sized genV1SecretReference

genV1SecretReference :: Int -> Gen V1SecretReference
genV1SecretReference n =
  V1SecretReference
    <$> arbitraryReducedMaybe n -- v1SecretReferenceName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1SecretReferenceNamespace :: Maybe Text
  
instance Arbitrary V1SecretVolumeSource where
  arbitrary = sized genV1SecretVolumeSource

genV1SecretVolumeSource :: Int -> Gen V1SecretVolumeSource
genV1SecretVolumeSource n =
  V1SecretVolumeSource
    <$> arbitraryReducedMaybe n -- v1SecretVolumeSourceDefaultMode :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1SecretVolumeSourceItems :: Maybe [V1KeyToPath]
    <*> arbitraryReducedMaybe n -- v1SecretVolumeSourceOptional :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1SecretVolumeSourceSecretName :: Maybe Text
  
instance Arbitrary V1SecurityContext where
  arbitrary = sized genV1SecurityContext

genV1SecurityContext :: Int -> Gen V1SecurityContext
genV1SecurityContext n =
  V1SecurityContext
    <$> arbitraryReducedMaybe n -- v1SecurityContextAllowPrivilegeEscalation :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1SecurityContextCapabilities :: Maybe V1Capabilities
    <*> arbitraryReducedMaybe n -- v1SecurityContextPrivileged :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1SecurityContextReadOnlyRootFilesystem :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1SecurityContextRunAsNonRoot :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1SecurityContextRunAsUser :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1SecurityContextSeLinuxOptions :: Maybe V1SELinuxOptions
  
instance Arbitrary V1SelfSubjectAccessReview where
  arbitrary = sized genV1SelfSubjectAccessReview

genV1SelfSubjectAccessReview :: Int -> Gen V1SelfSubjectAccessReview
genV1SelfSubjectAccessReview n =
  V1SelfSubjectAccessReview
    <$> arbitraryReducedMaybe n -- v1SelfSubjectAccessReviewApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1SelfSubjectAccessReviewKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1SelfSubjectAccessReviewMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1SelfSubjectAccessReviewSpec :: V1SelfSubjectAccessReviewSpec
    <*> arbitraryReducedMaybe n -- v1SelfSubjectAccessReviewStatus :: Maybe V1SubjectAccessReviewStatus
  
instance Arbitrary V1SelfSubjectAccessReviewSpec where
  arbitrary = sized genV1SelfSubjectAccessReviewSpec

genV1SelfSubjectAccessReviewSpec :: Int -> Gen V1SelfSubjectAccessReviewSpec
genV1SelfSubjectAccessReviewSpec n =
  V1SelfSubjectAccessReviewSpec
    <$> arbitraryReducedMaybe n -- v1SelfSubjectAccessReviewSpecNonResourceAttributes :: Maybe V1NonResourceAttributes
    <*> arbitraryReducedMaybe n -- v1SelfSubjectAccessReviewSpecResourceAttributes :: Maybe V1ResourceAttributes
  
instance Arbitrary V1SelfSubjectRulesReview where
  arbitrary = sized genV1SelfSubjectRulesReview

genV1SelfSubjectRulesReview :: Int -> Gen V1SelfSubjectRulesReview
genV1SelfSubjectRulesReview n =
  V1SelfSubjectRulesReview
    <$> arbitraryReducedMaybe n -- v1SelfSubjectRulesReviewApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1SelfSubjectRulesReviewKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1SelfSubjectRulesReviewMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1SelfSubjectRulesReviewSpec :: V1SelfSubjectRulesReviewSpec
    <*> arbitraryReducedMaybe n -- v1SelfSubjectRulesReviewStatus :: Maybe V1SubjectRulesReviewStatus
  
instance Arbitrary V1SelfSubjectRulesReviewSpec where
  arbitrary = sized genV1SelfSubjectRulesReviewSpec

genV1SelfSubjectRulesReviewSpec :: Int -> Gen V1SelfSubjectRulesReviewSpec
genV1SelfSubjectRulesReviewSpec n =
  V1SelfSubjectRulesReviewSpec
    <$> arbitraryReducedMaybe n -- v1SelfSubjectRulesReviewSpecNamespace :: Maybe Text
  
instance Arbitrary V1ServerAddressByClientCIDR where
  arbitrary = sized genV1ServerAddressByClientCIDR

genV1ServerAddressByClientCIDR :: Int -> Gen V1ServerAddressByClientCIDR
genV1ServerAddressByClientCIDR n =
  V1ServerAddressByClientCIDR
    <$> arbitrary -- v1ServerAddressByClientCIDRClientCidr :: Text
    <*> arbitrary -- v1ServerAddressByClientCIDRServerAddress :: Text
  
instance Arbitrary V1Service where
  arbitrary = sized genV1Service

genV1Service :: Int -> Gen V1Service
genV1Service n =
  V1Service
    <$> arbitraryReducedMaybe n -- v1ServiceApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ServiceKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ServiceMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1ServiceSpec :: Maybe V1ServiceSpec
    <*> arbitraryReducedMaybe n -- v1ServiceStatus :: Maybe V1ServiceStatus
  
instance Arbitrary V1ServiceAccount where
  arbitrary = sized genV1ServiceAccount

genV1ServiceAccount :: Int -> Gen V1ServiceAccount
genV1ServiceAccount n =
  V1ServiceAccount
    <$> arbitraryReducedMaybe n -- v1ServiceAccountApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ServiceAccountAutomountServiceAccountToken :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1ServiceAccountImagePullSecrets :: Maybe [V1LocalObjectReference]
    <*> arbitraryReducedMaybe n -- v1ServiceAccountKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ServiceAccountMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1ServiceAccountSecrets :: Maybe [V1ObjectReference]
  
instance Arbitrary V1ServiceAccountList where
  arbitrary = sized genV1ServiceAccountList

genV1ServiceAccountList :: Int -> Gen V1ServiceAccountList
genV1ServiceAccountList n =
  V1ServiceAccountList
    <$> arbitraryReducedMaybe n -- v1ServiceAccountListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1ServiceAccountListItems :: [V1ServiceAccount]
    <*> arbitraryReducedMaybe n -- v1ServiceAccountListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ServiceAccountListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1ServiceList where
  arbitrary = sized genV1ServiceList

genV1ServiceList :: Int -> Gen V1ServiceList
genV1ServiceList n =
  V1ServiceList
    <$> arbitraryReducedMaybe n -- v1ServiceListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1ServiceListItems :: [V1Service]
    <*> arbitraryReducedMaybe n -- v1ServiceListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ServiceListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1ServicePort where
  arbitrary = sized genV1ServicePort

genV1ServicePort :: Int -> Gen V1ServicePort
genV1ServicePort n =
  V1ServicePort
    <$> arbitraryReducedMaybe n -- v1ServicePortName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ServicePortNodePort :: Maybe Int
    <*> arbitrary -- v1ServicePortPort :: Int
    <*> arbitraryReducedMaybe n -- v1ServicePortProtocol :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- v1ServicePortTargetPort :: Maybe A.Value
  
instance Arbitrary V1ServiceSpec where
  arbitrary = sized genV1ServiceSpec

genV1ServiceSpec :: Int -> Gen V1ServiceSpec
genV1ServiceSpec n =
  V1ServiceSpec
    <$> arbitraryReducedMaybe n -- v1ServiceSpecClusterIp :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ServiceSpecExternalIPs :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1ServiceSpecExternalName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ServiceSpecExternalTrafficPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ServiceSpecHealthCheckNodePort :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1ServiceSpecLoadBalancerIp :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ServiceSpecLoadBalancerSourceRanges :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1ServiceSpecPorts :: Maybe [V1ServicePort]
    <*> arbitraryReducedMaybe n -- v1ServiceSpecPublishNotReadyAddresses :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1ServiceSpecSelector :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1ServiceSpecSessionAffinity :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ServiceSpecSessionAffinityConfig :: Maybe V1SessionAffinityConfig
    <*> arbitraryReducedMaybe n -- v1ServiceSpecType :: Maybe Text
  
instance Arbitrary V1ServiceStatus where
  arbitrary = sized genV1ServiceStatus

genV1ServiceStatus :: Int -> Gen V1ServiceStatus
genV1ServiceStatus n =
  V1ServiceStatus
    <$> arbitraryReducedMaybe n -- v1ServiceStatusLoadBalancer :: Maybe V1LoadBalancerStatus
  
instance Arbitrary V1SessionAffinityConfig where
  arbitrary = sized genV1SessionAffinityConfig

genV1SessionAffinityConfig :: Int -> Gen V1SessionAffinityConfig
genV1SessionAffinityConfig n =
  V1SessionAffinityConfig
    <$> arbitraryReducedMaybe n -- v1SessionAffinityConfigClientIp :: Maybe V1ClientIPConfig
  
instance Arbitrary V1StatefulSet where
  arbitrary = sized genV1StatefulSet

genV1StatefulSet :: Int -> Gen V1StatefulSet
genV1StatefulSet n =
  V1StatefulSet
    <$> arbitraryReducedMaybe n -- v1StatefulSetApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StatefulSetKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StatefulSetMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1StatefulSetSpec :: Maybe V1StatefulSetSpec
    <*> arbitraryReducedMaybe n -- v1StatefulSetStatus :: Maybe V1StatefulSetStatus
  
instance Arbitrary V1StatefulSetCondition where
  arbitrary = sized genV1StatefulSetCondition

genV1StatefulSetCondition :: Int -> Gen V1StatefulSetCondition
genV1StatefulSetCondition n =
  V1StatefulSetCondition
    <$> arbitraryReducedMaybe n -- v1StatefulSetConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1StatefulSetConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StatefulSetConditionReason :: Maybe Text
    <*> arbitrary -- v1StatefulSetConditionStatus :: Text
    <*> arbitrary -- v1StatefulSetConditionType :: Text
  
instance Arbitrary V1StatefulSetList where
  arbitrary = sized genV1StatefulSetList

genV1StatefulSetList :: Int -> Gen V1StatefulSetList
genV1StatefulSetList n =
  V1StatefulSetList
    <$> arbitraryReducedMaybe n -- v1StatefulSetListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1StatefulSetListItems :: [V1StatefulSet]
    <*> arbitraryReducedMaybe n -- v1StatefulSetListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StatefulSetListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1StatefulSetSpec where
  arbitrary = sized genV1StatefulSetSpec

genV1StatefulSetSpec :: Int -> Gen V1StatefulSetSpec
genV1StatefulSetSpec n =
  V1StatefulSetSpec
    <$> arbitraryReducedMaybe n -- v1StatefulSetSpecPodManagementPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StatefulSetSpecReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1StatefulSetSpecRevisionHistoryLimit :: Maybe Int
    <*> arbitraryReduced n -- v1StatefulSetSpecSelector :: V1LabelSelector
    <*> arbitrary -- v1StatefulSetSpecServiceName :: Text
    <*> arbitraryReduced n -- v1StatefulSetSpecTemplate :: V1PodTemplateSpec
    <*> arbitraryReducedMaybe n -- v1StatefulSetSpecUpdateStrategy :: Maybe V1StatefulSetUpdateStrategy
    <*> arbitraryReducedMaybe n -- v1StatefulSetSpecVolumeClaimTemplates :: Maybe [V1PersistentVolumeClaim]
  
instance Arbitrary V1StatefulSetStatus where
  arbitrary = sized genV1StatefulSetStatus

genV1StatefulSetStatus :: Int -> Gen V1StatefulSetStatus
genV1StatefulSetStatus n =
  V1StatefulSetStatus
    <$> arbitraryReducedMaybe n -- v1StatefulSetStatusCollisionCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1StatefulSetStatusConditions :: Maybe [V1StatefulSetCondition]
    <*> arbitraryReducedMaybe n -- v1StatefulSetStatusCurrentReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1StatefulSetStatusCurrentRevision :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StatefulSetStatusObservedGeneration :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1StatefulSetStatusReadyReplicas :: Maybe Int
    <*> arbitrary -- v1StatefulSetStatusReplicas :: Int
    <*> arbitraryReducedMaybe n -- v1StatefulSetStatusUpdateRevision :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StatefulSetStatusUpdatedReplicas :: Maybe Int
  
instance Arbitrary V1StatefulSetUpdateStrategy where
  arbitrary = sized genV1StatefulSetUpdateStrategy

genV1StatefulSetUpdateStrategy :: Int -> Gen V1StatefulSetUpdateStrategy
genV1StatefulSetUpdateStrategy n =
  V1StatefulSetUpdateStrategy
    <$> arbitraryReducedMaybe n -- v1StatefulSetUpdateStrategyRollingUpdate :: Maybe V1RollingUpdateStatefulSetStrategy
    <*> arbitraryReducedMaybe n -- v1StatefulSetUpdateStrategyType :: Maybe Text
  
instance Arbitrary V1Status where
  arbitrary = sized genV1Status

genV1Status :: Int -> Gen V1Status
genV1Status n =
  V1Status
    <$> arbitraryReducedMaybe n -- v1StatusApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StatusCode :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1StatusDetails :: Maybe V1StatusDetails
    <*> arbitraryReducedMaybe n -- v1StatusKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StatusMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StatusMetadata :: Maybe V1ListMeta
    <*> arbitraryReducedMaybe n -- v1StatusReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StatusStatus :: Maybe Text
  
instance Arbitrary V1StatusCause where
  arbitrary = sized genV1StatusCause

genV1StatusCause :: Int -> Gen V1StatusCause
genV1StatusCause n =
  V1StatusCause
    <$> arbitraryReducedMaybe n -- v1StatusCauseField :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StatusCauseMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StatusCauseReason :: Maybe Text
  
instance Arbitrary V1StatusDetails where
  arbitrary = sized genV1StatusDetails

genV1StatusDetails :: Int -> Gen V1StatusDetails
genV1StatusDetails n =
  V1StatusDetails
    <$> arbitraryReducedMaybe n -- v1StatusDetailsCauses :: Maybe [V1StatusCause]
    <*> arbitraryReducedMaybe n -- v1StatusDetailsGroup :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StatusDetailsKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StatusDetailsName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StatusDetailsRetryAfterSeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1StatusDetailsUid :: Maybe Text
  
instance Arbitrary V1StorageClass where
  arbitrary = sized genV1StorageClass

genV1StorageClass :: Int -> Gen V1StorageClass
genV1StorageClass n =
  V1StorageClass
    <$> arbitraryReducedMaybe n -- v1StorageClassAllowVolumeExpansion :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1StorageClassApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StorageClassKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StorageClassMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1StorageClassMountOptions :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1StorageClassParameters :: Maybe (Map.Map String Text)
    <*> arbitrary -- v1StorageClassProvisioner :: Text
    <*> arbitraryReducedMaybe n -- v1StorageClassReclaimPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StorageClassVolumeBindingMode :: Maybe Text
  
instance Arbitrary V1StorageClassList where
  arbitrary = sized genV1StorageClassList

genV1StorageClassList :: Int -> Gen V1StorageClassList
genV1StorageClassList n =
  V1StorageClassList
    <$> arbitraryReducedMaybe n -- v1StorageClassListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1StorageClassListItems :: [V1StorageClass]
    <*> arbitraryReducedMaybe n -- v1StorageClassListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StorageClassListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1StorageOSPersistentVolumeSource where
  arbitrary = sized genV1StorageOSPersistentVolumeSource

genV1StorageOSPersistentVolumeSource :: Int -> Gen V1StorageOSPersistentVolumeSource
genV1StorageOSPersistentVolumeSource n =
  V1StorageOSPersistentVolumeSource
    <$> arbitraryReducedMaybe n -- v1StorageOSPersistentVolumeSourceFsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StorageOSPersistentVolumeSourceReadOnly :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1StorageOSPersistentVolumeSourceSecretRef :: Maybe V1ObjectReference
    <*> arbitraryReducedMaybe n -- v1StorageOSPersistentVolumeSourceVolumeName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StorageOSPersistentVolumeSourceVolumeNamespace :: Maybe Text
  
instance Arbitrary V1StorageOSVolumeSource where
  arbitrary = sized genV1StorageOSVolumeSource

genV1StorageOSVolumeSource :: Int -> Gen V1StorageOSVolumeSource
genV1StorageOSVolumeSource n =
  V1StorageOSVolumeSource
    <$> arbitraryReducedMaybe n -- v1StorageOSVolumeSourceFsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StorageOSVolumeSourceReadOnly :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1StorageOSVolumeSourceSecretRef :: Maybe V1LocalObjectReference
    <*> arbitraryReducedMaybe n -- v1StorageOSVolumeSourceVolumeName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StorageOSVolumeSourceVolumeNamespace :: Maybe Text
  
instance Arbitrary V1Subject where
  arbitrary = sized genV1Subject

genV1Subject :: Int -> Gen V1Subject
genV1Subject n =
  V1Subject
    <$> arbitraryReducedMaybe n -- v1SubjectApiGroup :: Maybe Text
    <*> arbitrary -- v1SubjectKind :: Text
    <*> arbitrary -- v1SubjectName :: Text
    <*> arbitraryReducedMaybe n -- v1SubjectNamespace :: Maybe Text
  
instance Arbitrary V1SubjectAccessReview where
  arbitrary = sized genV1SubjectAccessReview

genV1SubjectAccessReview :: Int -> Gen V1SubjectAccessReview
genV1SubjectAccessReview n =
  V1SubjectAccessReview
    <$> arbitraryReducedMaybe n -- v1SubjectAccessReviewApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1SubjectAccessReviewKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1SubjectAccessReviewMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1SubjectAccessReviewSpec :: V1SubjectAccessReviewSpec
    <*> arbitraryReducedMaybe n -- v1SubjectAccessReviewStatus :: Maybe V1SubjectAccessReviewStatus
  
instance Arbitrary V1SubjectAccessReviewSpec where
  arbitrary = sized genV1SubjectAccessReviewSpec

genV1SubjectAccessReviewSpec :: Int -> Gen V1SubjectAccessReviewSpec
genV1SubjectAccessReviewSpec n =
  V1SubjectAccessReviewSpec
    <$> arbitraryReducedMaybe n -- v1SubjectAccessReviewSpecExtra :: Maybe (Map.Map String [Text])
    <*> arbitraryReducedMaybe n -- v1SubjectAccessReviewSpecGroups :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1SubjectAccessReviewSpecNonResourceAttributes :: Maybe V1NonResourceAttributes
    <*> arbitraryReducedMaybe n -- v1SubjectAccessReviewSpecResourceAttributes :: Maybe V1ResourceAttributes
    <*> arbitraryReducedMaybe n -- v1SubjectAccessReviewSpecUid :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1SubjectAccessReviewSpecUser :: Maybe Text
  
instance Arbitrary V1SubjectAccessReviewStatus where
  arbitrary = sized genV1SubjectAccessReviewStatus

genV1SubjectAccessReviewStatus :: Int -> Gen V1SubjectAccessReviewStatus
genV1SubjectAccessReviewStatus n =
  V1SubjectAccessReviewStatus
    <$> arbitrary -- v1SubjectAccessReviewStatusAllowed :: Bool
    <*> arbitraryReducedMaybe n -- v1SubjectAccessReviewStatusDenied :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1SubjectAccessReviewStatusEvaluationError :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1SubjectAccessReviewStatusReason :: Maybe Text
  
instance Arbitrary V1SubjectRulesReviewStatus where
  arbitrary = sized genV1SubjectRulesReviewStatus

genV1SubjectRulesReviewStatus :: Int -> Gen V1SubjectRulesReviewStatus
genV1SubjectRulesReviewStatus n =
  V1SubjectRulesReviewStatus
    <$> arbitraryReducedMaybe n -- v1SubjectRulesReviewStatusEvaluationError :: Maybe Text
    <*> arbitrary -- v1SubjectRulesReviewStatusIncomplete :: Bool
    <*> arbitraryReduced n -- v1SubjectRulesReviewStatusNonResourceRules :: [V1NonResourceRule]
    <*> arbitraryReduced n -- v1SubjectRulesReviewStatusResourceRules :: [V1ResourceRule]
  
instance Arbitrary V1TCPSocketAction where
  arbitrary = sized genV1TCPSocketAction

genV1TCPSocketAction :: Int -> Gen V1TCPSocketAction
genV1TCPSocketAction n =
  V1TCPSocketAction
    <$> arbitraryReducedMaybe n -- v1TCPSocketActionHost :: Maybe Text
    <*> arbitraryReduced n -- v1TCPSocketActionPort :: A.Value
  
instance Arbitrary V1Taint where
  arbitrary = sized genV1Taint

genV1Taint :: Int -> Gen V1Taint
genV1Taint n =
  V1Taint
    <$> arbitrary -- v1TaintEffect :: Text
    <*> arbitrary -- v1TaintKey :: Text
    <*> arbitraryReducedMaybe n -- v1TaintTimeAdded :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1TaintValue :: Maybe Text
  
instance Arbitrary V1TokenReview where
  arbitrary = sized genV1TokenReview

genV1TokenReview :: Int -> Gen V1TokenReview
genV1TokenReview n =
  V1TokenReview
    <$> arbitraryReducedMaybe n -- v1TokenReviewApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1TokenReviewKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1TokenReviewMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1TokenReviewSpec :: V1TokenReviewSpec
    <*> arbitraryReducedMaybe n -- v1TokenReviewStatus :: Maybe V1TokenReviewStatus
  
instance Arbitrary V1TokenReviewSpec where
  arbitrary = sized genV1TokenReviewSpec

genV1TokenReviewSpec :: Int -> Gen V1TokenReviewSpec
genV1TokenReviewSpec n =
  V1TokenReviewSpec
    <$> arbitraryReducedMaybe n -- v1TokenReviewSpecToken :: Maybe Text
  
instance Arbitrary V1TokenReviewStatus where
  arbitrary = sized genV1TokenReviewStatus

genV1TokenReviewStatus :: Int -> Gen V1TokenReviewStatus
genV1TokenReviewStatus n =
  V1TokenReviewStatus
    <$> arbitraryReducedMaybe n -- v1TokenReviewStatusAuthenticated :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1TokenReviewStatusError :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1TokenReviewStatusUser :: Maybe V1UserInfo
  
instance Arbitrary V1Toleration where
  arbitrary = sized genV1Toleration

genV1Toleration :: Int -> Gen V1Toleration
genV1Toleration n =
  V1Toleration
    <$> arbitraryReducedMaybe n -- v1TolerationEffect :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1TolerationKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1TolerationOperator :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1TolerationTolerationSeconds :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1TolerationValue :: Maybe Text
  
instance Arbitrary V1UserInfo where
  arbitrary = sized genV1UserInfo

genV1UserInfo :: Int -> Gen V1UserInfo
genV1UserInfo n =
  V1UserInfo
    <$> arbitraryReducedMaybe n -- v1UserInfoExtra :: Maybe (Map.Map String [Text])
    <*> arbitraryReducedMaybe n -- v1UserInfoGroups :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1UserInfoUid :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1UserInfoUsername :: Maybe Text
  
instance Arbitrary V1Volume where
  arbitrary = sized genV1Volume

genV1Volume :: Int -> Gen V1Volume
genV1Volume n =
  V1Volume
    <$> arbitraryReducedMaybe n -- v1VolumeAwsElasticBlockStore :: Maybe V1AWSElasticBlockStoreVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeAzureDisk :: Maybe V1AzureDiskVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeAzureFile :: Maybe V1AzureFileVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeCephfs :: Maybe V1CephFSVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeCinder :: Maybe V1CinderVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeConfigMap :: Maybe V1ConfigMapVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeDownwardApi :: Maybe V1DownwardAPIVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeEmptyDir :: Maybe V1EmptyDirVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeFc :: Maybe V1FCVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeFlexVolume :: Maybe V1FlexVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeFlocker :: Maybe V1FlockerVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeGcePersistentDisk :: Maybe V1GCEPersistentDiskVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeGitRepo :: Maybe V1GitRepoVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeGlusterfs :: Maybe V1GlusterfsVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeHostPath :: Maybe V1HostPathVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeIscsi :: Maybe V1ISCSIVolumeSource
    <*> arbitrary -- v1VolumeName :: Text
    <*> arbitraryReducedMaybe n -- v1VolumeNfs :: Maybe V1NFSVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumePersistentVolumeClaim :: Maybe V1PersistentVolumeClaimVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumePhotonPersistentDisk :: Maybe V1PhotonPersistentDiskVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumePortworxVolume :: Maybe V1PortworxVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeProjected :: Maybe V1ProjectedVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeQuobyte :: Maybe V1QuobyteVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeRbd :: Maybe V1RBDVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeScaleIo :: Maybe V1ScaleIOVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeSecret :: Maybe V1SecretVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeStorageos :: Maybe V1StorageOSVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeVsphereVolume :: Maybe V1VsphereVirtualDiskVolumeSource
  
instance Arbitrary V1VolumeDevice where
  arbitrary = sized genV1VolumeDevice

genV1VolumeDevice :: Int -> Gen V1VolumeDevice
genV1VolumeDevice n =
  V1VolumeDevice
    <$> arbitrary -- v1VolumeDeviceDevicePath :: Text
    <*> arbitrary -- v1VolumeDeviceName :: Text
  
instance Arbitrary V1VolumeMount where
  arbitrary = sized genV1VolumeMount

genV1VolumeMount :: Int -> Gen V1VolumeMount
genV1VolumeMount n =
  V1VolumeMount
    <$> arbitrary -- v1VolumeMountMountPath :: Text
    <*> arbitraryReducedMaybe n -- v1VolumeMountMountPropagation :: Maybe Text
    <*> arbitrary -- v1VolumeMountName :: Text
    <*> arbitraryReducedMaybe n -- v1VolumeMountReadOnly :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1VolumeMountSubPath :: Maybe Text
  
instance Arbitrary V1VolumeProjection where
  arbitrary = sized genV1VolumeProjection

genV1VolumeProjection :: Int -> Gen V1VolumeProjection
genV1VolumeProjection n =
  V1VolumeProjection
    <$> arbitraryReducedMaybe n -- v1VolumeProjectionConfigMap :: Maybe V1ConfigMapProjection
    <*> arbitraryReducedMaybe n -- v1VolumeProjectionDownwardApi :: Maybe V1DownwardAPIProjection
    <*> arbitraryReducedMaybe n -- v1VolumeProjectionSecret :: Maybe V1SecretProjection
  
instance Arbitrary V1VsphereVirtualDiskVolumeSource where
  arbitrary = sized genV1VsphereVirtualDiskVolumeSource

genV1VsphereVirtualDiskVolumeSource :: Int -> Gen V1VsphereVirtualDiskVolumeSource
genV1VsphereVirtualDiskVolumeSource n =
  V1VsphereVirtualDiskVolumeSource
    <$> arbitraryReducedMaybe n -- v1VsphereVirtualDiskVolumeSourceFsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1VsphereVirtualDiskVolumeSourceStoragePolicyId :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1VsphereVirtualDiskVolumeSourceStoragePolicyName :: Maybe Text
    <*> arbitrary -- v1VsphereVirtualDiskVolumeSourceVolumePath :: Text
  
instance Arbitrary V1WatchEvent where
  arbitrary = sized genV1WatchEvent

genV1WatchEvent :: Int -> Gen V1WatchEvent
genV1WatchEvent n =
  V1WatchEvent
    <$> arbitraryReduced n -- v1WatchEventObject :: RuntimeRawExtension
    <*> arbitrary -- v1WatchEventType :: Text
  
instance Arbitrary V1WeightedPodAffinityTerm where
  arbitrary = sized genV1WeightedPodAffinityTerm

genV1WeightedPodAffinityTerm :: Int -> Gen V1WeightedPodAffinityTerm
genV1WeightedPodAffinityTerm n =
  V1WeightedPodAffinityTerm
    <$> arbitraryReduced n -- v1WeightedPodAffinityTermPodAffinityTerm :: V1PodAffinityTerm
    <*> arbitrary -- v1WeightedPodAffinityTermWeight :: Int
  
instance Arbitrary V1alpha1AggregationRule where
  arbitrary = sized genV1alpha1AggregationRule

genV1alpha1AggregationRule :: Int -> Gen V1alpha1AggregationRule
genV1alpha1AggregationRule n =
  V1alpha1AggregationRule
    <$> arbitraryReducedMaybe n -- v1alpha1AggregationRuleClusterRoleSelectors :: Maybe [V1LabelSelector]
  
instance Arbitrary V1alpha1ClusterRole where
  arbitrary = sized genV1alpha1ClusterRole

genV1alpha1ClusterRole :: Int -> Gen V1alpha1ClusterRole
genV1alpha1ClusterRole n =
  V1alpha1ClusterRole
    <$> arbitraryReducedMaybe n -- v1alpha1ClusterRoleAggregationRule :: Maybe V1alpha1AggregationRule
    <*> arbitraryReducedMaybe n -- v1alpha1ClusterRoleApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ClusterRoleKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ClusterRoleMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1alpha1ClusterRoleRules :: [V1alpha1PolicyRule]
  
instance Arbitrary V1alpha1ClusterRoleBinding where
  arbitrary = sized genV1alpha1ClusterRoleBinding

genV1alpha1ClusterRoleBinding :: Int -> Gen V1alpha1ClusterRoleBinding
genV1alpha1ClusterRoleBinding n =
  V1alpha1ClusterRoleBinding
    <$> arbitraryReducedMaybe n -- v1alpha1ClusterRoleBindingApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ClusterRoleBindingKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ClusterRoleBindingMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1alpha1ClusterRoleBindingRoleRef :: V1alpha1RoleRef
    <*> arbitraryReduced n -- v1alpha1ClusterRoleBindingSubjects :: [V1alpha1Subject]
  
instance Arbitrary V1alpha1ClusterRoleBindingList where
  arbitrary = sized genV1alpha1ClusterRoleBindingList

genV1alpha1ClusterRoleBindingList :: Int -> Gen V1alpha1ClusterRoleBindingList
genV1alpha1ClusterRoleBindingList n =
  V1alpha1ClusterRoleBindingList
    <$> arbitraryReducedMaybe n -- v1alpha1ClusterRoleBindingListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1alpha1ClusterRoleBindingListItems :: [V1alpha1ClusterRoleBinding]
    <*> arbitraryReducedMaybe n -- v1alpha1ClusterRoleBindingListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ClusterRoleBindingListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1alpha1ClusterRoleList where
  arbitrary = sized genV1alpha1ClusterRoleList

genV1alpha1ClusterRoleList :: Int -> Gen V1alpha1ClusterRoleList
genV1alpha1ClusterRoleList n =
  V1alpha1ClusterRoleList
    <$> arbitraryReducedMaybe n -- v1alpha1ClusterRoleListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1alpha1ClusterRoleListItems :: [V1alpha1ClusterRole]
    <*> arbitraryReducedMaybe n -- v1alpha1ClusterRoleListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ClusterRoleListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1alpha1Initializer where
  arbitrary = sized genV1alpha1Initializer

genV1alpha1Initializer :: Int -> Gen V1alpha1Initializer
genV1alpha1Initializer n =
  V1alpha1Initializer
    <$> arbitrary -- v1alpha1InitializerName :: Text
    <*> arbitraryReducedMaybe n -- v1alpha1InitializerRules :: Maybe [V1alpha1Rule]
  
instance Arbitrary V1alpha1InitializerConfiguration where
  arbitrary = sized genV1alpha1InitializerConfiguration

genV1alpha1InitializerConfiguration :: Int -> Gen V1alpha1InitializerConfiguration
genV1alpha1InitializerConfiguration n =
  V1alpha1InitializerConfiguration
    <$> arbitraryReducedMaybe n -- v1alpha1InitializerConfigurationApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1InitializerConfigurationInitializers :: Maybe [V1alpha1Initializer]
    <*> arbitraryReducedMaybe n -- v1alpha1InitializerConfigurationKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1InitializerConfigurationMetadata :: Maybe V1ObjectMeta
  
instance Arbitrary V1alpha1InitializerConfigurationList where
  arbitrary = sized genV1alpha1InitializerConfigurationList

genV1alpha1InitializerConfigurationList :: Int -> Gen V1alpha1InitializerConfigurationList
genV1alpha1InitializerConfigurationList n =
  V1alpha1InitializerConfigurationList
    <$> arbitraryReducedMaybe n -- v1alpha1InitializerConfigurationListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1alpha1InitializerConfigurationListItems :: [V1alpha1InitializerConfiguration]
    <*> arbitraryReducedMaybe n -- v1alpha1InitializerConfigurationListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1InitializerConfigurationListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1alpha1PodPreset where
  arbitrary = sized genV1alpha1PodPreset

genV1alpha1PodPreset :: Int -> Gen V1alpha1PodPreset
genV1alpha1PodPreset n =
  V1alpha1PodPreset
    <$> arbitraryReducedMaybe n -- v1alpha1PodPresetApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1PodPresetKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1PodPresetMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1alpha1PodPresetSpec :: Maybe V1alpha1PodPresetSpec
  
instance Arbitrary V1alpha1PodPresetList where
  arbitrary = sized genV1alpha1PodPresetList

genV1alpha1PodPresetList :: Int -> Gen V1alpha1PodPresetList
genV1alpha1PodPresetList n =
  V1alpha1PodPresetList
    <$> arbitraryReducedMaybe n -- v1alpha1PodPresetListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1alpha1PodPresetListItems :: [V1alpha1PodPreset]
    <*> arbitraryReducedMaybe n -- v1alpha1PodPresetListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1PodPresetListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1alpha1PodPresetSpec where
  arbitrary = sized genV1alpha1PodPresetSpec

genV1alpha1PodPresetSpec :: Int -> Gen V1alpha1PodPresetSpec
genV1alpha1PodPresetSpec n =
  V1alpha1PodPresetSpec
    <$> arbitraryReducedMaybe n -- v1alpha1PodPresetSpecEnv :: Maybe [V1EnvVar]
    <*> arbitraryReducedMaybe n -- v1alpha1PodPresetSpecEnvFrom :: Maybe [V1EnvFromSource]
    <*> arbitraryReducedMaybe n -- v1alpha1PodPresetSpecSelector :: Maybe V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1alpha1PodPresetSpecVolumeMounts :: Maybe [V1VolumeMount]
    <*> arbitraryReducedMaybe n -- v1alpha1PodPresetSpecVolumes :: Maybe [V1Volume]
  
instance Arbitrary V1alpha1PolicyRule where
  arbitrary = sized genV1alpha1PolicyRule

genV1alpha1PolicyRule :: Int -> Gen V1alpha1PolicyRule
genV1alpha1PolicyRule n =
  V1alpha1PolicyRule
    <$> arbitraryReducedMaybe n -- v1alpha1PolicyRuleApiGroups :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1alpha1PolicyRuleNonResourceUrLs :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1alpha1PolicyRuleResourceNames :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1alpha1PolicyRuleResources :: Maybe [Text]
    <*> arbitrary -- v1alpha1PolicyRuleVerbs :: [Text]
  
instance Arbitrary V1alpha1PriorityClass where
  arbitrary = sized genV1alpha1PriorityClass

genV1alpha1PriorityClass :: Int -> Gen V1alpha1PriorityClass
genV1alpha1PriorityClass n =
  V1alpha1PriorityClass
    <$> arbitraryReducedMaybe n -- v1alpha1PriorityClassApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1PriorityClassDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1PriorityClassGlobalDefault :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1alpha1PriorityClassKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1PriorityClassMetadata :: Maybe V1ObjectMeta
    <*> arbitrary -- v1alpha1PriorityClassValue :: Int
  
instance Arbitrary V1alpha1PriorityClassList where
  arbitrary = sized genV1alpha1PriorityClassList

genV1alpha1PriorityClassList :: Int -> Gen V1alpha1PriorityClassList
genV1alpha1PriorityClassList n =
  V1alpha1PriorityClassList
    <$> arbitraryReducedMaybe n -- v1alpha1PriorityClassListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1alpha1PriorityClassListItems :: [V1alpha1PriorityClass]
    <*> arbitraryReducedMaybe n -- v1alpha1PriorityClassListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1PriorityClassListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1alpha1Role where
  arbitrary = sized genV1alpha1Role

genV1alpha1Role :: Int -> Gen V1alpha1Role
genV1alpha1Role n =
  V1alpha1Role
    <$> arbitraryReducedMaybe n -- v1alpha1RoleApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1RoleKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1RoleMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1alpha1RoleRules :: [V1alpha1PolicyRule]
  
instance Arbitrary V1alpha1RoleBinding where
  arbitrary = sized genV1alpha1RoleBinding

genV1alpha1RoleBinding :: Int -> Gen V1alpha1RoleBinding
genV1alpha1RoleBinding n =
  V1alpha1RoleBinding
    <$> arbitraryReducedMaybe n -- v1alpha1RoleBindingApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1RoleBindingKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1RoleBindingMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1alpha1RoleBindingRoleRef :: V1alpha1RoleRef
    <*> arbitraryReduced n -- v1alpha1RoleBindingSubjects :: [V1alpha1Subject]
  
instance Arbitrary V1alpha1RoleBindingList where
  arbitrary = sized genV1alpha1RoleBindingList

genV1alpha1RoleBindingList :: Int -> Gen V1alpha1RoleBindingList
genV1alpha1RoleBindingList n =
  V1alpha1RoleBindingList
    <$> arbitraryReducedMaybe n -- v1alpha1RoleBindingListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1alpha1RoleBindingListItems :: [V1alpha1RoleBinding]
    <*> arbitraryReducedMaybe n -- v1alpha1RoleBindingListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1RoleBindingListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1alpha1RoleList where
  arbitrary = sized genV1alpha1RoleList

genV1alpha1RoleList :: Int -> Gen V1alpha1RoleList
genV1alpha1RoleList n =
  V1alpha1RoleList
    <$> arbitraryReducedMaybe n -- v1alpha1RoleListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1alpha1RoleListItems :: [V1alpha1Role]
    <*> arbitraryReducedMaybe n -- v1alpha1RoleListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1RoleListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1alpha1RoleRef where
  arbitrary = sized genV1alpha1RoleRef

genV1alpha1RoleRef :: Int -> Gen V1alpha1RoleRef
genV1alpha1RoleRef n =
  V1alpha1RoleRef
    <$> arbitrary -- v1alpha1RoleRefApiGroup :: Text
    <*> arbitrary -- v1alpha1RoleRefKind :: Text
    <*> arbitrary -- v1alpha1RoleRefName :: Text
  
instance Arbitrary V1alpha1Rule where
  arbitrary = sized genV1alpha1Rule

genV1alpha1Rule :: Int -> Gen V1alpha1Rule
genV1alpha1Rule n =
  V1alpha1Rule
    <$> arbitraryReducedMaybe n -- v1alpha1RuleApiGroups :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1alpha1RuleApiVersions :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1alpha1RuleResources :: Maybe [Text]
  
instance Arbitrary V1alpha1Subject where
  arbitrary = sized genV1alpha1Subject

genV1alpha1Subject :: Int -> Gen V1alpha1Subject
genV1alpha1Subject n =
  V1alpha1Subject
    <$> arbitraryReducedMaybe n -- v1alpha1SubjectApiVersion :: Maybe Text
    <*> arbitrary -- v1alpha1SubjectKind :: Text
    <*> arbitrary -- v1alpha1SubjectName :: Text
    <*> arbitraryReducedMaybe n -- v1alpha1SubjectNamespace :: Maybe Text
  
instance Arbitrary V1alpha1VolumeAttachment where
  arbitrary = sized genV1alpha1VolumeAttachment

genV1alpha1VolumeAttachment :: Int -> Gen V1alpha1VolumeAttachment
genV1alpha1VolumeAttachment n =
  V1alpha1VolumeAttachment
    <$> arbitraryReducedMaybe n -- v1alpha1VolumeAttachmentApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1VolumeAttachmentKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1VolumeAttachmentMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1alpha1VolumeAttachmentSpec :: V1alpha1VolumeAttachmentSpec
    <*> arbitraryReducedMaybe n -- v1alpha1VolumeAttachmentStatus :: Maybe V1alpha1VolumeAttachmentStatus
  
instance Arbitrary V1alpha1VolumeAttachmentList where
  arbitrary = sized genV1alpha1VolumeAttachmentList

genV1alpha1VolumeAttachmentList :: Int -> Gen V1alpha1VolumeAttachmentList
genV1alpha1VolumeAttachmentList n =
  V1alpha1VolumeAttachmentList
    <$> arbitraryReducedMaybe n -- v1alpha1VolumeAttachmentListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1alpha1VolumeAttachmentListItems :: [V1alpha1VolumeAttachment]
    <*> arbitraryReducedMaybe n -- v1alpha1VolumeAttachmentListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1VolumeAttachmentListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1alpha1VolumeAttachmentSource where
  arbitrary = sized genV1alpha1VolumeAttachmentSource

genV1alpha1VolumeAttachmentSource :: Int -> Gen V1alpha1VolumeAttachmentSource
genV1alpha1VolumeAttachmentSource n =
  V1alpha1VolumeAttachmentSource
    <$> arbitraryReducedMaybe n -- v1alpha1VolumeAttachmentSourcePersistentVolumeName :: Maybe Text
  
instance Arbitrary V1alpha1VolumeAttachmentSpec where
  arbitrary = sized genV1alpha1VolumeAttachmentSpec

genV1alpha1VolumeAttachmentSpec :: Int -> Gen V1alpha1VolumeAttachmentSpec
genV1alpha1VolumeAttachmentSpec n =
  V1alpha1VolumeAttachmentSpec
    <$> arbitrary -- v1alpha1VolumeAttachmentSpecAttacher :: Text
    <*> arbitrary -- v1alpha1VolumeAttachmentSpecNodeName :: Text
    <*> arbitraryReduced n -- v1alpha1VolumeAttachmentSpecSource :: V1alpha1VolumeAttachmentSource
  
instance Arbitrary V1alpha1VolumeAttachmentStatus where
  arbitrary = sized genV1alpha1VolumeAttachmentStatus

genV1alpha1VolumeAttachmentStatus :: Int -> Gen V1alpha1VolumeAttachmentStatus
genV1alpha1VolumeAttachmentStatus n =
  V1alpha1VolumeAttachmentStatus
    <$> arbitraryReducedMaybe n -- v1alpha1VolumeAttachmentStatusAttachError :: Maybe V1alpha1VolumeError
    <*> arbitrary -- v1alpha1VolumeAttachmentStatusAttached :: Bool
    <*> arbitraryReducedMaybe n -- v1alpha1VolumeAttachmentStatusAttachmentMetadata :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1alpha1VolumeAttachmentStatusDetachError :: Maybe V1alpha1VolumeError
  
instance Arbitrary V1alpha1VolumeError where
  arbitrary = sized genV1alpha1VolumeError

genV1alpha1VolumeError :: Int -> Gen V1alpha1VolumeError
genV1alpha1VolumeError n =
  V1alpha1VolumeError
    <$> arbitraryReducedMaybe n -- v1alpha1VolumeErrorMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1VolumeErrorTime :: Maybe DateTime
  
instance Arbitrary V1beta1APIService where
  arbitrary = sized genV1beta1APIService

genV1beta1APIService :: Int -> Gen V1beta1APIService
genV1beta1APIService n =
  V1beta1APIService
    <$> arbitraryReducedMaybe n -- v1beta1APIServiceApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1APIServiceKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1APIServiceMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta1APIServiceSpec :: Maybe V1beta1APIServiceSpec
    <*> arbitraryReducedMaybe n -- v1beta1APIServiceStatus :: Maybe V1beta1APIServiceStatus
  
instance Arbitrary V1beta1APIServiceCondition where
  arbitrary = sized genV1beta1APIServiceCondition

genV1beta1APIServiceCondition :: Int -> Gen V1beta1APIServiceCondition
genV1beta1APIServiceCondition n =
  V1beta1APIServiceCondition
    <$> arbitraryReducedMaybe n -- v1beta1APIServiceConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1beta1APIServiceConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1APIServiceConditionReason :: Maybe Text
    <*> arbitrary -- v1beta1APIServiceConditionStatus :: Text
    <*> arbitrary -- v1beta1APIServiceConditionType :: Text
  
instance Arbitrary V1beta1APIServiceList where
  arbitrary = sized genV1beta1APIServiceList

genV1beta1APIServiceList :: Int -> Gen V1beta1APIServiceList
genV1beta1APIServiceList n =
  V1beta1APIServiceList
    <$> arbitraryReducedMaybe n -- v1beta1APIServiceListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta1APIServiceListItems :: [V1beta1APIService]
    <*> arbitraryReducedMaybe n -- v1beta1APIServiceListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1APIServiceListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta1APIServiceSpec where
  arbitrary = sized genV1beta1APIServiceSpec

genV1beta1APIServiceSpec :: Int -> Gen V1beta1APIServiceSpec
genV1beta1APIServiceSpec n =
  V1beta1APIServiceSpec
    <$> arbitraryReduced n -- v1beta1APIServiceSpecCaBundle :: ByteArray
    <*> arbitraryReducedMaybe n -- v1beta1APIServiceSpecGroup :: Maybe Text
    <*> arbitrary -- v1beta1APIServiceSpecGroupPriorityMinimum :: Int
    <*> arbitraryReducedMaybe n -- v1beta1APIServiceSpecInsecureSkipTlsVerify :: Maybe Bool
    <*> arbitraryReduced n -- v1beta1APIServiceSpecService :: ApiregistrationV1beta1ServiceReference
    <*> arbitraryReducedMaybe n -- v1beta1APIServiceSpecVersion :: Maybe Text
    <*> arbitrary -- v1beta1APIServiceSpecVersionPriority :: Int
  
instance Arbitrary V1beta1APIServiceStatus where
  arbitrary = sized genV1beta1APIServiceStatus

genV1beta1APIServiceStatus :: Int -> Gen V1beta1APIServiceStatus
genV1beta1APIServiceStatus n =
  V1beta1APIServiceStatus
    <$> arbitraryReducedMaybe n -- v1beta1APIServiceStatusConditions :: Maybe [V1beta1APIServiceCondition]
  
instance Arbitrary V1beta1AggregationRule where
  arbitrary = sized genV1beta1AggregationRule

genV1beta1AggregationRule :: Int -> Gen V1beta1AggregationRule
genV1beta1AggregationRule n =
  V1beta1AggregationRule
    <$> arbitraryReducedMaybe n -- v1beta1AggregationRuleClusterRoleSelectors :: Maybe [V1LabelSelector]
  
instance Arbitrary V1beta1AllowedFlexVolume where
  arbitrary = sized genV1beta1AllowedFlexVolume

genV1beta1AllowedFlexVolume :: Int -> Gen V1beta1AllowedFlexVolume
genV1beta1AllowedFlexVolume n =
  V1beta1AllowedFlexVolume
    <$> arbitrary -- v1beta1AllowedFlexVolumeDriver :: Text
  
instance Arbitrary V1beta1AllowedHostPath where
  arbitrary = sized genV1beta1AllowedHostPath

genV1beta1AllowedHostPath :: Int -> Gen V1beta1AllowedHostPath
genV1beta1AllowedHostPath n =
  V1beta1AllowedHostPath
    <$> arbitraryReducedMaybe n -- v1beta1AllowedHostPathPathPrefix :: Maybe Text
  
instance Arbitrary V1beta1CertificateSigningRequest where
  arbitrary = sized genV1beta1CertificateSigningRequest

genV1beta1CertificateSigningRequest :: Int -> Gen V1beta1CertificateSigningRequest
genV1beta1CertificateSigningRequest n =
  V1beta1CertificateSigningRequest
    <$> arbitraryReducedMaybe n -- v1beta1CertificateSigningRequestApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1CertificateSigningRequestKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1CertificateSigningRequestMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta1CertificateSigningRequestSpec :: Maybe V1beta1CertificateSigningRequestSpec
    <*> arbitraryReducedMaybe n -- v1beta1CertificateSigningRequestStatus :: Maybe V1beta1CertificateSigningRequestStatus
  
instance Arbitrary V1beta1CertificateSigningRequestCondition where
  arbitrary = sized genV1beta1CertificateSigningRequestCondition

genV1beta1CertificateSigningRequestCondition :: Int -> Gen V1beta1CertificateSigningRequestCondition
genV1beta1CertificateSigningRequestCondition n =
  V1beta1CertificateSigningRequestCondition
    <$> arbitraryReducedMaybe n -- v1beta1CertificateSigningRequestConditionLastUpdateTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1beta1CertificateSigningRequestConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1CertificateSigningRequestConditionReason :: Maybe Text
    <*> arbitrary -- v1beta1CertificateSigningRequestConditionType :: Text
  
instance Arbitrary V1beta1CertificateSigningRequestList where
  arbitrary = sized genV1beta1CertificateSigningRequestList

genV1beta1CertificateSigningRequestList :: Int -> Gen V1beta1CertificateSigningRequestList
genV1beta1CertificateSigningRequestList n =
  V1beta1CertificateSigningRequestList
    <$> arbitraryReducedMaybe n -- v1beta1CertificateSigningRequestListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta1CertificateSigningRequestListItems :: [V1beta1CertificateSigningRequest]
    <*> arbitraryReducedMaybe n -- v1beta1CertificateSigningRequestListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1CertificateSigningRequestListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta1CertificateSigningRequestSpec where
  arbitrary = sized genV1beta1CertificateSigningRequestSpec

genV1beta1CertificateSigningRequestSpec :: Int -> Gen V1beta1CertificateSigningRequestSpec
genV1beta1CertificateSigningRequestSpec n =
  V1beta1CertificateSigningRequestSpec
    <$> arbitraryReducedMaybe n -- v1beta1CertificateSigningRequestSpecExtra :: Maybe (Map.Map String [Text])
    <*> arbitraryReducedMaybe n -- v1beta1CertificateSigningRequestSpecGroups :: Maybe [Text]
    <*> arbitraryReduced n -- v1beta1CertificateSigningRequestSpecRequest :: ByteArray
    <*> arbitraryReducedMaybe n -- v1beta1CertificateSigningRequestSpecUid :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1CertificateSigningRequestSpecUsages :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1beta1CertificateSigningRequestSpecUsername :: Maybe Text
  
instance Arbitrary V1beta1CertificateSigningRequestStatus where
  arbitrary = sized genV1beta1CertificateSigningRequestStatus

genV1beta1CertificateSigningRequestStatus :: Int -> Gen V1beta1CertificateSigningRequestStatus
genV1beta1CertificateSigningRequestStatus n =
  V1beta1CertificateSigningRequestStatus
    <$> arbitraryReducedMaybe n -- v1beta1CertificateSigningRequestStatusCertificate :: Maybe ByteArray
    <*> arbitraryReducedMaybe n -- v1beta1CertificateSigningRequestStatusConditions :: Maybe [V1beta1CertificateSigningRequestCondition]
  
instance Arbitrary V1beta1ClusterRole where
  arbitrary = sized genV1beta1ClusterRole

genV1beta1ClusterRole :: Int -> Gen V1beta1ClusterRole
genV1beta1ClusterRole n =
  V1beta1ClusterRole
    <$> arbitraryReducedMaybe n -- v1beta1ClusterRoleAggregationRule :: Maybe V1beta1AggregationRule
    <*> arbitraryReducedMaybe n -- v1beta1ClusterRoleApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ClusterRoleKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ClusterRoleMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1beta1ClusterRoleRules :: [V1beta1PolicyRule]
  
instance Arbitrary V1beta1ClusterRoleBinding where
  arbitrary = sized genV1beta1ClusterRoleBinding

genV1beta1ClusterRoleBinding :: Int -> Gen V1beta1ClusterRoleBinding
genV1beta1ClusterRoleBinding n =
  V1beta1ClusterRoleBinding
    <$> arbitraryReducedMaybe n -- v1beta1ClusterRoleBindingApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ClusterRoleBindingKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ClusterRoleBindingMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1beta1ClusterRoleBindingRoleRef :: V1beta1RoleRef
    <*> arbitraryReduced n -- v1beta1ClusterRoleBindingSubjects :: [V1beta1Subject]
  
instance Arbitrary V1beta1ClusterRoleBindingList where
  arbitrary = sized genV1beta1ClusterRoleBindingList

genV1beta1ClusterRoleBindingList :: Int -> Gen V1beta1ClusterRoleBindingList
genV1beta1ClusterRoleBindingList n =
  V1beta1ClusterRoleBindingList
    <$> arbitraryReducedMaybe n -- v1beta1ClusterRoleBindingListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta1ClusterRoleBindingListItems :: [V1beta1ClusterRoleBinding]
    <*> arbitraryReducedMaybe n -- v1beta1ClusterRoleBindingListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ClusterRoleBindingListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta1ClusterRoleList where
  arbitrary = sized genV1beta1ClusterRoleList

genV1beta1ClusterRoleList :: Int -> Gen V1beta1ClusterRoleList
genV1beta1ClusterRoleList n =
  V1beta1ClusterRoleList
    <$> arbitraryReducedMaybe n -- v1beta1ClusterRoleListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta1ClusterRoleListItems :: [V1beta1ClusterRole]
    <*> arbitraryReducedMaybe n -- v1beta1ClusterRoleListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ClusterRoleListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta1ControllerRevision where
  arbitrary = sized genV1beta1ControllerRevision

genV1beta1ControllerRevision :: Int -> Gen V1beta1ControllerRevision
genV1beta1ControllerRevision n =
  V1beta1ControllerRevision
    <$> arbitraryReducedMaybe n -- v1beta1ControllerRevisionApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ControllerRevisionData :: Maybe RuntimeRawExtension
    <*> arbitraryReducedMaybe n -- v1beta1ControllerRevisionKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ControllerRevisionMetadata :: Maybe V1ObjectMeta
    <*> arbitrary -- v1beta1ControllerRevisionRevision :: Integer
  
instance Arbitrary V1beta1ControllerRevisionList where
  arbitrary = sized genV1beta1ControllerRevisionList

genV1beta1ControllerRevisionList :: Int -> Gen V1beta1ControllerRevisionList
genV1beta1ControllerRevisionList n =
  V1beta1ControllerRevisionList
    <$> arbitraryReducedMaybe n -- v1beta1ControllerRevisionListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta1ControllerRevisionListItems :: [V1beta1ControllerRevision]
    <*> arbitraryReducedMaybe n -- v1beta1ControllerRevisionListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ControllerRevisionListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta1CronJob where
  arbitrary = sized genV1beta1CronJob

genV1beta1CronJob :: Int -> Gen V1beta1CronJob
genV1beta1CronJob n =
  V1beta1CronJob
    <$> arbitraryReducedMaybe n -- v1beta1CronJobApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1CronJobKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1CronJobMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta1CronJobSpec :: Maybe V1beta1CronJobSpec
    <*> arbitraryReducedMaybe n -- v1beta1CronJobStatus :: Maybe V1beta1CronJobStatus
  
instance Arbitrary V1beta1CronJobList where
  arbitrary = sized genV1beta1CronJobList

genV1beta1CronJobList :: Int -> Gen V1beta1CronJobList
genV1beta1CronJobList n =
  V1beta1CronJobList
    <$> arbitraryReducedMaybe n -- v1beta1CronJobListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta1CronJobListItems :: [V1beta1CronJob]
    <*> arbitraryReducedMaybe n -- v1beta1CronJobListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1CronJobListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta1CronJobSpec where
  arbitrary = sized genV1beta1CronJobSpec

genV1beta1CronJobSpec :: Int -> Gen V1beta1CronJobSpec
genV1beta1CronJobSpec n =
  V1beta1CronJobSpec
    <$> arbitraryReducedMaybe n -- v1beta1CronJobSpecConcurrencyPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1CronJobSpecFailedJobsHistoryLimit :: Maybe Int
    <*> arbitraryReduced n -- v1beta1CronJobSpecJobTemplate :: V1beta1JobTemplateSpec
    <*> arbitrary -- v1beta1CronJobSpecSchedule :: Text
    <*> arbitraryReducedMaybe n -- v1beta1CronJobSpecStartingDeadlineSeconds :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1beta1CronJobSpecSuccessfulJobsHistoryLimit :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta1CronJobSpecSuspend :: Maybe Bool
  
instance Arbitrary V1beta1CronJobStatus where
  arbitrary = sized genV1beta1CronJobStatus

genV1beta1CronJobStatus :: Int -> Gen V1beta1CronJobStatus
genV1beta1CronJobStatus n =
  V1beta1CronJobStatus
    <$> arbitraryReducedMaybe n -- v1beta1CronJobStatusActive :: Maybe [V1ObjectReference]
    <*> arbitraryReducedMaybe n -- v1beta1CronJobStatusLastScheduleTime :: Maybe DateTime
  
instance Arbitrary V1beta1CustomResourceDefinition where
  arbitrary = sized genV1beta1CustomResourceDefinition

genV1beta1CustomResourceDefinition :: Int -> Gen V1beta1CustomResourceDefinition
genV1beta1CustomResourceDefinition n =
  V1beta1CustomResourceDefinition
    <$> arbitraryReducedMaybe n -- v1beta1CustomResourceDefinitionApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1CustomResourceDefinitionKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1CustomResourceDefinitionMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta1CustomResourceDefinitionSpec :: Maybe V1beta1CustomResourceDefinitionSpec
    <*> arbitraryReducedMaybe n -- v1beta1CustomResourceDefinitionStatus :: Maybe V1beta1CustomResourceDefinitionStatus
  
instance Arbitrary V1beta1CustomResourceDefinitionCondition where
  arbitrary = sized genV1beta1CustomResourceDefinitionCondition

genV1beta1CustomResourceDefinitionCondition :: Int -> Gen V1beta1CustomResourceDefinitionCondition
genV1beta1CustomResourceDefinitionCondition n =
  V1beta1CustomResourceDefinitionCondition
    <$> arbitraryReducedMaybe n -- v1beta1CustomResourceDefinitionConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1beta1CustomResourceDefinitionConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1CustomResourceDefinitionConditionReason :: Maybe Text
    <*> arbitrary -- v1beta1CustomResourceDefinitionConditionStatus :: Text
    <*> arbitrary -- v1beta1CustomResourceDefinitionConditionType :: Text
  
instance Arbitrary V1beta1CustomResourceDefinitionList where
  arbitrary = sized genV1beta1CustomResourceDefinitionList

genV1beta1CustomResourceDefinitionList :: Int -> Gen V1beta1CustomResourceDefinitionList
genV1beta1CustomResourceDefinitionList n =
  V1beta1CustomResourceDefinitionList
    <$> arbitraryReducedMaybe n -- v1beta1CustomResourceDefinitionListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta1CustomResourceDefinitionListItems :: [V1beta1CustomResourceDefinition]
    <*> arbitraryReducedMaybe n -- v1beta1CustomResourceDefinitionListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1CustomResourceDefinitionListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta1CustomResourceDefinitionNames where
  arbitrary = sized genV1beta1CustomResourceDefinitionNames

genV1beta1CustomResourceDefinitionNames :: Int -> Gen V1beta1CustomResourceDefinitionNames
genV1beta1CustomResourceDefinitionNames n =
  V1beta1CustomResourceDefinitionNames
    <$> arbitrary -- v1beta1CustomResourceDefinitionNamesKind :: Text
    <*> arbitraryReducedMaybe n -- v1beta1CustomResourceDefinitionNamesListKind :: Maybe Text
    <*> arbitrary -- v1beta1CustomResourceDefinitionNamesPlural :: Text
    <*> arbitraryReducedMaybe n -- v1beta1CustomResourceDefinitionNamesShortNames :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1beta1CustomResourceDefinitionNamesSingular :: Maybe Text
  
instance Arbitrary V1beta1CustomResourceDefinitionSpec where
  arbitrary = sized genV1beta1CustomResourceDefinitionSpec

genV1beta1CustomResourceDefinitionSpec :: Int -> Gen V1beta1CustomResourceDefinitionSpec
genV1beta1CustomResourceDefinitionSpec n =
  V1beta1CustomResourceDefinitionSpec
    <$> arbitrary -- v1beta1CustomResourceDefinitionSpecGroup :: Text
    <*> arbitraryReduced n -- v1beta1CustomResourceDefinitionSpecNames :: V1beta1CustomResourceDefinitionNames
    <*> arbitrary -- v1beta1CustomResourceDefinitionSpecScope :: Text
    <*> arbitraryReducedMaybe n -- v1beta1CustomResourceDefinitionSpecValidation :: Maybe V1beta1CustomResourceValidation
    <*> arbitrary -- v1beta1CustomResourceDefinitionSpecVersion :: Text
  
instance Arbitrary V1beta1CustomResourceDefinitionStatus where
  arbitrary = sized genV1beta1CustomResourceDefinitionStatus

genV1beta1CustomResourceDefinitionStatus :: Int -> Gen V1beta1CustomResourceDefinitionStatus
genV1beta1CustomResourceDefinitionStatus n =
  V1beta1CustomResourceDefinitionStatus
    <$> arbitraryReduced n -- v1beta1CustomResourceDefinitionStatusAcceptedNames :: V1beta1CustomResourceDefinitionNames
    <*> arbitraryReduced n -- v1beta1CustomResourceDefinitionStatusConditions :: [V1beta1CustomResourceDefinitionCondition]
  
instance Arbitrary V1beta1CustomResourceValidation where
  arbitrary = sized genV1beta1CustomResourceValidation

genV1beta1CustomResourceValidation :: Int -> Gen V1beta1CustomResourceValidation
genV1beta1CustomResourceValidation n =
  V1beta1CustomResourceValidation
    <$> arbitraryReducedMaybe n -- v1beta1CustomResourceValidationOpenApiv3Schema :: Maybe V1beta1JSONSchemaProps
  
instance Arbitrary V1beta1DaemonSet where
  arbitrary = sized genV1beta1DaemonSet

genV1beta1DaemonSet :: Int -> Gen V1beta1DaemonSet
genV1beta1DaemonSet n =
  V1beta1DaemonSet
    <$> arbitraryReducedMaybe n -- v1beta1DaemonSetApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1DaemonSetKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1DaemonSetMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta1DaemonSetSpec :: Maybe V1beta1DaemonSetSpec
    <*> arbitraryReducedMaybe n -- v1beta1DaemonSetStatus :: Maybe V1beta1DaemonSetStatus
  
instance Arbitrary V1beta1DaemonSetCondition where
  arbitrary = sized genV1beta1DaemonSetCondition

genV1beta1DaemonSetCondition :: Int -> Gen V1beta1DaemonSetCondition
genV1beta1DaemonSetCondition n =
  V1beta1DaemonSetCondition
    <$> arbitraryReducedMaybe n -- v1beta1DaemonSetConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1beta1DaemonSetConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1DaemonSetConditionReason :: Maybe Text
    <*> arbitrary -- v1beta1DaemonSetConditionStatus :: Text
    <*> arbitrary -- v1beta1DaemonSetConditionType :: Text
  
instance Arbitrary V1beta1DaemonSetList where
  arbitrary = sized genV1beta1DaemonSetList

genV1beta1DaemonSetList :: Int -> Gen V1beta1DaemonSetList
genV1beta1DaemonSetList n =
  V1beta1DaemonSetList
    <$> arbitraryReducedMaybe n -- v1beta1DaemonSetListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta1DaemonSetListItems :: [V1beta1DaemonSet]
    <*> arbitraryReducedMaybe n -- v1beta1DaemonSetListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1DaemonSetListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta1DaemonSetSpec where
  arbitrary = sized genV1beta1DaemonSetSpec

genV1beta1DaemonSetSpec :: Int -> Gen V1beta1DaemonSetSpec
genV1beta1DaemonSetSpec n =
  V1beta1DaemonSetSpec
    <$> arbitraryReducedMaybe n -- v1beta1DaemonSetSpecMinReadySeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta1DaemonSetSpecRevisionHistoryLimit :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta1DaemonSetSpecSelector :: Maybe V1LabelSelector
    <*> arbitraryReduced n -- v1beta1DaemonSetSpecTemplate :: V1PodTemplateSpec
    <*> arbitraryReducedMaybe n -- v1beta1DaemonSetSpecTemplateGeneration :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1beta1DaemonSetSpecUpdateStrategy :: Maybe V1beta1DaemonSetUpdateStrategy
  
instance Arbitrary V1beta1DaemonSetStatus where
  arbitrary = sized genV1beta1DaemonSetStatus

genV1beta1DaemonSetStatus :: Int -> Gen V1beta1DaemonSetStatus
genV1beta1DaemonSetStatus n =
  V1beta1DaemonSetStatus
    <$> arbitraryReducedMaybe n -- v1beta1DaemonSetStatusCollisionCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta1DaemonSetStatusConditions :: Maybe [V1beta1DaemonSetCondition]
    <*> arbitrary -- v1beta1DaemonSetStatusCurrentNumberScheduled :: Int
    <*> arbitrary -- v1beta1DaemonSetStatusDesiredNumberScheduled :: Int
    <*> arbitraryReducedMaybe n -- v1beta1DaemonSetStatusNumberAvailable :: Maybe Int
    <*> arbitrary -- v1beta1DaemonSetStatusNumberMisscheduled :: Int
    <*> arbitrary -- v1beta1DaemonSetStatusNumberReady :: Int
    <*> arbitraryReducedMaybe n -- v1beta1DaemonSetStatusNumberUnavailable :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta1DaemonSetStatusObservedGeneration :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1beta1DaemonSetStatusUpdatedNumberScheduled :: Maybe Int
  
instance Arbitrary V1beta1DaemonSetUpdateStrategy where
  arbitrary = sized genV1beta1DaemonSetUpdateStrategy

genV1beta1DaemonSetUpdateStrategy :: Int -> Gen V1beta1DaemonSetUpdateStrategy
genV1beta1DaemonSetUpdateStrategy n =
  V1beta1DaemonSetUpdateStrategy
    <$> arbitraryReducedMaybe n -- v1beta1DaemonSetUpdateStrategyRollingUpdate :: Maybe V1beta1RollingUpdateDaemonSet
    <*> arbitraryReducedMaybe n -- v1beta1DaemonSetUpdateStrategyType :: Maybe Text
  
instance Arbitrary V1beta1Event where
  arbitrary = sized genV1beta1Event

genV1beta1Event :: Int -> Gen V1beta1Event
genV1beta1Event n =
  V1beta1Event
    <$> arbitraryReducedMaybe n -- v1beta1EventAction :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1EventApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1EventDeprecatedCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta1EventDeprecatedFirstTimestamp :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1beta1EventDeprecatedLastTimestamp :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1beta1EventDeprecatedSource :: Maybe V1EventSource
    <*> arbitraryReduced n -- v1beta1EventEventTime :: DateTime
    <*> arbitraryReducedMaybe n -- v1beta1EventKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1EventMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta1EventNote :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1EventReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1EventRegarding :: Maybe V1ObjectReference
    <*> arbitraryReducedMaybe n -- v1beta1EventRelated :: Maybe V1ObjectReference
    <*> arbitraryReducedMaybe n -- v1beta1EventReportingController :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1EventReportingInstance :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1EventSeries :: Maybe V1beta1EventSeries
    <*> arbitraryReducedMaybe n -- v1beta1EventType :: Maybe Text
  
instance Arbitrary V1beta1EventList where
  arbitrary = sized genV1beta1EventList

genV1beta1EventList :: Int -> Gen V1beta1EventList
genV1beta1EventList n =
  V1beta1EventList
    <$> arbitraryReducedMaybe n -- v1beta1EventListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta1EventListItems :: [V1beta1Event]
    <*> arbitraryReducedMaybe n -- v1beta1EventListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1EventListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta1EventSeries where
  arbitrary = sized genV1beta1EventSeries

genV1beta1EventSeries :: Int -> Gen V1beta1EventSeries
genV1beta1EventSeries n =
  V1beta1EventSeries
    <$> arbitrary -- v1beta1EventSeriesCount :: Int
    <*> arbitraryReduced n -- v1beta1EventSeriesLastObservedTime :: DateTime
    <*> arbitrary -- v1beta1EventSeriesState :: Text
  
instance Arbitrary V1beta1Eviction where
  arbitrary = sized genV1beta1Eviction

genV1beta1Eviction :: Int -> Gen V1beta1Eviction
genV1beta1Eviction n =
  V1beta1Eviction
    <$> arbitraryReducedMaybe n -- v1beta1EvictionApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1EvictionDeleteOptions :: Maybe V1DeleteOptions
    <*> arbitraryReducedMaybe n -- v1beta1EvictionKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1EvictionMetadata :: Maybe V1ObjectMeta
  
instance Arbitrary V1beta1ExternalDocumentation where
  arbitrary = sized genV1beta1ExternalDocumentation

genV1beta1ExternalDocumentation :: Int -> Gen V1beta1ExternalDocumentation
genV1beta1ExternalDocumentation n =
  V1beta1ExternalDocumentation
    <$> arbitraryReducedMaybe n -- v1beta1ExternalDocumentationDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ExternalDocumentationUrl :: Maybe Text
  
instance Arbitrary V1beta1FSGroupStrategyOptions where
  arbitrary = sized genV1beta1FSGroupStrategyOptions

genV1beta1FSGroupStrategyOptions :: Int -> Gen V1beta1FSGroupStrategyOptions
genV1beta1FSGroupStrategyOptions n =
  V1beta1FSGroupStrategyOptions
    <$> arbitraryReducedMaybe n -- v1beta1FSGroupStrategyOptionsRanges :: Maybe [V1beta1IDRange]
    <*> arbitraryReducedMaybe n -- v1beta1FSGroupStrategyOptionsRule :: Maybe Text
  
instance Arbitrary V1beta1HTTPIngressPath where
  arbitrary = sized genV1beta1HTTPIngressPath

genV1beta1HTTPIngressPath :: Int -> Gen V1beta1HTTPIngressPath
genV1beta1HTTPIngressPath n =
  V1beta1HTTPIngressPath
    <$> arbitraryReduced n -- v1beta1HTTPIngressPathBackend :: V1beta1IngressBackend
    <*> arbitraryReducedMaybe n -- v1beta1HTTPIngressPathPath :: Maybe Text
  
instance Arbitrary V1beta1HTTPIngressRuleValue where
  arbitrary = sized genV1beta1HTTPIngressRuleValue

genV1beta1HTTPIngressRuleValue :: Int -> Gen V1beta1HTTPIngressRuleValue
genV1beta1HTTPIngressRuleValue n =
  V1beta1HTTPIngressRuleValue
    <$> arbitraryReduced n -- v1beta1HTTPIngressRuleValuePaths :: [V1beta1HTTPIngressPath]
  
instance Arbitrary V1beta1HostPortRange where
  arbitrary = sized genV1beta1HostPortRange

genV1beta1HostPortRange :: Int -> Gen V1beta1HostPortRange
genV1beta1HostPortRange n =
  V1beta1HostPortRange
    <$> arbitrary -- v1beta1HostPortRangeMax :: Int
    <*> arbitrary -- v1beta1HostPortRangeMin :: Int
  
instance Arbitrary V1beta1IDRange where
  arbitrary = sized genV1beta1IDRange

genV1beta1IDRange :: Int -> Gen V1beta1IDRange
genV1beta1IDRange n =
  V1beta1IDRange
    <$> arbitrary -- v1beta1IDRangeMax :: Integer
    <*> arbitrary -- v1beta1IDRangeMin :: Integer
  
instance Arbitrary V1beta1IPBlock where
  arbitrary = sized genV1beta1IPBlock

genV1beta1IPBlock :: Int -> Gen V1beta1IPBlock
genV1beta1IPBlock n =
  V1beta1IPBlock
    <$> arbitrary -- v1beta1IPBlockCidr :: Text
    <*> arbitraryReducedMaybe n -- v1beta1IPBlockExcept :: Maybe [Text]
  
instance Arbitrary V1beta1Ingress where
  arbitrary = sized genV1beta1Ingress

genV1beta1Ingress :: Int -> Gen V1beta1Ingress
genV1beta1Ingress n =
  V1beta1Ingress
    <$> arbitraryReducedMaybe n -- v1beta1IngressApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1IngressKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1IngressMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta1IngressSpec :: Maybe V1beta1IngressSpec
    <*> arbitraryReducedMaybe n -- v1beta1IngressStatus :: Maybe V1beta1IngressStatus
  
instance Arbitrary V1beta1IngressBackend where
  arbitrary = sized genV1beta1IngressBackend

genV1beta1IngressBackend :: Int -> Gen V1beta1IngressBackend
genV1beta1IngressBackend n =
  V1beta1IngressBackend
    <$> arbitrary -- v1beta1IngressBackendServiceName :: Text
    <*> arbitraryReduced n -- v1beta1IngressBackendServicePort :: A.Value
  
instance Arbitrary V1beta1IngressList where
  arbitrary = sized genV1beta1IngressList

genV1beta1IngressList :: Int -> Gen V1beta1IngressList
genV1beta1IngressList n =
  V1beta1IngressList
    <$> arbitraryReducedMaybe n -- v1beta1IngressListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta1IngressListItems :: [V1beta1Ingress]
    <*> arbitraryReducedMaybe n -- v1beta1IngressListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1IngressListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta1IngressRule where
  arbitrary = sized genV1beta1IngressRule

genV1beta1IngressRule :: Int -> Gen V1beta1IngressRule
genV1beta1IngressRule n =
  V1beta1IngressRule
    <$> arbitraryReducedMaybe n -- v1beta1IngressRuleHost :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1IngressRuleHttp :: Maybe V1beta1HTTPIngressRuleValue
  
instance Arbitrary V1beta1IngressSpec where
  arbitrary = sized genV1beta1IngressSpec

genV1beta1IngressSpec :: Int -> Gen V1beta1IngressSpec
genV1beta1IngressSpec n =
  V1beta1IngressSpec
    <$> arbitraryReducedMaybe n -- v1beta1IngressSpecBackend :: Maybe V1beta1IngressBackend
    <*> arbitraryReducedMaybe n -- v1beta1IngressSpecRules :: Maybe [V1beta1IngressRule]
    <*> arbitraryReducedMaybe n -- v1beta1IngressSpecTls :: Maybe [V1beta1IngressTLS]
  
instance Arbitrary V1beta1IngressStatus where
  arbitrary = sized genV1beta1IngressStatus

genV1beta1IngressStatus :: Int -> Gen V1beta1IngressStatus
genV1beta1IngressStatus n =
  V1beta1IngressStatus
    <$> arbitraryReducedMaybe n -- v1beta1IngressStatusLoadBalancer :: Maybe V1LoadBalancerStatus
  
instance Arbitrary V1beta1IngressTLS where
  arbitrary = sized genV1beta1IngressTLS

genV1beta1IngressTLS :: Int -> Gen V1beta1IngressTLS
genV1beta1IngressTLS n =
  V1beta1IngressTLS
    <$> arbitraryReducedMaybe n -- v1beta1IngressTLSHosts :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1beta1IngressTLSSecretName :: Maybe Text
  
instance Arbitrary V1beta1JSON where
  arbitrary = sized genV1beta1JSON

genV1beta1JSON :: Int -> Gen V1beta1JSON
genV1beta1JSON n =
  V1beta1JSON
    <$> arbitraryReduced n -- v1beta1JSONRaw :: ByteArray
  
instance Arbitrary V1beta1JSONSchemaProps where
  arbitrary = sized genV1beta1JSONSchemaProps

genV1beta1JSONSchemaProps :: Int -> Gen V1beta1JSONSchemaProps
genV1beta1JSONSchemaProps n =
  V1beta1JSONSchemaProps
    <$> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsRef :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsSchema :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsAdditionalItems :: Maybe V1beta1JSONSchemaPropsOrBool
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsAdditionalProperties :: Maybe V1beta1JSONSchemaPropsOrBool
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsAllOf :: Maybe [V1beta1JSONSchemaProps]
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsAnyOf :: Maybe [V1beta1JSONSchemaProps]
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsDefault :: Maybe V1beta1JSON
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsDefinitions :: Maybe (Map.Map String V1beta1JSONSchemaProps)
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsDependencies :: Maybe (Map.Map String V1beta1JSONSchemaPropsOrStringArray)
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsEnum :: Maybe [V1beta1JSON]
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsExample :: Maybe V1beta1JSON
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsExclusiveMaximum :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsExclusiveMinimum :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsExternalDocs :: Maybe V1beta1ExternalDocumentation
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsFormat :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsId :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsItems :: Maybe V1beta1JSONSchemaPropsOrArray
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsMaxItems :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsMaxLength :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsMaxProperties :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsMaximum :: Maybe Double
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsMinItems :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsMinLength :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsMinProperties :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsMinimum :: Maybe Double
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsMultipleOf :: Maybe Double
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsNot :: Maybe V1beta1JSONSchemaProps
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsOneOf :: Maybe [V1beta1JSONSchemaProps]
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsPattern :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsPatternProperties :: Maybe (Map.Map String V1beta1JSONSchemaProps)
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsProperties :: Maybe (Map.Map String V1beta1JSONSchemaProps)
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsRequired :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1JSONSchemaPropsUniqueItems :: Maybe Bool
  
instance Arbitrary V1beta1JSONSchemaPropsOrArray where
  arbitrary = sized genV1beta1JSONSchemaPropsOrArray

genV1beta1JSONSchemaPropsOrArray :: Int -> Gen V1beta1JSONSchemaPropsOrArray
genV1beta1JSONSchemaPropsOrArray n =
  V1beta1JSONSchemaPropsOrArray
    <$> arbitraryReduced n -- v1beta1JSONSchemaPropsOrArrayJsonSchemas :: [V1beta1JSONSchemaProps]
    <*> arbitraryReduced n -- v1beta1JSONSchemaPropsOrArraySchema :: V1beta1JSONSchemaProps
  
instance Arbitrary V1beta1JSONSchemaPropsOrBool where
  arbitrary = sized genV1beta1JSONSchemaPropsOrBool

genV1beta1JSONSchemaPropsOrBool :: Int -> Gen V1beta1JSONSchemaPropsOrBool
genV1beta1JSONSchemaPropsOrBool n =
  V1beta1JSONSchemaPropsOrBool
    <$> arbitrary -- v1beta1JSONSchemaPropsOrBoolAllows :: Bool
    <*> arbitraryReduced n -- v1beta1JSONSchemaPropsOrBoolSchema :: V1beta1JSONSchemaProps
  
instance Arbitrary V1beta1JSONSchemaPropsOrStringArray where
  arbitrary = sized genV1beta1JSONSchemaPropsOrStringArray

genV1beta1JSONSchemaPropsOrStringArray :: Int -> Gen V1beta1JSONSchemaPropsOrStringArray
genV1beta1JSONSchemaPropsOrStringArray n =
  V1beta1JSONSchemaPropsOrStringArray
    <$> arbitrary -- v1beta1JSONSchemaPropsOrStringArrayProperty :: [Text]
    <*> arbitraryReduced n -- v1beta1JSONSchemaPropsOrStringArraySchema :: V1beta1JSONSchemaProps
  
instance Arbitrary V1beta1JobTemplateSpec where
  arbitrary = sized genV1beta1JobTemplateSpec

genV1beta1JobTemplateSpec :: Int -> Gen V1beta1JobTemplateSpec
genV1beta1JobTemplateSpec n =
  V1beta1JobTemplateSpec
    <$> arbitraryReducedMaybe n -- v1beta1JobTemplateSpecMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta1JobTemplateSpecSpec :: Maybe V1JobSpec
  
instance Arbitrary V1beta1LocalSubjectAccessReview where
  arbitrary = sized genV1beta1LocalSubjectAccessReview

genV1beta1LocalSubjectAccessReview :: Int -> Gen V1beta1LocalSubjectAccessReview
genV1beta1LocalSubjectAccessReview n =
  V1beta1LocalSubjectAccessReview
    <$> arbitraryReducedMaybe n -- v1beta1LocalSubjectAccessReviewApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1LocalSubjectAccessReviewKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1LocalSubjectAccessReviewMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1beta1LocalSubjectAccessReviewSpec :: V1beta1SubjectAccessReviewSpec
    <*> arbitraryReducedMaybe n -- v1beta1LocalSubjectAccessReviewStatus :: Maybe V1beta1SubjectAccessReviewStatus
  
instance Arbitrary V1beta1MutatingWebhookConfiguration where
  arbitrary = sized genV1beta1MutatingWebhookConfiguration

genV1beta1MutatingWebhookConfiguration :: Int -> Gen V1beta1MutatingWebhookConfiguration
genV1beta1MutatingWebhookConfiguration n =
  V1beta1MutatingWebhookConfiguration
    <$> arbitraryReducedMaybe n -- v1beta1MutatingWebhookConfigurationApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1MutatingWebhookConfigurationKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1MutatingWebhookConfigurationMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta1MutatingWebhookConfigurationWebhooks :: Maybe [V1beta1Webhook]
  
instance Arbitrary V1beta1MutatingWebhookConfigurationList where
  arbitrary = sized genV1beta1MutatingWebhookConfigurationList

genV1beta1MutatingWebhookConfigurationList :: Int -> Gen V1beta1MutatingWebhookConfigurationList
genV1beta1MutatingWebhookConfigurationList n =
  V1beta1MutatingWebhookConfigurationList
    <$> arbitraryReducedMaybe n -- v1beta1MutatingWebhookConfigurationListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta1MutatingWebhookConfigurationListItems :: [V1beta1MutatingWebhookConfiguration]
    <*> arbitraryReducedMaybe n -- v1beta1MutatingWebhookConfigurationListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1MutatingWebhookConfigurationListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta1NetworkPolicy where
  arbitrary = sized genV1beta1NetworkPolicy

genV1beta1NetworkPolicy :: Int -> Gen V1beta1NetworkPolicy
genV1beta1NetworkPolicy n =
  V1beta1NetworkPolicy
    <$> arbitraryReducedMaybe n -- v1beta1NetworkPolicyApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1NetworkPolicyKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1NetworkPolicyMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta1NetworkPolicySpec :: Maybe V1beta1NetworkPolicySpec
  
instance Arbitrary V1beta1NetworkPolicyEgressRule where
  arbitrary = sized genV1beta1NetworkPolicyEgressRule

genV1beta1NetworkPolicyEgressRule :: Int -> Gen V1beta1NetworkPolicyEgressRule
genV1beta1NetworkPolicyEgressRule n =
  V1beta1NetworkPolicyEgressRule
    <$> arbitraryReducedMaybe n -- v1beta1NetworkPolicyEgressRulePorts :: Maybe [V1beta1NetworkPolicyPort]
    <*> arbitraryReducedMaybe n -- v1beta1NetworkPolicyEgressRuleTo :: Maybe [V1beta1NetworkPolicyPeer]
  
instance Arbitrary V1beta1NetworkPolicyIngressRule where
  arbitrary = sized genV1beta1NetworkPolicyIngressRule

genV1beta1NetworkPolicyIngressRule :: Int -> Gen V1beta1NetworkPolicyIngressRule
genV1beta1NetworkPolicyIngressRule n =
  V1beta1NetworkPolicyIngressRule
    <$> arbitraryReducedMaybe n -- v1beta1NetworkPolicyIngressRuleFrom :: Maybe [V1beta1NetworkPolicyPeer]
    <*> arbitraryReducedMaybe n -- v1beta1NetworkPolicyIngressRulePorts :: Maybe [V1beta1NetworkPolicyPort]
  
instance Arbitrary V1beta1NetworkPolicyList where
  arbitrary = sized genV1beta1NetworkPolicyList

genV1beta1NetworkPolicyList :: Int -> Gen V1beta1NetworkPolicyList
genV1beta1NetworkPolicyList n =
  V1beta1NetworkPolicyList
    <$> arbitraryReducedMaybe n -- v1beta1NetworkPolicyListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta1NetworkPolicyListItems :: [V1beta1NetworkPolicy]
    <*> arbitraryReducedMaybe n -- v1beta1NetworkPolicyListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1NetworkPolicyListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta1NetworkPolicyPeer where
  arbitrary = sized genV1beta1NetworkPolicyPeer

genV1beta1NetworkPolicyPeer :: Int -> Gen V1beta1NetworkPolicyPeer
genV1beta1NetworkPolicyPeer n =
  V1beta1NetworkPolicyPeer
    <$> arbitraryReducedMaybe n -- v1beta1NetworkPolicyPeerIpBlock :: Maybe V1beta1IPBlock
    <*> arbitraryReducedMaybe n -- v1beta1NetworkPolicyPeerNamespaceSelector :: Maybe V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1beta1NetworkPolicyPeerPodSelector :: Maybe V1LabelSelector
  
instance Arbitrary V1beta1NetworkPolicyPort where
  arbitrary = sized genV1beta1NetworkPolicyPort

genV1beta1NetworkPolicyPort :: Int -> Gen V1beta1NetworkPolicyPort
genV1beta1NetworkPolicyPort n =
  V1beta1NetworkPolicyPort
    <$> arbitraryReducedMaybeValue n -- v1beta1NetworkPolicyPortPort :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- v1beta1NetworkPolicyPortProtocol :: Maybe Text
  
instance Arbitrary V1beta1NetworkPolicySpec where
  arbitrary = sized genV1beta1NetworkPolicySpec

genV1beta1NetworkPolicySpec :: Int -> Gen V1beta1NetworkPolicySpec
genV1beta1NetworkPolicySpec n =
  V1beta1NetworkPolicySpec
    <$> arbitraryReducedMaybe n -- v1beta1NetworkPolicySpecEgress :: Maybe [V1beta1NetworkPolicyEgressRule]
    <*> arbitraryReducedMaybe n -- v1beta1NetworkPolicySpecIngress :: Maybe [V1beta1NetworkPolicyIngressRule]
    <*> arbitraryReduced n -- v1beta1NetworkPolicySpecPodSelector :: V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1beta1NetworkPolicySpecPolicyTypes :: Maybe [Text]
  
instance Arbitrary V1beta1NonResourceAttributes where
  arbitrary = sized genV1beta1NonResourceAttributes

genV1beta1NonResourceAttributes :: Int -> Gen V1beta1NonResourceAttributes
genV1beta1NonResourceAttributes n =
  V1beta1NonResourceAttributes
    <$> arbitraryReducedMaybe n -- v1beta1NonResourceAttributesPath :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1NonResourceAttributesVerb :: Maybe Text
  
instance Arbitrary V1beta1NonResourceRule where
  arbitrary = sized genV1beta1NonResourceRule

genV1beta1NonResourceRule :: Int -> Gen V1beta1NonResourceRule
genV1beta1NonResourceRule n =
  V1beta1NonResourceRule
    <$> arbitraryReducedMaybe n -- v1beta1NonResourceRuleNonResourceUrLs :: Maybe [Text]
    <*> arbitrary -- v1beta1NonResourceRuleVerbs :: [Text]
  
instance Arbitrary V1beta1PodDisruptionBudget where
  arbitrary = sized genV1beta1PodDisruptionBudget

genV1beta1PodDisruptionBudget :: Int -> Gen V1beta1PodDisruptionBudget
genV1beta1PodDisruptionBudget n =
  V1beta1PodDisruptionBudget
    <$> arbitraryReducedMaybe n -- v1beta1PodDisruptionBudgetApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1PodDisruptionBudgetKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1PodDisruptionBudgetMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta1PodDisruptionBudgetSpec :: Maybe V1beta1PodDisruptionBudgetSpec
    <*> arbitraryReducedMaybe n -- v1beta1PodDisruptionBudgetStatus :: Maybe V1beta1PodDisruptionBudgetStatus
  
instance Arbitrary V1beta1PodDisruptionBudgetList where
  arbitrary = sized genV1beta1PodDisruptionBudgetList

genV1beta1PodDisruptionBudgetList :: Int -> Gen V1beta1PodDisruptionBudgetList
genV1beta1PodDisruptionBudgetList n =
  V1beta1PodDisruptionBudgetList
    <$> arbitraryReducedMaybe n -- v1beta1PodDisruptionBudgetListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta1PodDisruptionBudgetListItems :: [V1beta1PodDisruptionBudget]
    <*> arbitraryReducedMaybe n -- v1beta1PodDisruptionBudgetListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1PodDisruptionBudgetListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta1PodDisruptionBudgetSpec where
  arbitrary = sized genV1beta1PodDisruptionBudgetSpec

genV1beta1PodDisruptionBudgetSpec :: Int -> Gen V1beta1PodDisruptionBudgetSpec
genV1beta1PodDisruptionBudgetSpec n =
  V1beta1PodDisruptionBudgetSpec
    <$> arbitraryReducedMaybeValue n -- v1beta1PodDisruptionBudgetSpecMaxUnavailable :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- v1beta1PodDisruptionBudgetSpecMinAvailable :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- v1beta1PodDisruptionBudgetSpecSelector :: Maybe V1LabelSelector
  
instance Arbitrary V1beta1PodDisruptionBudgetStatus where
  arbitrary = sized genV1beta1PodDisruptionBudgetStatus

genV1beta1PodDisruptionBudgetStatus :: Int -> Gen V1beta1PodDisruptionBudgetStatus
genV1beta1PodDisruptionBudgetStatus n =
  V1beta1PodDisruptionBudgetStatus
    <$> arbitrary -- v1beta1PodDisruptionBudgetStatusCurrentHealthy :: Int
    <*> arbitrary -- v1beta1PodDisruptionBudgetStatusDesiredHealthy :: Int
    <*> arbitraryReduced n -- v1beta1PodDisruptionBudgetStatusDisruptedPods :: (Map.Map String DateTime)
    <*> arbitrary -- v1beta1PodDisruptionBudgetStatusDisruptionsAllowed :: Int
    <*> arbitrary -- v1beta1PodDisruptionBudgetStatusExpectedPods :: Int
    <*> arbitraryReducedMaybe n -- v1beta1PodDisruptionBudgetStatusObservedGeneration :: Maybe Integer
  
instance Arbitrary V1beta1PodSecurityPolicy where
  arbitrary = sized genV1beta1PodSecurityPolicy

genV1beta1PodSecurityPolicy :: Int -> Gen V1beta1PodSecurityPolicy
genV1beta1PodSecurityPolicy n =
  V1beta1PodSecurityPolicy
    <$> arbitraryReducedMaybe n -- v1beta1PodSecurityPolicyApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1PodSecurityPolicyKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1PodSecurityPolicyMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta1PodSecurityPolicySpec :: Maybe V1beta1PodSecurityPolicySpec
  
instance Arbitrary V1beta1PodSecurityPolicyList where
  arbitrary = sized genV1beta1PodSecurityPolicyList

genV1beta1PodSecurityPolicyList :: Int -> Gen V1beta1PodSecurityPolicyList
genV1beta1PodSecurityPolicyList n =
  V1beta1PodSecurityPolicyList
    <$> arbitraryReducedMaybe n -- v1beta1PodSecurityPolicyListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta1PodSecurityPolicyListItems :: [V1beta1PodSecurityPolicy]
    <*> arbitraryReducedMaybe n -- v1beta1PodSecurityPolicyListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1PodSecurityPolicyListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta1PodSecurityPolicySpec where
  arbitrary = sized genV1beta1PodSecurityPolicySpec

genV1beta1PodSecurityPolicySpec :: Int -> Gen V1beta1PodSecurityPolicySpec
genV1beta1PodSecurityPolicySpec n =
  V1beta1PodSecurityPolicySpec
    <$> arbitraryReducedMaybe n -- v1beta1PodSecurityPolicySpecAllowPrivilegeEscalation :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1beta1PodSecurityPolicySpecAllowedCapabilities :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1beta1PodSecurityPolicySpecAllowedFlexVolumes :: Maybe [V1beta1AllowedFlexVolume]
    <*> arbitraryReducedMaybe n -- v1beta1PodSecurityPolicySpecAllowedHostPaths :: Maybe [V1beta1AllowedHostPath]
    <*> arbitraryReducedMaybe n -- v1beta1PodSecurityPolicySpecDefaultAddCapabilities :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1beta1PodSecurityPolicySpecDefaultAllowPrivilegeEscalation :: Maybe Bool
    <*> arbitraryReduced n -- v1beta1PodSecurityPolicySpecFsGroup :: V1beta1FSGroupStrategyOptions
    <*> arbitraryReducedMaybe n -- v1beta1PodSecurityPolicySpecHostIpc :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1beta1PodSecurityPolicySpecHostNetwork :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1beta1PodSecurityPolicySpecHostPid :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1beta1PodSecurityPolicySpecHostPorts :: Maybe [V1beta1HostPortRange]
    <*> arbitraryReducedMaybe n -- v1beta1PodSecurityPolicySpecPrivileged :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1beta1PodSecurityPolicySpecReadOnlyRootFilesystem :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1beta1PodSecurityPolicySpecRequiredDropCapabilities :: Maybe [Text]
    <*> arbitraryReduced n -- v1beta1PodSecurityPolicySpecRunAsUser :: V1beta1RunAsUserStrategyOptions
    <*> arbitraryReduced n -- v1beta1PodSecurityPolicySpecSeLinux :: V1beta1SELinuxStrategyOptions
    <*> arbitraryReduced n -- v1beta1PodSecurityPolicySpecSupplementalGroups :: V1beta1SupplementalGroupsStrategyOptions
    <*> arbitraryReducedMaybe n -- v1beta1PodSecurityPolicySpecVolumes :: Maybe [Text]
  
instance Arbitrary V1beta1PolicyRule where
  arbitrary = sized genV1beta1PolicyRule

genV1beta1PolicyRule :: Int -> Gen V1beta1PolicyRule
genV1beta1PolicyRule n =
  V1beta1PolicyRule
    <$> arbitraryReducedMaybe n -- v1beta1PolicyRuleApiGroups :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1beta1PolicyRuleNonResourceUrLs :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1beta1PolicyRuleResourceNames :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1beta1PolicyRuleResources :: Maybe [Text]
    <*> arbitrary -- v1beta1PolicyRuleVerbs :: [Text]
  
instance Arbitrary V1beta1ReplicaSet where
  arbitrary = sized genV1beta1ReplicaSet

genV1beta1ReplicaSet :: Int -> Gen V1beta1ReplicaSet
genV1beta1ReplicaSet n =
  V1beta1ReplicaSet
    <$> arbitraryReducedMaybe n -- v1beta1ReplicaSetApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ReplicaSetKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ReplicaSetMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta1ReplicaSetSpec :: Maybe V1beta1ReplicaSetSpec
    <*> arbitraryReducedMaybe n -- v1beta1ReplicaSetStatus :: Maybe V1beta1ReplicaSetStatus
  
instance Arbitrary V1beta1ReplicaSetCondition where
  arbitrary = sized genV1beta1ReplicaSetCondition

genV1beta1ReplicaSetCondition :: Int -> Gen V1beta1ReplicaSetCondition
genV1beta1ReplicaSetCondition n =
  V1beta1ReplicaSetCondition
    <$> arbitraryReducedMaybe n -- v1beta1ReplicaSetConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1beta1ReplicaSetConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ReplicaSetConditionReason :: Maybe Text
    <*> arbitrary -- v1beta1ReplicaSetConditionStatus :: Text
    <*> arbitrary -- v1beta1ReplicaSetConditionType :: Text
  
instance Arbitrary V1beta1ReplicaSetList where
  arbitrary = sized genV1beta1ReplicaSetList

genV1beta1ReplicaSetList :: Int -> Gen V1beta1ReplicaSetList
genV1beta1ReplicaSetList n =
  V1beta1ReplicaSetList
    <$> arbitraryReducedMaybe n -- v1beta1ReplicaSetListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta1ReplicaSetListItems :: [V1beta1ReplicaSet]
    <*> arbitraryReducedMaybe n -- v1beta1ReplicaSetListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ReplicaSetListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta1ReplicaSetSpec where
  arbitrary = sized genV1beta1ReplicaSetSpec

genV1beta1ReplicaSetSpec :: Int -> Gen V1beta1ReplicaSetSpec
genV1beta1ReplicaSetSpec n =
  V1beta1ReplicaSetSpec
    <$> arbitraryReducedMaybe n -- v1beta1ReplicaSetSpecMinReadySeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta1ReplicaSetSpecReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta1ReplicaSetSpecSelector :: Maybe V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1beta1ReplicaSetSpecTemplate :: Maybe V1PodTemplateSpec
  
instance Arbitrary V1beta1ReplicaSetStatus where
  arbitrary = sized genV1beta1ReplicaSetStatus

genV1beta1ReplicaSetStatus :: Int -> Gen V1beta1ReplicaSetStatus
genV1beta1ReplicaSetStatus n =
  V1beta1ReplicaSetStatus
    <$> arbitraryReducedMaybe n -- v1beta1ReplicaSetStatusAvailableReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta1ReplicaSetStatusConditions :: Maybe [V1beta1ReplicaSetCondition]
    <*> arbitraryReducedMaybe n -- v1beta1ReplicaSetStatusFullyLabeledReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta1ReplicaSetStatusObservedGeneration :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1beta1ReplicaSetStatusReadyReplicas :: Maybe Int
    <*> arbitrary -- v1beta1ReplicaSetStatusReplicas :: Int
  
instance Arbitrary V1beta1ResourceAttributes where
  arbitrary = sized genV1beta1ResourceAttributes

genV1beta1ResourceAttributes :: Int -> Gen V1beta1ResourceAttributes
genV1beta1ResourceAttributes n =
  V1beta1ResourceAttributes
    <$> arbitraryReducedMaybe n -- v1beta1ResourceAttributesGroup :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ResourceAttributesName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ResourceAttributesNamespace :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ResourceAttributesResource :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ResourceAttributesSubresource :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ResourceAttributesVerb :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ResourceAttributesVersion :: Maybe Text
  
instance Arbitrary V1beta1ResourceRule where
  arbitrary = sized genV1beta1ResourceRule

genV1beta1ResourceRule :: Int -> Gen V1beta1ResourceRule
genV1beta1ResourceRule n =
  V1beta1ResourceRule
    <$> arbitraryReducedMaybe n -- v1beta1ResourceRuleApiGroups :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1beta1ResourceRuleResourceNames :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1beta1ResourceRuleResources :: Maybe [Text]
    <*> arbitrary -- v1beta1ResourceRuleVerbs :: [Text]
  
instance Arbitrary V1beta1Role where
  arbitrary = sized genV1beta1Role

genV1beta1Role :: Int -> Gen V1beta1Role
genV1beta1Role n =
  V1beta1Role
    <$> arbitraryReducedMaybe n -- v1beta1RoleApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1RoleKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1RoleMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1beta1RoleRules :: [V1beta1PolicyRule]
  
instance Arbitrary V1beta1RoleBinding where
  arbitrary = sized genV1beta1RoleBinding

genV1beta1RoleBinding :: Int -> Gen V1beta1RoleBinding
genV1beta1RoleBinding n =
  V1beta1RoleBinding
    <$> arbitraryReducedMaybe n -- v1beta1RoleBindingApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1RoleBindingKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1RoleBindingMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1beta1RoleBindingRoleRef :: V1beta1RoleRef
    <*> arbitraryReduced n -- v1beta1RoleBindingSubjects :: [V1beta1Subject]
  
instance Arbitrary V1beta1RoleBindingList where
  arbitrary = sized genV1beta1RoleBindingList

genV1beta1RoleBindingList :: Int -> Gen V1beta1RoleBindingList
genV1beta1RoleBindingList n =
  V1beta1RoleBindingList
    <$> arbitraryReducedMaybe n -- v1beta1RoleBindingListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta1RoleBindingListItems :: [V1beta1RoleBinding]
    <*> arbitraryReducedMaybe n -- v1beta1RoleBindingListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1RoleBindingListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta1RoleList where
  arbitrary = sized genV1beta1RoleList

genV1beta1RoleList :: Int -> Gen V1beta1RoleList
genV1beta1RoleList n =
  V1beta1RoleList
    <$> arbitraryReducedMaybe n -- v1beta1RoleListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta1RoleListItems :: [V1beta1Role]
    <*> arbitraryReducedMaybe n -- v1beta1RoleListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1RoleListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta1RoleRef where
  arbitrary = sized genV1beta1RoleRef

genV1beta1RoleRef :: Int -> Gen V1beta1RoleRef
genV1beta1RoleRef n =
  V1beta1RoleRef
    <$> arbitrary -- v1beta1RoleRefApiGroup :: Text
    <*> arbitrary -- v1beta1RoleRefKind :: Text
    <*> arbitrary -- v1beta1RoleRefName :: Text
  
instance Arbitrary V1beta1RollingUpdateDaemonSet where
  arbitrary = sized genV1beta1RollingUpdateDaemonSet

genV1beta1RollingUpdateDaemonSet :: Int -> Gen V1beta1RollingUpdateDaemonSet
genV1beta1RollingUpdateDaemonSet n =
  V1beta1RollingUpdateDaemonSet
    <$> arbitraryReducedMaybeValue n -- v1beta1RollingUpdateDaemonSetMaxUnavailable :: Maybe A.Value
  
instance Arbitrary V1beta1RollingUpdateStatefulSetStrategy where
  arbitrary = sized genV1beta1RollingUpdateStatefulSetStrategy

genV1beta1RollingUpdateStatefulSetStrategy :: Int -> Gen V1beta1RollingUpdateStatefulSetStrategy
genV1beta1RollingUpdateStatefulSetStrategy n =
  V1beta1RollingUpdateStatefulSetStrategy
    <$> arbitraryReducedMaybe n -- v1beta1RollingUpdateStatefulSetStrategyPartition :: Maybe Int
  
instance Arbitrary V1beta1RuleWithOperations where
  arbitrary = sized genV1beta1RuleWithOperations

genV1beta1RuleWithOperations :: Int -> Gen V1beta1RuleWithOperations
genV1beta1RuleWithOperations n =
  V1beta1RuleWithOperations
    <$> arbitraryReducedMaybe n -- v1beta1RuleWithOperationsApiGroups :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1beta1RuleWithOperationsApiVersions :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1beta1RuleWithOperationsOperations :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1beta1RuleWithOperationsResources :: Maybe [Text]
  
instance Arbitrary V1beta1RunAsUserStrategyOptions where
  arbitrary = sized genV1beta1RunAsUserStrategyOptions

genV1beta1RunAsUserStrategyOptions :: Int -> Gen V1beta1RunAsUserStrategyOptions
genV1beta1RunAsUserStrategyOptions n =
  V1beta1RunAsUserStrategyOptions
    <$> arbitraryReducedMaybe n -- v1beta1RunAsUserStrategyOptionsRanges :: Maybe [V1beta1IDRange]
    <*> arbitrary -- v1beta1RunAsUserStrategyOptionsRule :: Text
  
instance Arbitrary V1beta1SELinuxStrategyOptions where
  arbitrary = sized genV1beta1SELinuxStrategyOptions

genV1beta1SELinuxStrategyOptions :: Int -> Gen V1beta1SELinuxStrategyOptions
genV1beta1SELinuxStrategyOptions n =
  V1beta1SELinuxStrategyOptions
    <$> arbitrary -- v1beta1SELinuxStrategyOptionsRule :: Text
    <*> arbitraryReducedMaybe n -- v1beta1SELinuxStrategyOptionsSeLinuxOptions :: Maybe V1SELinuxOptions
  
instance Arbitrary V1beta1SelfSubjectAccessReview where
  arbitrary = sized genV1beta1SelfSubjectAccessReview

genV1beta1SelfSubjectAccessReview :: Int -> Gen V1beta1SelfSubjectAccessReview
genV1beta1SelfSubjectAccessReview n =
  V1beta1SelfSubjectAccessReview
    <$> arbitraryReducedMaybe n -- v1beta1SelfSubjectAccessReviewApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1SelfSubjectAccessReviewKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1SelfSubjectAccessReviewMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1beta1SelfSubjectAccessReviewSpec :: V1beta1SelfSubjectAccessReviewSpec
    <*> arbitraryReducedMaybe n -- v1beta1SelfSubjectAccessReviewStatus :: Maybe V1beta1SubjectAccessReviewStatus
  
instance Arbitrary V1beta1SelfSubjectAccessReviewSpec where
  arbitrary = sized genV1beta1SelfSubjectAccessReviewSpec

genV1beta1SelfSubjectAccessReviewSpec :: Int -> Gen V1beta1SelfSubjectAccessReviewSpec
genV1beta1SelfSubjectAccessReviewSpec n =
  V1beta1SelfSubjectAccessReviewSpec
    <$> arbitraryReducedMaybe n -- v1beta1SelfSubjectAccessReviewSpecNonResourceAttributes :: Maybe V1beta1NonResourceAttributes
    <*> arbitraryReducedMaybe n -- v1beta1SelfSubjectAccessReviewSpecResourceAttributes :: Maybe V1beta1ResourceAttributes
  
instance Arbitrary V1beta1SelfSubjectRulesReview where
  arbitrary = sized genV1beta1SelfSubjectRulesReview

genV1beta1SelfSubjectRulesReview :: Int -> Gen V1beta1SelfSubjectRulesReview
genV1beta1SelfSubjectRulesReview n =
  V1beta1SelfSubjectRulesReview
    <$> arbitraryReducedMaybe n -- v1beta1SelfSubjectRulesReviewApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1SelfSubjectRulesReviewKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1SelfSubjectRulesReviewMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1beta1SelfSubjectRulesReviewSpec :: V1beta1SelfSubjectRulesReviewSpec
    <*> arbitraryReducedMaybe n -- v1beta1SelfSubjectRulesReviewStatus :: Maybe V1beta1SubjectRulesReviewStatus
  
instance Arbitrary V1beta1SelfSubjectRulesReviewSpec where
  arbitrary = sized genV1beta1SelfSubjectRulesReviewSpec

genV1beta1SelfSubjectRulesReviewSpec :: Int -> Gen V1beta1SelfSubjectRulesReviewSpec
genV1beta1SelfSubjectRulesReviewSpec n =
  V1beta1SelfSubjectRulesReviewSpec
    <$> arbitraryReducedMaybe n -- v1beta1SelfSubjectRulesReviewSpecNamespace :: Maybe Text
  
instance Arbitrary V1beta1StatefulSet where
  arbitrary = sized genV1beta1StatefulSet

genV1beta1StatefulSet :: Int -> Gen V1beta1StatefulSet
genV1beta1StatefulSet n =
  V1beta1StatefulSet
    <$> arbitraryReducedMaybe n -- v1beta1StatefulSetApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1StatefulSetKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1StatefulSetMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta1StatefulSetSpec :: Maybe V1beta1StatefulSetSpec
    <*> arbitraryReducedMaybe n -- v1beta1StatefulSetStatus :: Maybe V1beta1StatefulSetStatus
  
instance Arbitrary V1beta1StatefulSetCondition where
  arbitrary = sized genV1beta1StatefulSetCondition

genV1beta1StatefulSetCondition :: Int -> Gen V1beta1StatefulSetCondition
genV1beta1StatefulSetCondition n =
  V1beta1StatefulSetCondition
    <$> arbitraryReducedMaybe n -- v1beta1StatefulSetConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1beta1StatefulSetConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1StatefulSetConditionReason :: Maybe Text
    <*> arbitrary -- v1beta1StatefulSetConditionStatus :: Text
    <*> arbitrary -- v1beta1StatefulSetConditionType :: Text
  
instance Arbitrary V1beta1StatefulSetList where
  arbitrary = sized genV1beta1StatefulSetList

genV1beta1StatefulSetList :: Int -> Gen V1beta1StatefulSetList
genV1beta1StatefulSetList n =
  V1beta1StatefulSetList
    <$> arbitraryReducedMaybe n -- v1beta1StatefulSetListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta1StatefulSetListItems :: [V1beta1StatefulSet]
    <*> arbitraryReducedMaybe n -- v1beta1StatefulSetListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1StatefulSetListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta1StatefulSetSpec where
  arbitrary = sized genV1beta1StatefulSetSpec

genV1beta1StatefulSetSpec :: Int -> Gen V1beta1StatefulSetSpec
genV1beta1StatefulSetSpec n =
  V1beta1StatefulSetSpec
    <$> arbitraryReducedMaybe n -- v1beta1StatefulSetSpecPodManagementPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1StatefulSetSpecReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta1StatefulSetSpecRevisionHistoryLimit :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta1StatefulSetSpecSelector :: Maybe V1LabelSelector
    <*> arbitrary -- v1beta1StatefulSetSpecServiceName :: Text
    <*> arbitraryReduced n -- v1beta1StatefulSetSpecTemplate :: V1PodTemplateSpec
    <*> arbitraryReducedMaybe n -- v1beta1StatefulSetSpecUpdateStrategy :: Maybe V1beta1StatefulSetUpdateStrategy
    <*> arbitraryReducedMaybe n -- v1beta1StatefulSetSpecVolumeClaimTemplates :: Maybe [V1PersistentVolumeClaim]
  
instance Arbitrary V1beta1StatefulSetStatus where
  arbitrary = sized genV1beta1StatefulSetStatus

genV1beta1StatefulSetStatus :: Int -> Gen V1beta1StatefulSetStatus
genV1beta1StatefulSetStatus n =
  V1beta1StatefulSetStatus
    <$> arbitraryReducedMaybe n -- v1beta1StatefulSetStatusCollisionCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta1StatefulSetStatusConditions :: Maybe [V1beta1StatefulSetCondition]
    <*> arbitraryReducedMaybe n -- v1beta1StatefulSetStatusCurrentReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta1StatefulSetStatusCurrentRevision :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1StatefulSetStatusObservedGeneration :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1beta1StatefulSetStatusReadyReplicas :: Maybe Int
    <*> arbitrary -- v1beta1StatefulSetStatusReplicas :: Int
    <*> arbitraryReducedMaybe n -- v1beta1StatefulSetStatusUpdateRevision :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1StatefulSetStatusUpdatedReplicas :: Maybe Int
  
instance Arbitrary V1beta1StatefulSetUpdateStrategy where
  arbitrary = sized genV1beta1StatefulSetUpdateStrategy

genV1beta1StatefulSetUpdateStrategy :: Int -> Gen V1beta1StatefulSetUpdateStrategy
genV1beta1StatefulSetUpdateStrategy n =
  V1beta1StatefulSetUpdateStrategy
    <$> arbitraryReducedMaybe n -- v1beta1StatefulSetUpdateStrategyRollingUpdate :: Maybe V1beta1RollingUpdateStatefulSetStrategy
    <*> arbitraryReducedMaybe n -- v1beta1StatefulSetUpdateStrategyType :: Maybe Text
  
instance Arbitrary V1beta1StorageClass where
  arbitrary = sized genV1beta1StorageClass

genV1beta1StorageClass :: Int -> Gen V1beta1StorageClass
genV1beta1StorageClass n =
  V1beta1StorageClass
    <$> arbitraryReducedMaybe n -- v1beta1StorageClassAllowVolumeExpansion :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1beta1StorageClassApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1StorageClassKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1StorageClassMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta1StorageClassMountOptions :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1beta1StorageClassParameters :: Maybe (Map.Map String Text)
    <*> arbitrary -- v1beta1StorageClassProvisioner :: Text
    <*> arbitraryReducedMaybe n -- v1beta1StorageClassReclaimPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1StorageClassVolumeBindingMode :: Maybe Text
  
instance Arbitrary V1beta1StorageClassList where
  arbitrary = sized genV1beta1StorageClassList

genV1beta1StorageClassList :: Int -> Gen V1beta1StorageClassList
genV1beta1StorageClassList n =
  V1beta1StorageClassList
    <$> arbitraryReducedMaybe n -- v1beta1StorageClassListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta1StorageClassListItems :: [V1beta1StorageClass]
    <*> arbitraryReducedMaybe n -- v1beta1StorageClassListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1StorageClassListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta1Subject where
  arbitrary = sized genV1beta1Subject

genV1beta1Subject :: Int -> Gen V1beta1Subject
genV1beta1Subject n =
  V1beta1Subject
    <$> arbitraryReducedMaybe n -- v1beta1SubjectApiGroup :: Maybe Text
    <*> arbitrary -- v1beta1SubjectKind :: Text
    <*> arbitrary -- v1beta1SubjectName :: Text
    <*> arbitraryReducedMaybe n -- v1beta1SubjectNamespace :: Maybe Text
  
instance Arbitrary V1beta1SubjectAccessReview where
  arbitrary = sized genV1beta1SubjectAccessReview

genV1beta1SubjectAccessReview :: Int -> Gen V1beta1SubjectAccessReview
genV1beta1SubjectAccessReview n =
  V1beta1SubjectAccessReview
    <$> arbitraryReducedMaybe n -- v1beta1SubjectAccessReviewApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1SubjectAccessReviewKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1SubjectAccessReviewMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1beta1SubjectAccessReviewSpec :: V1beta1SubjectAccessReviewSpec
    <*> arbitraryReducedMaybe n -- v1beta1SubjectAccessReviewStatus :: Maybe V1beta1SubjectAccessReviewStatus
  
instance Arbitrary V1beta1SubjectAccessReviewSpec where
  arbitrary = sized genV1beta1SubjectAccessReviewSpec

genV1beta1SubjectAccessReviewSpec :: Int -> Gen V1beta1SubjectAccessReviewSpec
genV1beta1SubjectAccessReviewSpec n =
  V1beta1SubjectAccessReviewSpec
    <$> arbitraryReducedMaybe n -- v1beta1SubjectAccessReviewSpecExtra :: Maybe (Map.Map String [Text])
    <*> arbitraryReducedMaybe n -- v1beta1SubjectAccessReviewSpecGroup :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1beta1SubjectAccessReviewSpecNonResourceAttributes :: Maybe V1beta1NonResourceAttributes
    <*> arbitraryReducedMaybe n -- v1beta1SubjectAccessReviewSpecResourceAttributes :: Maybe V1beta1ResourceAttributes
    <*> arbitraryReducedMaybe n -- v1beta1SubjectAccessReviewSpecUid :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1SubjectAccessReviewSpecUser :: Maybe Text
  
instance Arbitrary V1beta1SubjectAccessReviewStatus where
  arbitrary = sized genV1beta1SubjectAccessReviewStatus

genV1beta1SubjectAccessReviewStatus :: Int -> Gen V1beta1SubjectAccessReviewStatus
genV1beta1SubjectAccessReviewStatus n =
  V1beta1SubjectAccessReviewStatus
    <$> arbitrary -- v1beta1SubjectAccessReviewStatusAllowed :: Bool
    <*> arbitraryReducedMaybe n -- v1beta1SubjectAccessReviewStatusDenied :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1beta1SubjectAccessReviewStatusEvaluationError :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1SubjectAccessReviewStatusReason :: Maybe Text
  
instance Arbitrary V1beta1SubjectRulesReviewStatus where
  arbitrary = sized genV1beta1SubjectRulesReviewStatus

genV1beta1SubjectRulesReviewStatus :: Int -> Gen V1beta1SubjectRulesReviewStatus
genV1beta1SubjectRulesReviewStatus n =
  V1beta1SubjectRulesReviewStatus
    <$> arbitraryReducedMaybe n -- v1beta1SubjectRulesReviewStatusEvaluationError :: Maybe Text
    <*> arbitrary -- v1beta1SubjectRulesReviewStatusIncomplete :: Bool
    <*> arbitraryReduced n -- v1beta1SubjectRulesReviewStatusNonResourceRules :: [V1beta1NonResourceRule]
    <*> arbitraryReduced n -- v1beta1SubjectRulesReviewStatusResourceRules :: [V1beta1ResourceRule]
  
instance Arbitrary V1beta1SupplementalGroupsStrategyOptions where
  arbitrary = sized genV1beta1SupplementalGroupsStrategyOptions

genV1beta1SupplementalGroupsStrategyOptions :: Int -> Gen V1beta1SupplementalGroupsStrategyOptions
genV1beta1SupplementalGroupsStrategyOptions n =
  V1beta1SupplementalGroupsStrategyOptions
    <$> arbitraryReducedMaybe n -- v1beta1SupplementalGroupsStrategyOptionsRanges :: Maybe [V1beta1IDRange]
    <*> arbitraryReducedMaybe n -- v1beta1SupplementalGroupsStrategyOptionsRule :: Maybe Text
  
instance Arbitrary V1beta1TokenReview where
  arbitrary = sized genV1beta1TokenReview

genV1beta1TokenReview :: Int -> Gen V1beta1TokenReview
genV1beta1TokenReview n =
  V1beta1TokenReview
    <$> arbitraryReducedMaybe n -- v1beta1TokenReviewApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1TokenReviewKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1TokenReviewMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1beta1TokenReviewSpec :: V1beta1TokenReviewSpec
    <*> arbitraryReducedMaybe n -- v1beta1TokenReviewStatus :: Maybe V1beta1TokenReviewStatus
  
instance Arbitrary V1beta1TokenReviewSpec where
  arbitrary = sized genV1beta1TokenReviewSpec

genV1beta1TokenReviewSpec :: Int -> Gen V1beta1TokenReviewSpec
genV1beta1TokenReviewSpec n =
  V1beta1TokenReviewSpec
    <$> arbitraryReducedMaybe n -- v1beta1TokenReviewSpecToken :: Maybe Text
  
instance Arbitrary V1beta1TokenReviewStatus where
  arbitrary = sized genV1beta1TokenReviewStatus

genV1beta1TokenReviewStatus :: Int -> Gen V1beta1TokenReviewStatus
genV1beta1TokenReviewStatus n =
  V1beta1TokenReviewStatus
    <$> arbitraryReducedMaybe n -- v1beta1TokenReviewStatusAuthenticated :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1beta1TokenReviewStatusError :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1TokenReviewStatusUser :: Maybe V1beta1UserInfo
  
instance Arbitrary V1beta1UserInfo where
  arbitrary = sized genV1beta1UserInfo

genV1beta1UserInfo :: Int -> Gen V1beta1UserInfo
genV1beta1UserInfo n =
  V1beta1UserInfo
    <$> arbitraryReducedMaybe n -- v1beta1UserInfoExtra :: Maybe (Map.Map String [Text])
    <*> arbitraryReducedMaybe n -- v1beta1UserInfoGroups :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1beta1UserInfoUid :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1UserInfoUsername :: Maybe Text
  
instance Arbitrary V1beta1ValidatingWebhookConfiguration where
  arbitrary = sized genV1beta1ValidatingWebhookConfiguration

genV1beta1ValidatingWebhookConfiguration :: Int -> Gen V1beta1ValidatingWebhookConfiguration
genV1beta1ValidatingWebhookConfiguration n =
  V1beta1ValidatingWebhookConfiguration
    <$> arbitraryReducedMaybe n -- v1beta1ValidatingWebhookConfigurationApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingWebhookConfigurationKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingWebhookConfigurationMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingWebhookConfigurationWebhooks :: Maybe [V1beta1Webhook]
  
instance Arbitrary V1beta1ValidatingWebhookConfigurationList where
  arbitrary = sized genV1beta1ValidatingWebhookConfigurationList

genV1beta1ValidatingWebhookConfigurationList :: Int -> Gen V1beta1ValidatingWebhookConfigurationList
genV1beta1ValidatingWebhookConfigurationList n =
  V1beta1ValidatingWebhookConfigurationList
    <$> arbitraryReducedMaybe n -- v1beta1ValidatingWebhookConfigurationListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta1ValidatingWebhookConfigurationListItems :: [V1beta1ValidatingWebhookConfiguration]
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingWebhookConfigurationListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingWebhookConfigurationListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta1Webhook where
  arbitrary = sized genV1beta1Webhook

genV1beta1Webhook :: Int -> Gen V1beta1Webhook
genV1beta1Webhook n =
  V1beta1Webhook
    <$> arbitraryReduced n -- v1beta1WebhookClientConfig :: V1beta1WebhookClientConfig
    <*> arbitraryReducedMaybe n -- v1beta1WebhookFailurePolicy :: Maybe Text
    <*> arbitrary -- v1beta1WebhookName :: Text
    <*> arbitraryReducedMaybe n -- v1beta1WebhookNamespaceSelector :: Maybe V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1beta1WebhookRules :: Maybe [V1beta1RuleWithOperations]
  
instance Arbitrary V1beta1WebhookClientConfig where
  arbitrary = sized genV1beta1WebhookClientConfig

genV1beta1WebhookClientConfig :: Int -> Gen V1beta1WebhookClientConfig
genV1beta1WebhookClientConfig n =
  V1beta1WebhookClientConfig
    <$> arbitraryReduced n -- v1beta1WebhookClientConfigCaBundle :: ByteArray
    <*> arbitraryReducedMaybe n -- v1beta1WebhookClientConfigService :: Maybe AdmissionregistrationV1beta1ServiceReference
    <*> arbitraryReducedMaybe n -- v1beta1WebhookClientConfigUrl :: Maybe Text
  
instance Arbitrary V1beta2ControllerRevision where
  arbitrary = sized genV1beta2ControllerRevision

genV1beta2ControllerRevision :: Int -> Gen V1beta2ControllerRevision
genV1beta2ControllerRevision n =
  V1beta2ControllerRevision
    <$> arbitraryReducedMaybe n -- v1beta2ControllerRevisionApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2ControllerRevisionData :: Maybe RuntimeRawExtension
    <*> arbitraryReducedMaybe n -- v1beta2ControllerRevisionKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2ControllerRevisionMetadata :: Maybe V1ObjectMeta
    <*> arbitrary -- v1beta2ControllerRevisionRevision :: Integer
  
instance Arbitrary V1beta2ControllerRevisionList where
  arbitrary = sized genV1beta2ControllerRevisionList

genV1beta2ControllerRevisionList :: Int -> Gen V1beta2ControllerRevisionList
genV1beta2ControllerRevisionList n =
  V1beta2ControllerRevisionList
    <$> arbitraryReducedMaybe n -- v1beta2ControllerRevisionListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta2ControllerRevisionListItems :: [V1beta2ControllerRevision]
    <*> arbitraryReducedMaybe n -- v1beta2ControllerRevisionListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2ControllerRevisionListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta2DaemonSet where
  arbitrary = sized genV1beta2DaemonSet

genV1beta2DaemonSet :: Int -> Gen V1beta2DaemonSet
genV1beta2DaemonSet n =
  V1beta2DaemonSet
    <$> arbitraryReducedMaybe n -- v1beta2DaemonSetApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2DaemonSetKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2DaemonSetMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta2DaemonSetSpec :: Maybe V1beta2DaemonSetSpec
    <*> arbitraryReducedMaybe n -- v1beta2DaemonSetStatus :: Maybe V1beta2DaemonSetStatus
  
instance Arbitrary V1beta2DaemonSetCondition where
  arbitrary = sized genV1beta2DaemonSetCondition

genV1beta2DaemonSetCondition :: Int -> Gen V1beta2DaemonSetCondition
genV1beta2DaemonSetCondition n =
  V1beta2DaemonSetCondition
    <$> arbitraryReducedMaybe n -- v1beta2DaemonSetConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1beta2DaemonSetConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2DaemonSetConditionReason :: Maybe Text
    <*> arbitrary -- v1beta2DaemonSetConditionStatus :: Text
    <*> arbitrary -- v1beta2DaemonSetConditionType :: Text
  
instance Arbitrary V1beta2DaemonSetList where
  arbitrary = sized genV1beta2DaemonSetList

genV1beta2DaemonSetList :: Int -> Gen V1beta2DaemonSetList
genV1beta2DaemonSetList n =
  V1beta2DaemonSetList
    <$> arbitraryReducedMaybe n -- v1beta2DaemonSetListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta2DaemonSetListItems :: [V1beta2DaemonSet]
    <*> arbitraryReducedMaybe n -- v1beta2DaemonSetListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2DaemonSetListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta2DaemonSetSpec where
  arbitrary = sized genV1beta2DaemonSetSpec

genV1beta2DaemonSetSpec :: Int -> Gen V1beta2DaemonSetSpec
genV1beta2DaemonSetSpec n =
  V1beta2DaemonSetSpec
    <$> arbitraryReducedMaybe n -- v1beta2DaemonSetSpecMinReadySeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta2DaemonSetSpecRevisionHistoryLimit :: Maybe Int
    <*> arbitraryReduced n -- v1beta2DaemonSetSpecSelector :: V1LabelSelector
    <*> arbitraryReduced n -- v1beta2DaemonSetSpecTemplate :: V1PodTemplateSpec
    <*> arbitraryReducedMaybe n -- v1beta2DaemonSetSpecUpdateStrategy :: Maybe V1beta2DaemonSetUpdateStrategy
  
instance Arbitrary V1beta2DaemonSetStatus where
  arbitrary = sized genV1beta2DaemonSetStatus

genV1beta2DaemonSetStatus :: Int -> Gen V1beta2DaemonSetStatus
genV1beta2DaemonSetStatus n =
  V1beta2DaemonSetStatus
    <$> arbitraryReducedMaybe n -- v1beta2DaemonSetStatusCollisionCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta2DaemonSetStatusConditions :: Maybe [V1beta2DaemonSetCondition]
    <*> arbitrary -- v1beta2DaemonSetStatusCurrentNumberScheduled :: Int
    <*> arbitrary -- v1beta2DaemonSetStatusDesiredNumberScheduled :: Int
    <*> arbitraryReducedMaybe n -- v1beta2DaemonSetStatusNumberAvailable :: Maybe Int
    <*> arbitrary -- v1beta2DaemonSetStatusNumberMisscheduled :: Int
    <*> arbitrary -- v1beta2DaemonSetStatusNumberReady :: Int
    <*> arbitraryReducedMaybe n -- v1beta2DaemonSetStatusNumberUnavailable :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta2DaemonSetStatusObservedGeneration :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1beta2DaemonSetStatusUpdatedNumberScheduled :: Maybe Int
  
instance Arbitrary V1beta2DaemonSetUpdateStrategy where
  arbitrary = sized genV1beta2DaemonSetUpdateStrategy

genV1beta2DaemonSetUpdateStrategy :: Int -> Gen V1beta2DaemonSetUpdateStrategy
genV1beta2DaemonSetUpdateStrategy n =
  V1beta2DaemonSetUpdateStrategy
    <$> arbitraryReducedMaybe n -- v1beta2DaemonSetUpdateStrategyRollingUpdate :: Maybe V1beta2RollingUpdateDaemonSet
    <*> arbitraryReducedMaybe n -- v1beta2DaemonSetUpdateStrategyType :: Maybe Text
  
instance Arbitrary V1beta2Deployment where
  arbitrary = sized genV1beta2Deployment

genV1beta2Deployment :: Int -> Gen V1beta2Deployment
genV1beta2Deployment n =
  V1beta2Deployment
    <$> arbitraryReducedMaybe n -- v1beta2DeploymentApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2DeploymentKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2DeploymentMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta2DeploymentSpec :: Maybe V1beta2DeploymentSpec
    <*> arbitraryReducedMaybe n -- v1beta2DeploymentStatus :: Maybe V1beta2DeploymentStatus
  
instance Arbitrary V1beta2DeploymentCondition where
  arbitrary = sized genV1beta2DeploymentCondition

genV1beta2DeploymentCondition :: Int -> Gen V1beta2DeploymentCondition
genV1beta2DeploymentCondition n =
  V1beta2DeploymentCondition
    <$> arbitraryReducedMaybe n -- v1beta2DeploymentConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1beta2DeploymentConditionLastUpdateTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1beta2DeploymentConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2DeploymentConditionReason :: Maybe Text
    <*> arbitrary -- v1beta2DeploymentConditionStatus :: Text
    <*> arbitrary -- v1beta2DeploymentConditionType :: Text
  
instance Arbitrary V1beta2DeploymentList where
  arbitrary = sized genV1beta2DeploymentList

genV1beta2DeploymentList :: Int -> Gen V1beta2DeploymentList
genV1beta2DeploymentList n =
  V1beta2DeploymentList
    <$> arbitraryReducedMaybe n -- v1beta2DeploymentListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta2DeploymentListItems :: [V1beta2Deployment]
    <*> arbitraryReducedMaybe n -- v1beta2DeploymentListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2DeploymentListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta2DeploymentSpec where
  arbitrary = sized genV1beta2DeploymentSpec

genV1beta2DeploymentSpec :: Int -> Gen V1beta2DeploymentSpec
genV1beta2DeploymentSpec n =
  V1beta2DeploymentSpec
    <$> arbitraryReducedMaybe n -- v1beta2DeploymentSpecMinReadySeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta2DeploymentSpecPaused :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1beta2DeploymentSpecProgressDeadlineSeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta2DeploymentSpecReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta2DeploymentSpecRevisionHistoryLimit :: Maybe Int
    <*> arbitraryReduced n -- v1beta2DeploymentSpecSelector :: V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1beta2DeploymentSpecStrategy :: Maybe V1beta2DeploymentStrategy
    <*> arbitraryReduced n -- v1beta2DeploymentSpecTemplate :: V1PodTemplateSpec
  
instance Arbitrary V1beta2DeploymentStatus where
  arbitrary = sized genV1beta2DeploymentStatus

genV1beta2DeploymentStatus :: Int -> Gen V1beta2DeploymentStatus
genV1beta2DeploymentStatus n =
  V1beta2DeploymentStatus
    <$> arbitraryReducedMaybe n -- v1beta2DeploymentStatusAvailableReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta2DeploymentStatusCollisionCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta2DeploymentStatusConditions :: Maybe [V1beta2DeploymentCondition]
    <*> arbitraryReducedMaybe n -- v1beta2DeploymentStatusObservedGeneration :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1beta2DeploymentStatusReadyReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta2DeploymentStatusReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta2DeploymentStatusUnavailableReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta2DeploymentStatusUpdatedReplicas :: Maybe Int
  
instance Arbitrary V1beta2DeploymentStrategy where
  arbitrary = sized genV1beta2DeploymentStrategy

genV1beta2DeploymentStrategy :: Int -> Gen V1beta2DeploymentStrategy
genV1beta2DeploymentStrategy n =
  V1beta2DeploymentStrategy
    <$> arbitraryReducedMaybe n -- v1beta2DeploymentStrategyRollingUpdate :: Maybe V1beta2RollingUpdateDeployment
    <*> arbitraryReducedMaybe n -- v1beta2DeploymentStrategyType :: Maybe Text
  
instance Arbitrary V1beta2ReplicaSet where
  arbitrary = sized genV1beta2ReplicaSet

genV1beta2ReplicaSet :: Int -> Gen V1beta2ReplicaSet
genV1beta2ReplicaSet n =
  V1beta2ReplicaSet
    <$> arbitraryReducedMaybe n -- v1beta2ReplicaSetApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2ReplicaSetKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2ReplicaSetMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta2ReplicaSetSpec :: Maybe V1beta2ReplicaSetSpec
    <*> arbitraryReducedMaybe n -- v1beta2ReplicaSetStatus :: Maybe V1beta2ReplicaSetStatus
  
instance Arbitrary V1beta2ReplicaSetCondition where
  arbitrary = sized genV1beta2ReplicaSetCondition

genV1beta2ReplicaSetCondition :: Int -> Gen V1beta2ReplicaSetCondition
genV1beta2ReplicaSetCondition n =
  V1beta2ReplicaSetCondition
    <$> arbitraryReducedMaybe n -- v1beta2ReplicaSetConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1beta2ReplicaSetConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2ReplicaSetConditionReason :: Maybe Text
    <*> arbitrary -- v1beta2ReplicaSetConditionStatus :: Text
    <*> arbitrary -- v1beta2ReplicaSetConditionType :: Text
  
instance Arbitrary V1beta2ReplicaSetList where
  arbitrary = sized genV1beta2ReplicaSetList

genV1beta2ReplicaSetList :: Int -> Gen V1beta2ReplicaSetList
genV1beta2ReplicaSetList n =
  V1beta2ReplicaSetList
    <$> arbitraryReducedMaybe n -- v1beta2ReplicaSetListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta2ReplicaSetListItems :: [V1beta2ReplicaSet]
    <*> arbitraryReducedMaybe n -- v1beta2ReplicaSetListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2ReplicaSetListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta2ReplicaSetSpec where
  arbitrary = sized genV1beta2ReplicaSetSpec

genV1beta2ReplicaSetSpec :: Int -> Gen V1beta2ReplicaSetSpec
genV1beta2ReplicaSetSpec n =
  V1beta2ReplicaSetSpec
    <$> arbitraryReducedMaybe n -- v1beta2ReplicaSetSpecMinReadySeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta2ReplicaSetSpecReplicas :: Maybe Int
    <*> arbitraryReduced n -- v1beta2ReplicaSetSpecSelector :: V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1beta2ReplicaSetSpecTemplate :: Maybe V1PodTemplateSpec
  
instance Arbitrary V1beta2ReplicaSetStatus where
  arbitrary = sized genV1beta2ReplicaSetStatus

genV1beta2ReplicaSetStatus :: Int -> Gen V1beta2ReplicaSetStatus
genV1beta2ReplicaSetStatus n =
  V1beta2ReplicaSetStatus
    <$> arbitraryReducedMaybe n -- v1beta2ReplicaSetStatusAvailableReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta2ReplicaSetStatusConditions :: Maybe [V1beta2ReplicaSetCondition]
    <*> arbitraryReducedMaybe n -- v1beta2ReplicaSetStatusFullyLabeledReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta2ReplicaSetStatusObservedGeneration :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1beta2ReplicaSetStatusReadyReplicas :: Maybe Int
    <*> arbitrary -- v1beta2ReplicaSetStatusReplicas :: Int
  
instance Arbitrary V1beta2RollingUpdateDaemonSet where
  arbitrary = sized genV1beta2RollingUpdateDaemonSet

genV1beta2RollingUpdateDaemonSet :: Int -> Gen V1beta2RollingUpdateDaemonSet
genV1beta2RollingUpdateDaemonSet n =
  V1beta2RollingUpdateDaemonSet
    <$> arbitraryReducedMaybeValue n -- v1beta2RollingUpdateDaemonSetMaxUnavailable :: Maybe A.Value
  
instance Arbitrary V1beta2RollingUpdateDeployment where
  arbitrary = sized genV1beta2RollingUpdateDeployment

genV1beta2RollingUpdateDeployment :: Int -> Gen V1beta2RollingUpdateDeployment
genV1beta2RollingUpdateDeployment n =
  V1beta2RollingUpdateDeployment
    <$> arbitraryReducedMaybeValue n -- v1beta2RollingUpdateDeploymentMaxSurge :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- v1beta2RollingUpdateDeploymentMaxUnavailable :: Maybe A.Value
  
instance Arbitrary V1beta2RollingUpdateStatefulSetStrategy where
  arbitrary = sized genV1beta2RollingUpdateStatefulSetStrategy

genV1beta2RollingUpdateStatefulSetStrategy :: Int -> Gen V1beta2RollingUpdateStatefulSetStrategy
genV1beta2RollingUpdateStatefulSetStrategy n =
  V1beta2RollingUpdateStatefulSetStrategy
    <$> arbitraryReducedMaybe n -- v1beta2RollingUpdateStatefulSetStrategyPartition :: Maybe Int
  
instance Arbitrary V1beta2Scale where
  arbitrary = sized genV1beta2Scale

genV1beta2Scale :: Int -> Gen V1beta2Scale
genV1beta2Scale n =
  V1beta2Scale
    <$> arbitraryReducedMaybe n -- v1beta2ScaleApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2ScaleKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2ScaleMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta2ScaleSpec :: Maybe V1beta2ScaleSpec
    <*> arbitraryReducedMaybe n -- v1beta2ScaleStatus :: Maybe V1beta2ScaleStatus
  
instance Arbitrary V1beta2ScaleSpec where
  arbitrary = sized genV1beta2ScaleSpec

genV1beta2ScaleSpec :: Int -> Gen V1beta2ScaleSpec
genV1beta2ScaleSpec n =
  V1beta2ScaleSpec
    <$> arbitraryReducedMaybe n -- v1beta2ScaleSpecReplicas :: Maybe Int
  
instance Arbitrary V1beta2ScaleStatus where
  arbitrary = sized genV1beta2ScaleStatus

genV1beta2ScaleStatus :: Int -> Gen V1beta2ScaleStatus
genV1beta2ScaleStatus n =
  V1beta2ScaleStatus
    <$> arbitrary -- v1beta2ScaleStatusReplicas :: Int
    <*> arbitraryReducedMaybe n -- v1beta2ScaleStatusSelector :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1beta2ScaleStatusTargetSelector :: Maybe Text
  
instance Arbitrary V1beta2StatefulSet where
  arbitrary = sized genV1beta2StatefulSet

genV1beta2StatefulSet :: Int -> Gen V1beta2StatefulSet
genV1beta2StatefulSet n =
  V1beta2StatefulSet
    <$> arbitraryReducedMaybe n -- v1beta2StatefulSetApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2StatefulSetKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2StatefulSetMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta2StatefulSetSpec :: Maybe V1beta2StatefulSetSpec
    <*> arbitraryReducedMaybe n -- v1beta2StatefulSetStatus :: Maybe V1beta2StatefulSetStatus
  
instance Arbitrary V1beta2StatefulSetCondition where
  arbitrary = sized genV1beta2StatefulSetCondition

genV1beta2StatefulSetCondition :: Int -> Gen V1beta2StatefulSetCondition
genV1beta2StatefulSetCondition n =
  V1beta2StatefulSetCondition
    <$> arbitraryReducedMaybe n -- v1beta2StatefulSetConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1beta2StatefulSetConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2StatefulSetConditionReason :: Maybe Text
    <*> arbitrary -- v1beta2StatefulSetConditionStatus :: Text
    <*> arbitrary -- v1beta2StatefulSetConditionType :: Text
  
instance Arbitrary V1beta2StatefulSetList where
  arbitrary = sized genV1beta2StatefulSetList

genV1beta2StatefulSetList :: Int -> Gen V1beta2StatefulSetList
genV1beta2StatefulSetList n =
  V1beta2StatefulSetList
    <$> arbitraryReducedMaybe n -- v1beta2StatefulSetListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta2StatefulSetListItems :: [V1beta2StatefulSet]
    <*> arbitraryReducedMaybe n -- v1beta2StatefulSetListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2StatefulSetListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta2StatefulSetSpec where
  arbitrary = sized genV1beta2StatefulSetSpec

genV1beta2StatefulSetSpec :: Int -> Gen V1beta2StatefulSetSpec
genV1beta2StatefulSetSpec n =
  V1beta2StatefulSetSpec
    <$> arbitraryReducedMaybe n -- v1beta2StatefulSetSpecPodManagementPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2StatefulSetSpecReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta2StatefulSetSpecRevisionHistoryLimit :: Maybe Int
    <*> arbitraryReduced n -- v1beta2StatefulSetSpecSelector :: V1LabelSelector
    <*> arbitrary -- v1beta2StatefulSetSpecServiceName :: Text
    <*> arbitraryReduced n -- v1beta2StatefulSetSpecTemplate :: V1PodTemplateSpec
    <*> arbitraryReducedMaybe n -- v1beta2StatefulSetSpecUpdateStrategy :: Maybe V1beta2StatefulSetUpdateStrategy
    <*> arbitraryReducedMaybe n -- v1beta2StatefulSetSpecVolumeClaimTemplates :: Maybe [V1PersistentVolumeClaim]
  
instance Arbitrary V1beta2StatefulSetStatus where
  arbitrary = sized genV1beta2StatefulSetStatus

genV1beta2StatefulSetStatus :: Int -> Gen V1beta2StatefulSetStatus
genV1beta2StatefulSetStatus n =
  V1beta2StatefulSetStatus
    <$> arbitraryReducedMaybe n -- v1beta2StatefulSetStatusCollisionCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta2StatefulSetStatusConditions :: Maybe [V1beta2StatefulSetCondition]
    <*> arbitraryReducedMaybe n -- v1beta2StatefulSetStatusCurrentReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta2StatefulSetStatusCurrentRevision :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2StatefulSetStatusObservedGeneration :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1beta2StatefulSetStatusReadyReplicas :: Maybe Int
    <*> arbitrary -- v1beta2StatefulSetStatusReplicas :: Int
    <*> arbitraryReducedMaybe n -- v1beta2StatefulSetStatusUpdateRevision :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta2StatefulSetStatusUpdatedReplicas :: Maybe Int
  
instance Arbitrary V1beta2StatefulSetUpdateStrategy where
  arbitrary = sized genV1beta2StatefulSetUpdateStrategy

genV1beta2StatefulSetUpdateStrategy :: Int -> Gen V1beta2StatefulSetUpdateStrategy
genV1beta2StatefulSetUpdateStrategy n =
  V1beta2StatefulSetUpdateStrategy
    <$> arbitraryReducedMaybe n -- v1beta2StatefulSetUpdateStrategyRollingUpdate :: Maybe V1beta2RollingUpdateStatefulSetStrategy
    <*> arbitraryReducedMaybe n -- v1beta2StatefulSetUpdateStrategyType :: Maybe Text
  
instance Arbitrary V2alpha1CronJob where
  arbitrary = sized genV2alpha1CronJob

genV2alpha1CronJob :: Int -> Gen V2alpha1CronJob
genV2alpha1CronJob n =
  V2alpha1CronJob
    <$> arbitraryReducedMaybe n -- v2alpha1CronJobApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v2alpha1CronJobKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v2alpha1CronJobMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v2alpha1CronJobSpec :: Maybe V2alpha1CronJobSpec
    <*> arbitraryReducedMaybe n -- v2alpha1CronJobStatus :: Maybe V2alpha1CronJobStatus
  
instance Arbitrary V2alpha1CronJobList where
  arbitrary = sized genV2alpha1CronJobList

genV2alpha1CronJobList :: Int -> Gen V2alpha1CronJobList
genV2alpha1CronJobList n =
  V2alpha1CronJobList
    <$> arbitraryReducedMaybe n -- v2alpha1CronJobListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v2alpha1CronJobListItems :: [V2alpha1CronJob]
    <*> arbitraryReducedMaybe n -- v2alpha1CronJobListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v2alpha1CronJobListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V2alpha1CronJobSpec where
  arbitrary = sized genV2alpha1CronJobSpec

genV2alpha1CronJobSpec :: Int -> Gen V2alpha1CronJobSpec
genV2alpha1CronJobSpec n =
  V2alpha1CronJobSpec
    <$> arbitraryReducedMaybe n -- v2alpha1CronJobSpecConcurrencyPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v2alpha1CronJobSpecFailedJobsHistoryLimit :: Maybe Int
    <*> arbitraryReduced n -- v2alpha1CronJobSpecJobTemplate :: V2alpha1JobTemplateSpec
    <*> arbitrary -- v2alpha1CronJobSpecSchedule :: Text
    <*> arbitraryReducedMaybe n -- v2alpha1CronJobSpecStartingDeadlineSeconds :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v2alpha1CronJobSpecSuccessfulJobsHistoryLimit :: Maybe Int
    <*> arbitraryReducedMaybe n -- v2alpha1CronJobSpecSuspend :: Maybe Bool
  
instance Arbitrary V2alpha1CronJobStatus where
  arbitrary = sized genV2alpha1CronJobStatus

genV2alpha1CronJobStatus :: Int -> Gen V2alpha1CronJobStatus
genV2alpha1CronJobStatus n =
  V2alpha1CronJobStatus
    <$> arbitraryReducedMaybe n -- v2alpha1CronJobStatusActive :: Maybe [V1ObjectReference]
    <*> arbitraryReducedMaybe n -- v2alpha1CronJobStatusLastScheduleTime :: Maybe DateTime
  
instance Arbitrary V2alpha1JobTemplateSpec where
  arbitrary = sized genV2alpha1JobTemplateSpec

genV2alpha1JobTemplateSpec :: Int -> Gen V2alpha1JobTemplateSpec
genV2alpha1JobTemplateSpec n =
  V2alpha1JobTemplateSpec
    <$> arbitraryReducedMaybe n -- v2alpha1JobTemplateSpecMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v2alpha1JobTemplateSpecSpec :: Maybe V1JobSpec
  
instance Arbitrary V2beta1CrossVersionObjectReference where
  arbitrary = sized genV2beta1CrossVersionObjectReference

genV2beta1CrossVersionObjectReference :: Int -> Gen V2beta1CrossVersionObjectReference
genV2beta1CrossVersionObjectReference n =
  V2beta1CrossVersionObjectReference
    <$> arbitraryReducedMaybe n -- v2beta1CrossVersionObjectReferenceApiVersion :: Maybe Text
    <*> arbitrary -- v2beta1CrossVersionObjectReferenceKind :: Text
    <*> arbitrary -- v2beta1CrossVersionObjectReferenceName :: Text
  
instance Arbitrary V2beta1HorizontalPodAutoscaler where
  arbitrary = sized genV2beta1HorizontalPodAutoscaler

genV2beta1HorizontalPodAutoscaler :: Int -> Gen V2beta1HorizontalPodAutoscaler
genV2beta1HorizontalPodAutoscaler n =
  V2beta1HorizontalPodAutoscaler
    <$> arbitraryReducedMaybe n -- v2beta1HorizontalPodAutoscalerApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v2beta1HorizontalPodAutoscalerKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v2beta1HorizontalPodAutoscalerMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v2beta1HorizontalPodAutoscalerSpec :: Maybe V2beta1HorizontalPodAutoscalerSpec
    <*> arbitraryReducedMaybe n -- v2beta1HorizontalPodAutoscalerStatus :: Maybe V2beta1HorizontalPodAutoscalerStatus
  
instance Arbitrary V2beta1HorizontalPodAutoscalerCondition where
  arbitrary = sized genV2beta1HorizontalPodAutoscalerCondition

genV2beta1HorizontalPodAutoscalerCondition :: Int -> Gen V2beta1HorizontalPodAutoscalerCondition
genV2beta1HorizontalPodAutoscalerCondition n =
  V2beta1HorizontalPodAutoscalerCondition
    <$> arbitraryReducedMaybe n -- v2beta1HorizontalPodAutoscalerConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v2beta1HorizontalPodAutoscalerConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v2beta1HorizontalPodAutoscalerConditionReason :: Maybe Text
    <*> arbitrary -- v2beta1HorizontalPodAutoscalerConditionStatus :: Text
    <*> arbitrary -- v2beta1HorizontalPodAutoscalerConditionType :: Text
  
instance Arbitrary V2beta1HorizontalPodAutoscalerList where
  arbitrary = sized genV2beta1HorizontalPodAutoscalerList

genV2beta1HorizontalPodAutoscalerList :: Int -> Gen V2beta1HorizontalPodAutoscalerList
genV2beta1HorizontalPodAutoscalerList n =
  V2beta1HorizontalPodAutoscalerList
    <$> arbitraryReducedMaybe n -- v2beta1HorizontalPodAutoscalerListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v2beta1HorizontalPodAutoscalerListItems :: [V2beta1HorizontalPodAutoscaler]
    <*> arbitraryReducedMaybe n -- v2beta1HorizontalPodAutoscalerListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v2beta1HorizontalPodAutoscalerListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V2beta1HorizontalPodAutoscalerSpec where
  arbitrary = sized genV2beta1HorizontalPodAutoscalerSpec

genV2beta1HorizontalPodAutoscalerSpec :: Int -> Gen V2beta1HorizontalPodAutoscalerSpec
genV2beta1HorizontalPodAutoscalerSpec n =
  V2beta1HorizontalPodAutoscalerSpec
    <$> arbitrary -- v2beta1HorizontalPodAutoscalerSpecMaxReplicas :: Int
    <*> arbitraryReducedMaybe n -- v2beta1HorizontalPodAutoscalerSpecMetrics :: Maybe [V2beta1MetricSpec]
    <*> arbitraryReducedMaybe n -- v2beta1HorizontalPodAutoscalerSpecMinReplicas :: Maybe Int
    <*> arbitraryReduced n -- v2beta1HorizontalPodAutoscalerSpecScaleTargetRef :: V2beta1CrossVersionObjectReference
  
instance Arbitrary V2beta1HorizontalPodAutoscalerStatus where
  arbitrary = sized genV2beta1HorizontalPodAutoscalerStatus

genV2beta1HorizontalPodAutoscalerStatus :: Int -> Gen V2beta1HorizontalPodAutoscalerStatus
genV2beta1HorizontalPodAutoscalerStatus n =
  V2beta1HorizontalPodAutoscalerStatus
    <$> arbitraryReduced n -- v2beta1HorizontalPodAutoscalerStatusConditions :: [V2beta1HorizontalPodAutoscalerCondition]
    <*> arbitraryReduced n -- v2beta1HorizontalPodAutoscalerStatusCurrentMetrics :: [V2beta1MetricStatus]
    <*> arbitrary -- v2beta1HorizontalPodAutoscalerStatusCurrentReplicas :: Int
    <*> arbitrary -- v2beta1HorizontalPodAutoscalerStatusDesiredReplicas :: Int
    <*> arbitraryReducedMaybe n -- v2beta1HorizontalPodAutoscalerStatusLastScaleTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v2beta1HorizontalPodAutoscalerStatusObservedGeneration :: Maybe Integer
  
instance Arbitrary V2beta1MetricSpec where
  arbitrary = sized genV2beta1MetricSpec

genV2beta1MetricSpec :: Int -> Gen V2beta1MetricSpec
genV2beta1MetricSpec n =
  V2beta1MetricSpec
    <$> arbitraryReducedMaybe n -- v2beta1MetricSpecObject :: Maybe V2beta1ObjectMetricSource
    <*> arbitraryReducedMaybe n -- v2beta1MetricSpecPods :: Maybe V2beta1PodsMetricSource
    <*> arbitraryReducedMaybe n -- v2beta1MetricSpecResource :: Maybe V2beta1ResourceMetricSource
    <*> arbitrary -- v2beta1MetricSpecType :: Text
  
instance Arbitrary V2beta1MetricStatus where
  arbitrary = sized genV2beta1MetricStatus

genV2beta1MetricStatus :: Int -> Gen V2beta1MetricStatus
genV2beta1MetricStatus n =
  V2beta1MetricStatus
    <$> arbitraryReducedMaybe n -- v2beta1MetricStatusObject :: Maybe V2beta1ObjectMetricStatus
    <*> arbitraryReducedMaybe n -- v2beta1MetricStatusPods :: Maybe V2beta1PodsMetricStatus
    <*> arbitraryReducedMaybe n -- v2beta1MetricStatusResource :: Maybe V2beta1ResourceMetricStatus
    <*> arbitrary -- v2beta1MetricStatusType :: Text
  
instance Arbitrary V2beta1ObjectMetricSource where
  arbitrary = sized genV2beta1ObjectMetricSource

genV2beta1ObjectMetricSource :: Int -> Gen V2beta1ObjectMetricSource
genV2beta1ObjectMetricSource n =
  V2beta1ObjectMetricSource
    <$> arbitrary -- v2beta1ObjectMetricSourceMetricName :: Text
    <*> arbitraryReduced n -- v2beta1ObjectMetricSourceTarget :: V2beta1CrossVersionObjectReference
    <*> arbitrary -- v2beta1ObjectMetricSourceTargetValue :: Text
  
instance Arbitrary V2beta1ObjectMetricStatus where
  arbitrary = sized genV2beta1ObjectMetricStatus

genV2beta1ObjectMetricStatus :: Int -> Gen V2beta1ObjectMetricStatus
genV2beta1ObjectMetricStatus n =
  V2beta1ObjectMetricStatus
    <$> arbitrary -- v2beta1ObjectMetricStatusCurrentValue :: Text
    <*> arbitrary -- v2beta1ObjectMetricStatusMetricName :: Text
    <*> arbitraryReduced n -- v2beta1ObjectMetricStatusTarget :: V2beta1CrossVersionObjectReference
  
instance Arbitrary V2beta1PodsMetricSource where
  arbitrary = sized genV2beta1PodsMetricSource

genV2beta1PodsMetricSource :: Int -> Gen V2beta1PodsMetricSource
genV2beta1PodsMetricSource n =
  V2beta1PodsMetricSource
    <$> arbitrary -- v2beta1PodsMetricSourceMetricName :: Text
    <*> arbitrary -- v2beta1PodsMetricSourceTargetAverageValue :: Text
  
instance Arbitrary V2beta1PodsMetricStatus where
  arbitrary = sized genV2beta1PodsMetricStatus

genV2beta1PodsMetricStatus :: Int -> Gen V2beta1PodsMetricStatus
genV2beta1PodsMetricStatus n =
  V2beta1PodsMetricStatus
    <$> arbitrary -- v2beta1PodsMetricStatusCurrentAverageValue :: Text
    <*> arbitrary -- v2beta1PodsMetricStatusMetricName :: Text
  
instance Arbitrary V2beta1ResourceMetricSource where
  arbitrary = sized genV2beta1ResourceMetricSource

genV2beta1ResourceMetricSource :: Int -> Gen V2beta1ResourceMetricSource
genV2beta1ResourceMetricSource n =
  V2beta1ResourceMetricSource
    <$> arbitrary -- v2beta1ResourceMetricSourceName :: Text
    <*> arbitraryReducedMaybe n -- v2beta1ResourceMetricSourceTargetAverageUtilization :: Maybe Int
    <*> arbitraryReducedMaybe n -- v2beta1ResourceMetricSourceTargetAverageValue :: Maybe Text
  
instance Arbitrary V2beta1ResourceMetricStatus where
  arbitrary = sized genV2beta1ResourceMetricStatus

genV2beta1ResourceMetricStatus :: Int -> Gen V2beta1ResourceMetricStatus
genV2beta1ResourceMetricStatus n =
  V2beta1ResourceMetricStatus
    <$> arbitraryReducedMaybe n -- v2beta1ResourceMetricStatusCurrentAverageUtilization :: Maybe Int
    <*> arbitrary -- v2beta1ResourceMetricStatusCurrentAverageValue :: Text
    <*> arbitrary -- v2beta1ResourceMetricStatusName :: Text
  
instance Arbitrary VersionInfo where
  arbitrary = sized genVersionInfo

genVersionInfo :: Int -> Gen VersionInfo
genVersionInfo n =
  VersionInfo
    <$> arbitrary -- versionInfoBuildDate :: Text
    <*> arbitrary -- versionInfoCompiler :: Text
    <*> arbitrary -- versionInfoGitCommit :: Text
    <*> arbitrary -- versionInfoGitTreeState :: Text
    <*> arbitrary -- versionInfoGitVersion :: Text
    <*> arbitrary -- versionInfoGoVersion :: Text
    <*> arbitrary -- versionInfoMajor :: Text
    <*> arbitrary -- versionInfoMinor :: Text
    <*> arbitrary -- versionInfoPlatform :: Text
  



