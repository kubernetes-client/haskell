{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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

-- * Models
 
instance Arbitrary AdmissionregistrationV1beta1ServiceReference where
  arbitrary =
    AdmissionregistrationV1beta1ServiceReference
      <$> arbitrary -- admissionregistrationV1beta1ServiceReferenceName :: Text
      <*> arbitrary -- admissionregistrationV1beta1ServiceReferenceNamespace :: Text
      <*> arbitrary -- admissionregistrationV1beta1ServiceReferencePath :: Maybe Text
    
instance Arbitrary ApiregistrationV1beta1ServiceReference where
  arbitrary =
    ApiregistrationV1beta1ServiceReference
      <$> arbitrary -- apiregistrationV1beta1ServiceReferenceName :: Maybe Text
      <*> arbitrary -- apiregistrationV1beta1ServiceReferenceNamespace :: Maybe Text
    
instance Arbitrary AppsV1beta1Deployment where
  arbitrary =
    AppsV1beta1Deployment
      <$> arbitrary -- appsV1beta1DeploymentApiVersion :: Maybe Text
      <*> arbitrary -- appsV1beta1DeploymentKind :: Maybe Text
      <*> arbitrary -- appsV1beta1DeploymentMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- appsV1beta1DeploymentSpec :: Maybe AppsV1beta1DeploymentSpec
      <*> arbitrary -- appsV1beta1DeploymentStatus :: Maybe AppsV1beta1DeploymentStatus
    
instance Arbitrary AppsV1beta1DeploymentCondition where
  arbitrary =
    AppsV1beta1DeploymentCondition
      <$> arbitrary -- appsV1beta1DeploymentConditionLastTransitionTime :: Maybe DateTime
      <*> arbitrary -- appsV1beta1DeploymentConditionLastUpdateTime :: Maybe DateTime
      <*> arbitrary -- appsV1beta1DeploymentConditionMessage :: Maybe Text
      <*> arbitrary -- appsV1beta1DeploymentConditionReason :: Maybe Text
      <*> arbitrary -- appsV1beta1DeploymentConditionStatus :: Text
      <*> arbitrary -- appsV1beta1DeploymentConditionType :: Text
    
instance Arbitrary AppsV1beta1DeploymentList where
  arbitrary =
    AppsV1beta1DeploymentList
      <$> arbitrary -- appsV1beta1DeploymentListApiVersion :: Maybe Text
      <*> arbitrary -- appsV1beta1DeploymentListItems :: [AppsV1beta1Deployment]
      <*> arbitrary -- appsV1beta1DeploymentListKind :: Maybe Text
      <*> arbitrary -- appsV1beta1DeploymentListMetadata :: Maybe V1ListMeta
    
instance Arbitrary AppsV1beta1DeploymentRollback where
  arbitrary =
    AppsV1beta1DeploymentRollback
      <$> arbitrary -- appsV1beta1DeploymentRollbackApiVersion :: Maybe Text
      <*> arbitrary -- appsV1beta1DeploymentRollbackKind :: Maybe Text
      <*> arbitrary -- appsV1beta1DeploymentRollbackName :: Text
      <*> arbitrary -- appsV1beta1DeploymentRollbackRollbackTo :: AppsV1beta1RollbackConfig
      <*> arbitrary -- appsV1beta1DeploymentRollbackUpdatedAnnotations :: Maybe (Map.Map String Text)
    
instance Arbitrary AppsV1beta1DeploymentSpec where
  arbitrary =
    AppsV1beta1DeploymentSpec
      <$> arbitrary -- appsV1beta1DeploymentSpecMinReadySeconds :: Maybe Int
      <*> arbitrary -- appsV1beta1DeploymentSpecPaused :: Maybe Bool
      <*> arbitrary -- appsV1beta1DeploymentSpecProgressDeadlineSeconds :: Maybe Int
      <*> arbitrary -- appsV1beta1DeploymentSpecReplicas :: Maybe Int
      <*> arbitrary -- appsV1beta1DeploymentSpecRevisionHistoryLimit :: Maybe Int
      <*> arbitrary -- appsV1beta1DeploymentSpecRollbackTo :: Maybe AppsV1beta1RollbackConfig
      <*> arbitrary -- appsV1beta1DeploymentSpecSelector :: Maybe V1LabelSelector
      <*> arbitrary -- appsV1beta1DeploymentSpecStrategy :: Maybe AppsV1beta1DeploymentStrategy
      <*> arbitrary -- appsV1beta1DeploymentSpecTemplate :: V1PodTemplateSpec
    
instance Arbitrary AppsV1beta1DeploymentStatus where
  arbitrary =
    AppsV1beta1DeploymentStatus
      <$> arbitrary -- appsV1beta1DeploymentStatusAvailableReplicas :: Maybe Int
      <*> arbitrary -- appsV1beta1DeploymentStatusCollisionCount :: Maybe Int
      <*> arbitrary -- appsV1beta1DeploymentStatusConditions :: Maybe [AppsV1beta1DeploymentCondition]
      <*> arbitrary -- appsV1beta1DeploymentStatusObservedGeneration :: Maybe Integer
      <*> arbitrary -- appsV1beta1DeploymentStatusReadyReplicas :: Maybe Int
      <*> arbitrary -- appsV1beta1DeploymentStatusReplicas :: Maybe Int
      <*> arbitrary -- appsV1beta1DeploymentStatusUnavailableReplicas :: Maybe Int
      <*> arbitrary -- appsV1beta1DeploymentStatusUpdatedReplicas :: Maybe Int
    
instance Arbitrary AppsV1beta1DeploymentStrategy where
  arbitrary =
    AppsV1beta1DeploymentStrategy
      <$> arbitrary -- appsV1beta1DeploymentStrategyRollingUpdate :: Maybe AppsV1beta1RollingUpdateDeployment
      <*> arbitrary -- appsV1beta1DeploymentStrategyType :: Maybe Text
    
instance Arbitrary AppsV1beta1RollbackConfig where
  arbitrary =
    AppsV1beta1RollbackConfig
      <$> arbitrary -- appsV1beta1RollbackConfigRevision :: Maybe Integer
    
instance Arbitrary AppsV1beta1RollingUpdateDeployment where
  arbitrary =
    AppsV1beta1RollingUpdateDeployment
      <$> arbitrary -- appsV1beta1RollingUpdateDeploymentMaxSurge :: Maybe A.Value
      <*> arbitrary -- appsV1beta1RollingUpdateDeploymentMaxUnavailable :: Maybe A.Value
    
instance Arbitrary AppsV1beta1Scale where
  arbitrary =
    AppsV1beta1Scale
      <$> arbitrary -- appsV1beta1ScaleApiVersion :: Maybe Text
      <*> arbitrary -- appsV1beta1ScaleKind :: Maybe Text
      <*> arbitrary -- appsV1beta1ScaleMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- appsV1beta1ScaleSpec :: Maybe AppsV1beta1ScaleSpec
      <*> arbitrary -- appsV1beta1ScaleStatus :: Maybe AppsV1beta1ScaleStatus
    
instance Arbitrary AppsV1beta1ScaleSpec where
  arbitrary =
    AppsV1beta1ScaleSpec
      <$> arbitrary -- appsV1beta1ScaleSpecReplicas :: Maybe Int
    
instance Arbitrary AppsV1beta1ScaleStatus where
  arbitrary =
    AppsV1beta1ScaleStatus
      <$> arbitrary -- appsV1beta1ScaleStatusReplicas :: Int
      <*> arbitrary -- appsV1beta1ScaleStatusSelector :: Maybe (Map.Map String Text)
      <*> arbitrary -- appsV1beta1ScaleStatusTargetSelector :: Maybe Text
    
instance Arbitrary ExtensionsV1beta1Deployment where
  arbitrary =
    ExtensionsV1beta1Deployment
      <$> arbitrary -- extensionsV1beta1DeploymentApiVersion :: Maybe Text
      <*> arbitrary -- extensionsV1beta1DeploymentKind :: Maybe Text
      <*> arbitrary -- extensionsV1beta1DeploymentMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- extensionsV1beta1DeploymentSpec :: Maybe ExtensionsV1beta1DeploymentSpec
      <*> arbitrary -- extensionsV1beta1DeploymentStatus :: Maybe ExtensionsV1beta1DeploymentStatus
    
instance Arbitrary ExtensionsV1beta1DeploymentCondition where
  arbitrary =
    ExtensionsV1beta1DeploymentCondition
      <$> arbitrary -- extensionsV1beta1DeploymentConditionLastTransitionTime :: Maybe DateTime
      <*> arbitrary -- extensionsV1beta1DeploymentConditionLastUpdateTime :: Maybe DateTime
      <*> arbitrary -- extensionsV1beta1DeploymentConditionMessage :: Maybe Text
      <*> arbitrary -- extensionsV1beta1DeploymentConditionReason :: Maybe Text
      <*> arbitrary -- extensionsV1beta1DeploymentConditionStatus :: Text
      <*> arbitrary -- extensionsV1beta1DeploymentConditionType :: Text
    
instance Arbitrary ExtensionsV1beta1DeploymentList where
  arbitrary =
    ExtensionsV1beta1DeploymentList
      <$> arbitrary -- extensionsV1beta1DeploymentListApiVersion :: Maybe Text
      <*> arbitrary -- extensionsV1beta1DeploymentListItems :: [ExtensionsV1beta1Deployment]
      <*> arbitrary -- extensionsV1beta1DeploymentListKind :: Maybe Text
      <*> arbitrary -- extensionsV1beta1DeploymentListMetadata :: Maybe V1ListMeta
    
instance Arbitrary ExtensionsV1beta1DeploymentRollback where
  arbitrary =
    ExtensionsV1beta1DeploymentRollback
      <$> arbitrary -- extensionsV1beta1DeploymentRollbackApiVersion :: Maybe Text
      <*> arbitrary -- extensionsV1beta1DeploymentRollbackKind :: Maybe Text
      <*> arbitrary -- extensionsV1beta1DeploymentRollbackName :: Text
      <*> arbitrary -- extensionsV1beta1DeploymentRollbackRollbackTo :: ExtensionsV1beta1RollbackConfig
      <*> arbitrary -- extensionsV1beta1DeploymentRollbackUpdatedAnnotations :: Maybe (Map.Map String Text)
    
instance Arbitrary ExtensionsV1beta1DeploymentSpec where
  arbitrary =
    ExtensionsV1beta1DeploymentSpec
      <$> arbitrary -- extensionsV1beta1DeploymentSpecMinReadySeconds :: Maybe Int
      <*> arbitrary -- extensionsV1beta1DeploymentSpecPaused :: Maybe Bool
      <*> arbitrary -- extensionsV1beta1DeploymentSpecProgressDeadlineSeconds :: Maybe Int
      <*> arbitrary -- extensionsV1beta1DeploymentSpecReplicas :: Maybe Int
      <*> arbitrary -- extensionsV1beta1DeploymentSpecRevisionHistoryLimit :: Maybe Int
      <*> arbitrary -- extensionsV1beta1DeploymentSpecRollbackTo :: Maybe ExtensionsV1beta1RollbackConfig
      <*> arbitrary -- extensionsV1beta1DeploymentSpecSelector :: Maybe V1LabelSelector
      <*> arbitrary -- extensionsV1beta1DeploymentSpecStrategy :: Maybe ExtensionsV1beta1DeploymentStrategy
      <*> arbitrary -- extensionsV1beta1DeploymentSpecTemplate :: V1PodTemplateSpec
    
instance Arbitrary ExtensionsV1beta1DeploymentStatus where
  arbitrary =
    ExtensionsV1beta1DeploymentStatus
      <$> arbitrary -- extensionsV1beta1DeploymentStatusAvailableReplicas :: Maybe Int
      <*> arbitrary -- extensionsV1beta1DeploymentStatusCollisionCount :: Maybe Int
      <*> arbitrary -- extensionsV1beta1DeploymentStatusConditions :: Maybe [ExtensionsV1beta1DeploymentCondition]
      <*> arbitrary -- extensionsV1beta1DeploymentStatusObservedGeneration :: Maybe Integer
      <*> arbitrary -- extensionsV1beta1DeploymentStatusReadyReplicas :: Maybe Int
      <*> arbitrary -- extensionsV1beta1DeploymentStatusReplicas :: Maybe Int
      <*> arbitrary -- extensionsV1beta1DeploymentStatusUnavailableReplicas :: Maybe Int
      <*> arbitrary -- extensionsV1beta1DeploymentStatusUpdatedReplicas :: Maybe Int
    
instance Arbitrary ExtensionsV1beta1DeploymentStrategy where
  arbitrary =
    ExtensionsV1beta1DeploymentStrategy
      <$> arbitrary -- extensionsV1beta1DeploymentStrategyRollingUpdate :: Maybe ExtensionsV1beta1RollingUpdateDeployment
      <*> arbitrary -- extensionsV1beta1DeploymentStrategyType :: Maybe Text
    
instance Arbitrary ExtensionsV1beta1RollbackConfig where
  arbitrary =
    ExtensionsV1beta1RollbackConfig
      <$> arbitrary -- extensionsV1beta1RollbackConfigRevision :: Maybe Integer
    
instance Arbitrary ExtensionsV1beta1RollingUpdateDeployment where
  arbitrary =
    ExtensionsV1beta1RollingUpdateDeployment
      <$> arbitrary -- extensionsV1beta1RollingUpdateDeploymentMaxSurge :: Maybe A.Value
      <*> arbitrary -- extensionsV1beta1RollingUpdateDeploymentMaxUnavailable :: Maybe A.Value
    
instance Arbitrary ExtensionsV1beta1Scale where
  arbitrary =
    ExtensionsV1beta1Scale
      <$> arbitrary -- extensionsV1beta1ScaleApiVersion :: Maybe Text
      <*> arbitrary -- extensionsV1beta1ScaleKind :: Maybe Text
      <*> arbitrary -- extensionsV1beta1ScaleMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- extensionsV1beta1ScaleSpec :: Maybe ExtensionsV1beta1ScaleSpec
      <*> arbitrary -- extensionsV1beta1ScaleStatus :: Maybe ExtensionsV1beta1ScaleStatus
    
instance Arbitrary ExtensionsV1beta1ScaleSpec where
  arbitrary =
    ExtensionsV1beta1ScaleSpec
      <$> arbitrary -- extensionsV1beta1ScaleSpecReplicas :: Maybe Int
    
instance Arbitrary ExtensionsV1beta1ScaleStatus where
  arbitrary =
    ExtensionsV1beta1ScaleStatus
      <$> arbitrary -- extensionsV1beta1ScaleStatusReplicas :: Int
      <*> arbitrary -- extensionsV1beta1ScaleStatusSelector :: Maybe (Map.Map String Text)
      <*> arbitrary -- extensionsV1beta1ScaleStatusTargetSelector :: Maybe Text
    
instance Arbitrary RuntimeRawExtension where
  arbitrary =
    RuntimeRawExtension
      <$> arbitrary -- runtimeRawExtensionRaw :: ByteArray
    
instance Arbitrary V1APIGroup where
  arbitrary =
    V1APIGroup
      <$> arbitrary -- v1APIGroupApiVersion :: Maybe Text
      <*> arbitrary -- v1APIGroupKind :: Maybe Text
      <*> arbitrary -- v1APIGroupName :: Text
      <*> arbitrary -- v1APIGroupPreferredVersion :: Maybe V1GroupVersionForDiscovery
      <*> arbitrary -- v1APIGroupServerAddressByClientCidRs :: [V1ServerAddressByClientCIDR]
      <*> arbitrary -- v1APIGroupVersions :: [V1GroupVersionForDiscovery]
    
instance Arbitrary V1APIGroupList where
  arbitrary =
    V1APIGroupList
      <$> arbitrary -- v1APIGroupListApiVersion :: Maybe Text
      <*> arbitrary -- v1APIGroupListGroups :: [V1APIGroup]
      <*> arbitrary -- v1APIGroupListKind :: Maybe Text
    
instance Arbitrary V1APIResource where
  arbitrary =
    V1APIResource
      <$> arbitrary -- v1APIResourceCategories :: Maybe [Text]
      <*> arbitrary -- v1APIResourceGroup :: Maybe Text
      <*> arbitrary -- v1APIResourceKind :: Text
      <*> arbitrary -- v1APIResourceName :: Text
      <*> arbitrary -- v1APIResourceNamespaced :: Bool
      <*> arbitrary -- v1APIResourceShortNames :: Maybe [Text]
      <*> arbitrary -- v1APIResourceSingularName :: Text
      <*> arbitrary -- v1APIResourceVerbs :: [Text]
      <*> arbitrary -- v1APIResourceVersion :: Maybe Text
    
instance Arbitrary V1APIResourceList where
  arbitrary =
    V1APIResourceList
      <$> arbitrary -- v1APIResourceListApiVersion :: Maybe Text
      <*> arbitrary -- v1APIResourceListGroupVersion :: Text
      <*> arbitrary -- v1APIResourceListKind :: Maybe Text
      <*> arbitrary -- v1APIResourceListResources :: [V1APIResource]
    
instance Arbitrary V1APIVersions where
  arbitrary =
    V1APIVersions
      <$> arbitrary -- v1APIVersionsApiVersion :: Maybe Text
      <*> arbitrary -- v1APIVersionsKind :: Maybe Text
      <*> arbitrary -- v1APIVersionsServerAddressByClientCidRs :: [V1ServerAddressByClientCIDR]
      <*> arbitrary -- v1APIVersionsVersions :: [Text]
    
instance Arbitrary V1AWSElasticBlockStoreVolumeSource where
  arbitrary =
    V1AWSElasticBlockStoreVolumeSource
      <$> arbitrary -- v1AWSElasticBlockStoreVolumeSourceFsType :: Maybe Text
      <*> arbitrary -- v1AWSElasticBlockStoreVolumeSourcePartition :: Maybe Int
      <*> arbitrary -- v1AWSElasticBlockStoreVolumeSourceReadOnly :: Maybe Bool
      <*> arbitrary -- v1AWSElasticBlockStoreVolumeSourceVolumeId :: Text
    
instance Arbitrary V1Affinity where
  arbitrary =
    V1Affinity
      <$> arbitrary -- v1AffinityNodeAffinity :: Maybe V1NodeAffinity
      <*> arbitrary -- v1AffinityPodAffinity :: Maybe V1PodAffinity
      <*> arbitrary -- v1AffinityPodAntiAffinity :: Maybe V1PodAntiAffinity
    
instance Arbitrary V1AggregationRule where
  arbitrary =
    V1AggregationRule
      <$> arbitrary -- v1AggregationRuleClusterRoleSelectors :: Maybe [V1LabelSelector]
    
instance Arbitrary V1AttachedVolume where
  arbitrary =
    V1AttachedVolume
      <$> arbitrary -- v1AttachedVolumeDevicePath :: Text
      <*> arbitrary -- v1AttachedVolumeName :: Text
    
instance Arbitrary V1AzureDiskVolumeSource where
  arbitrary =
    V1AzureDiskVolumeSource
      <$> arbitrary -- v1AzureDiskVolumeSourceCachingMode :: Maybe Text
      <*> arbitrary -- v1AzureDiskVolumeSourceDiskName :: Text
      <*> arbitrary -- v1AzureDiskVolumeSourceDiskUri :: Text
      <*> arbitrary -- v1AzureDiskVolumeSourceFsType :: Maybe Text
      <*> arbitrary -- v1AzureDiskVolumeSourceKind :: Maybe Text
      <*> arbitrary -- v1AzureDiskVolumeSourceReadOnly :: Maybe Bool
    
instance Arbitrary V1AzureFilePersistentVolumeSource where
  arbitrary =
    V1AzureFilePersistentVolumeSource
      <$> arbitrary -- v1AzureFilePersistentVolumeSourceReadOnly :: Maybe Bool
      <*> arbitrary -- v1AzureFilePersistentVolumeSourceSecretName :: Text
      <*> arbitrary -- v1AzureFilePersistentVolumeSourceSecretNamespace :: Maybe Text
      <*> arbitrary -- v1AzureFilePersistentVolumeSourceShareName :: Text
    
instance Arbitrary V1AzureFileVolumeSource where
  arbitrary =
    V1AzureFileVolumeSource
      <$> arbitrary -- v1AzureFileVolumeSourceReadOnly :: Maybe Bool
      <*> arbitrary -- v1AzureFileVolumeSourceSecretName :: Text
      <*> arbitrary -- v1AzureFileVolumeSourceShareName :: Text
    
instance Arbitrary V1Binding where
  arbitrary =
    V1Binding
      <$> arbitrary -- v1BindingApiVersion :: Maybe Text
      <*> arbitrary -- v1BindingKind :: Maybe Text
      <*> arbitrary -- v1BindingMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1BindingTarget :: V1ObjectReference
    
instance Arbitrary V1CSIPersistentVolumeSource where
  arbitrary =
    V1CSIPersistentVolumeSource
      <$> arbitrary -- v1CSIPersistentVolumeSourceDriver :: Text
      <*> arbitrary -- v1CSIPersistentVolumeSourceReadOnly :: Maybe Bool
      <*> arbitrary -- v1CSIPersistentVolumeSourceVolumeHandle :: Text
    
instance Arbitrary V1Capabilities where
  arbitrary =
    V1Capabilities
      <$> arbitrary -- v1CapabilitiesAdd :: Maybe [Text]
      <*> arbitrary -- v1CapabilitiesDrop :: Maybe [Text]
    
instance Arbitrary V1CephFSPersistentVolumeSource where
  arbitrary =
    V1CephFSPersistentVolumeSource
      <$> arbitrary -- v1CephFSPersistentVolumeSourceMonitors :: [Text]
      <*> arbitrary -- v1CephFSPersistentVolumeSourcePath :: Maybe Text
      <*> arbitrary -- v1CephFSPersistentVolumeSourceReadOnly :: Maybe Bool
      <*> arbitrary -- v1CephFSPersistentVolumeSourceSecretFile :: Maybe Text
      <*> arbitrary -- v1CephFSPersistentVolumeSourceSecretRef :: Maybe V1SecretReference
      <*> arbitrary -- v1CephFSPersistentVolumeSourceUser :: Maybe Text
    
instance Arbitrary V1CephFSVolumeSource where
  arbitrary =
    V1CephFSVolumeSource
      <$> arbitrary -- v1CephFSVolumeSourceMonitors :: [Text]
      <*> arbitrary -- v1CephFSVolumeSourcePath :: Maybe Text
      <*> arbitrary -- v1CephFSVolumeSourceReadOnly :: Maybe Bool
      <*> arbitrary -- v1CephFSVolumeSourceSecretFile :: Maybe Text
      <*> arbitrary -- v1CephFSVolumeSourceSecretRef :: Maybe V1LocalObjectReference
      <*> arbitrary -- v1CephFSVolumeSourceUser :: Maybe Text
    
instance Arbitrary V1CinderVolumeSource where
  arbitrary =
    V1CinderVolumeSource
      <$> arbitrary -- v1CinderVolumeSourceFsType :: Maybe Text
      <*> arbitrary -- v1CinderVolumeSourceReadOnly :: Maybe Bool
      <*> arbitrary -- v1CinderVolumeSourceVolumeId :: Text
    
instance Arbitrary V1ClientIPConfig where
  arbitrary =
    V1ClientIPConfig
      <$> arbitrary -- v1ClientIPConfigTimeoutSeconds :: Maybe Int
    
instance Arbitrary V1ClusterRole where
  arbitrary =
    V1ClusterRole
      <$> arbitrary -- v1ClusterRoleAggregationRule :: Maybe V1AggregationRule
      <*> arbitrary -- v1ClusterRoleApiVersion :: Maybe Text
      <*> arbitrary -- v1ClusterRoleKind :: Maybe Text
      <*> arbitrary -- v1ClusterRoleMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1ClusterRoleRules :: [V1PolicyRule]
    
instance Arbitrary V1ClusterRoleBinding where
  arbitrary =
    V1ClusterRoleBinding
      <$> arbitrary -- v1ClusterRoleBindingApiVersion :: Maybe Text
      <*> arbitrary -- v1ClusterRoleBindingKind :: Maybe Text
      <*> arbitrary -- v1ClusterRoleBindingMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1ClusterRoleBindingRoleRef :: V1RoleRef
      <*> arbitrary -- v1ClusterRoleBindingSubjects :: [V1Subject]
    
instance Arbitrary V1ClusterRoleBindingList where
  arbitrary =
    V1ClusterRoleBindingList
      <$> arbitrary -- v1ClusterRoleBindingListApiVersion :: Maybe Text
      <*> arbitrary -- v1ClusterRoleBindingListItems :: [V1ClusterRoleBinding]
      <*> arbitrary -- v1ClusterRoleBindingListKind :: Maybe Text
      <*> arbitrary -- v1ClusterRoleBindingListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1ClusterRoleList where
  arbitrary =
    V1ClusterRoleList
      <$> arbitrary -- v1ClusterRoleListApiVersion :: Maybe Text
      <*> arbitrary -- v1ClusterRoleListItems :: [V1ClusterRole]
      <*> arbitrary -- v1ClusterRoleListKind :: Maybe Text
      <*> arbitrary -- v1ClusterRoleListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1ComponentCondition where
  arbitrary =
    V1ComponentCondition
      <$> arbitrary -- v1ComponentConditionError :: Maybe Text
      <*> arbitrary -- v1ComponentConditionMessage :: Maybe Text
      <*> arbitrary -- v1ComponentConditionStatus :: Text
      <*> arbitrary -- v1ComponentConditionType :: Text
    
instance Arbitrary V1ComponentStatus where
  arbitrary =
    V1ComponentStatus
      <$> arbitrary -- v1ComponentStatusApiVersion :: Maybe Text
      <*> arbitrary -- v1ComponentStatusConditions :: Maybe [V1ComponentCondition]
      <*> arbitrary -- v1ComponentStatusKind :: Maybe Text
      <*> arbitrary -- v1ComponentStatusMetadata :: Maybe V1ObjectMeta
    
instance Arbitrary V1ComponentStatusList where
  arbitrary =
    V1ComponentStatusList
      <$> arbitrary -- v1ComponentStatusListApiVersion :: Maybe Text
      <*> arbitrary -- v1ComponentStatusListItems :: [V1ComponentStatus]
      <*> arbitrary -- v1ComponentStatusListKind :: Maybe Text
      <*> arbitrary -- v1ComponentStatusListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1ConfigMap where
  arbitrary =
    V1ConfigMap
      <$> arbitrary -- v1ConfigMapApiVersion :: Maybe Text
      <*> arbitrary -- v1ConfigMapData :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1ConfigMapKind :: Maybe Text
      <*> arbitrary -- v1ConfigMapMetadata :: Maybe V1ObjectMeta
    
instance Arbitrary V1ConfigMapEnvSource where
  arbitrary =
    V1ConfigMapEnvSource
      <$> arbitrary -- v1ConfigMapEnvSourceName :: Maybe Text
      <*> arbitrary -- v1ConfigMapEnvSourceOptional :: Maybe Bool
    
instance Arbitrary V1ConfigMapKeySelector where
  arbitrary =
    V1ConfigMapKeySelector
      <$> arbitrary -- v1ConfigMapKeySelectorKey :: Text
      <*> arbitrary -- v1ConfigMapKeySelectorName :: Maybe Text
      <*> arbitrary -- v1ConfigMapKeySelectorOptional :: Maybe Bool
    
instance Arbitrary V1ConfigMapList where
  arbitrary =
    V1ConfigMapList
      <$> arbitrary -- v1ConfigMapListApiVersion :: Maybe Text
      <*> arbitrary -- v1ConfigMapListItems :: [V1ConfigMap]
      <*> arbitrary -- v1ConfigMapListKind :: Maybe Text
      <*> arbitrary -- v1ConfigMapListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1ConfigMapProjection where
  arbitrary =
    V1ConfigMapProjection
      <$> arbitrary -- v1ConfigMapProjectionItems :: Maybe [V1KeyToPath]
      <*> arbitrary -- v1ConfigMapProjectionName :: Maybe Text
      <*> arbitrary -- v1ConfigMapProjectionOptional :: Maybe Bool
    
instance Arbitrary V1ConfigMapVolumeSource where
  arbitrary =
    V1ConfigMapVolumeSource
      <$> arbitrary -- v1ConfigMapVolumeSourceDefaultMode :: Maybe Int
      <*> arbitrary -- v1ConfigMapVolumeSourceItems :: Maybe [V1KeyToPath]
      <*> arbitrary -- v1ConfigMapVolumeSourceName :: Maybe Text
      <*> arbitrary -- v1ConfigMapVolumeSourceOptional :: Maybe Bool
    
instance Arbitrary V1Container where
  arbitrary =
    V1Container
      <$> arbitrary -- v1ContainerArgs :: Maybe [Text]
      <*> arbitrary -- v1ContainerCommand :: Maybe [Text]
      <*> arbitrary -- v1ContainerEnv :: Maybe [V1EnvVar]
      <*> arbitrary -- v1ContainerEnvFrom :: Maybe [V1EnvFromSource]
      <*> arbitrary -- v1ContainerImage :: Maybe Text
      <*> arbitrary -- v1ContainerImagePullPolicy :: Maybe Text
      <*> arbitrary -- v1ContainerLifecycle :: Maybe V1Lifecycle
      <*> arbitrary -- v1ContainerLivenessProbe :: Maybe V1Probe
      <*> arbitrary -- v1ContainerName :: Text
      <*> arbitrary -- v1ContainerPorts :: Maybe [V1ContainerPort]
      <*> arbitrary -- v1ContainerReadinessProbe :: Maybe V1Probe
      <*> arbitrary -- v1ContainerResources :: Maybe V1ResourceRequirements
      <*> arbitrary -- v1ContainerSecurityContext :: Maybe V1SecurityContext
      <*> arbitrary -- v1ContainerStdin :: Maybe Bool
      <*> arbitrary -- v1ContainerStdinOnce :: Maybe Bool
      <*> arbitrary -- v1ContainerTerminationMessagePath :: Maybe Text
      <*> arbitrary -- v1ContainerTerminationMessagePolicy :: Maybe Text
      <*> arbitrary -- v1ContainerTty :: Maybe Bool
      <*> arbitrary -- v1ContainerVolumeDevices :: Maybe [V1VolumeDevice]
      <*> arbitrary -- v1ContainerVolumeMounts :: Maybe [V1VolumeMount]
      <*> arbitrary -- v1ContainerWorkingDir :: Maybe Text
    
instance Arbitrary V1ContainerImage where
  arbitrary =
    V1ContainerImage
      <$> arbitrary -- v1ContainerImageNames :: [Text]
      <*> arbitrary -- v1ContainerImageSizeBytes :: Maybe Integer
    
instance Arbitrary V1ContainerPort where
  arbitrary =
    V1ContainerPort
      <$> arbitrary -- v1ContainerPortContainerPort :: Int
      <*> arbitrary -- v1ContainerPortHostIp :: Maybe Text
      <*> arbitrary -- v1ContainerPortHostPort :: Maybe Int
      <*> arbitrary -- v1ContainerPortName :: Maybe Text
      <*> arbitrary -- v1ContainerPortProtocol :: Maybe Text
    
instance Arbitrary V1ContainerState where
  arbitrary =
    V1ContainerState
      <$> arbitrary -- v1ContainerStateRunning :: Maybe V1ContainerStateRunning
      <*> arbitrary -- v1ContainerStateTerminated :: Maybe V1ContainerStateTerminated
      <*> arbitrary -- v1ContainerStateWaiting :: Maybe V1ContainerStateWaiting
    
instance Arbitrary V1ContainerStateRunning where
  arbitrary =
    V1ContainerStateRunning
      <$> arbitrary -- v1ContainerStateRunningStartedAt :: Maybe DateTime
    
instance Arbitrary V1ContainerStateTerminated where
  arbitrary =
    V1ContainerStateTerminated
      <$> arbitrary -- v1ContainerStateTerminatedContainerId :: Maybe Text
      <*> arbitrary -- v1ContainerStateTerminatedExitCode :: Int
      <*> arbitrary -- v1ContainerStateTerminatedFinishedAt :: Maybe DateTime
      <*> arbitrary -- v1ContainerStateTerminatedMessage :: Maybe Text
      <*> arbitrary -- v1ContainerStateTerminatedReason :: Maybe Text
      <*> arbitrary -- v1ContainerStateTerminatedSignal :: Maybe Int
      <*> arbitrary -- v1ContainerStateTerminatedStartedAt :: Maybe DateTime
    
instance Arbitrary V1ContainerStateWaiting where
  arbitrary =
    V1ContainerStateWaiting
      <$> arbitrary -- v1ContainerStateWaitingMessage :: Maybe Text
      <*> arbitrary -- v1ContainerStateWaitingReason :: Maybe Text
    
instance Arbitrary V1ContainerStatus where
  arbitrary =
    V1ContainerStatus
      <$> arbitrary -- v1ContainerStatusContainerId :: Maybe Text
      <*> arbitrary -- v1ContainerStatusImage :: Text
      <*> arbitrary -- v1ContainerStatusImageId :: Text
      <*> arbitrary -- v1ContainerStatusLastState :: Maybe V1ContainerState
      <*> arbitrary -- v1ContainerStatusName :: Text
      <*> arbitrary -- v1ContainerStatusReady :: Bool
      <*> arbitrary -- v1ContainerStatusRestartCount :: Int
      <*> arbitrary -- v1ContainerStatusState :: Maybe V1ContainerState
    
instance Arbitrary V1ControllerRevision where
  arbitrary =
    V1ControllerRevision
      <$> arbitrary -- v1ControllerRevisionApiVersion :: Maybe Text
      <*> arbitrary -- v1ControllerRevisionData :: Maybe RuntimeRawExtension
      <*> arbitrary -- v1ControllerRevisionKind :: Maybe Text
      <*> arbitrary -- v1ControllerRevisionMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1ControllerRevisionRevision :: Integer
    
instance Arbitrary V1ControllerRevisionList where
  arbitrary =
    V1ControllerRevisionList
      <$> arbitrary -- v1ControllerRevisionListApiVersion :: Maybe Text
      <*> arbitrary -- v1ControllerRevisionListItems :: [V1ControllerRevision]
      <*> arbitrary -- v1ControllerRevisionListKind :: Maybe Text
      <*> arbitrary -- v1ControllerRevisionListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1CrossVersionObjectReference where
  arbitrary =
    V1CrossVersionObjectReference
      <$> arbitrary -- v1CrossVersionObjectReferenceApiVersion :: Maybe Text
      <*> arbitrary -- v1CrossVersionObjectReferenceKind :: Text
      <*> arbitrary -- v1CrossVersionObjectReferenceName :: Text
    
instance Arbitrary V1DaemonEndpoint where
  arbitrary =
    V1DaemonEndpoint
      <$> arbitrary -- v1DaemonEndpointPort :: Int
    
instance Arbitrary V1DaemonSet where
  arbitrary =
    V1DaemonSet
      <$> arbitrary -- v1DaemonSetApiVersion :: Maybe Text
      <*> arbitrary -- v1DaemonSetKind :: Maybe Text
      <*> arbitrary -- v1DaemonSetMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1DaemonSetSpec :: Maybe V1DaemonSetSpec
      <*> arbitrary -- v1DaemonSetStatus :: Maybe V1DaemonSetStatus
    
instance Arbitrary V1DaemonSetCondition where
  arbitrary =
    V1DaemonSetCondition
      <$> arbitrary -- v1DaemonSetConditionLastTransitionTime :: Maybe DateTime
      <*> arbitrary -- v1DaemonSetConditionMessage :: Maybe Text
      <*> arbitrary -- v1DaemonSetConditionReason :: Maybe Text
      <*> arbitrary -- v1DaemonSetConditionStatus :: Text
      <*> arbitrary -- v1DaemonSetConditionType :: Text
    
instance Arbitrary V1DaemonSetList where
  arbitrary =
    V1DaemonSetList
      <$> arbitrary -- v1DaemonSetListApiVersion :: Maybe Text
      <*> arbitrary -- v1DaemonSetListItems :: [V1DaemonSet]
      <*> arbitrary -- v1DaemonSetListKind :: Maybe Text
      <*> arbitrary -- v1DaemonSetListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1DaemonSetSpec where
  arbitrary =
    V1DaemonSetSpec
      <$> arbitrary -- v1DaemonSetSpecMinReadySeconds :: Maybe Int
      <*> arbitrary -- v1DaemonSetSpecRevisionHistoryLimit :: Maybe Int
      <*> arbitrary -- v1DaemonSetSpecSelector :: V1LabelSelector
      <*> arbitrary -- v1DaemonSetSpecTemplate :: V1PodTemplateSpec
      <*> arbitrary -- v1DaemonSetSpecUpdateStrategy :: Maybe V1DaemonSetUpdateStrategy
    
instance Arbitrary V1DaemonSetStatus where
  arbitrary =
    V1DaemonSetStatus
      <$> arbitrary -- v1DaemonSetStatusCollisionCount :: Maybe Int
      <*> arbitrary -- v1DaemonSetStatusConditions :: Maybe [V1DaemonSetCondition]
      <*> arbitrary -- v1DaemonSetStatusCurrentNumberScheduled :: Int
      <*> arbitrary -- v1DaemonSetStatusDesiredNumberScheduled :: Int
      <*> arbitrary -- v1DaemonSetStatusNumberAvailable :: Maybe Int
      <*> arbitrary -- v1DaemonSetStatusNumberMisscheduled :: Int
      <*> arbitrary -- v1DaemonSetStatusNumberReady :: Int
      <*> arbitrary -- v1DaemonSetStatusNumberUnavailable :: Maybe Int
      <*> arbitrary -- v1DaemonSetStatusObservedGeneration :: Maybe Integer
      <*> arbitrary -- v1DaemonSetStatusUpdatedNumberScheduled :: Maybe Int
    
instance Arbitrary V1DaemonSetUpdateStrategy where
  arbitrary =
    V1DaemonSetUpdateStrategy
      <$> arbitrary -- v1DaemonSetUpdateStrategyRollingUpdate :: Maybe V1RollingUpdateDaemonSet
      <*> arbitrary -- v1DaemonSetUpdateStrategyType :: Maybe Text
    
instance Arbitrary V1DeleteOptions where
  arbitrary =
    V1DeleteOptions
      <$> arbitrary -- v1DeleteOptionsApiVersion :: Maybe Text
      <*> arbitrary -- v1DeleteOptionsGracePeriodSeconds :: Maybe Integer
      <*> arbitrary -- v1DeleteOptionsKind :: Maybe Text
      <*> arbitrary -- v1DeleteOptionsOrphanDependents :: Maybe Bool
      <*> arbitrary -- v1DeleteOptionsPreconditions :: Maybe V1Preconditions
      <*> arbitrary -- v1DeleteOptionsPropagationPolicy :: Maybe Text
    
instance Arbitrary V1Deployment where
  arbitrary =
    V1Deployment
      <$> arbitrary -- v1DeploymentApiVersion :: Maybe Text
      <*> arbitrary -- v1DeploymentKind :: Maybe Text
      <*> arbitrary -- v1DeploymentMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1DeploymentSpec :: Maybe V1DeploymentSpec
      <*> arbitrary -- v1DeploymentStatus :: Maybe V1DeploymentStatus
    
instance Arbitrary V1DeploymentCondition where
  arbitrary =
    V1DeploymentCondition
      <$> arbitrary -- v1DeploymentConditionLastTransitionTime :: Maybe DateTime
      <*> arbitrary -- v1DeploymentConditionLastUpdateTime :: Maybe DateTime
      <*> arbitrary -- v1DeploymentConditionMessage :: Maybe Text
      <*> arbitrary -- v1DeploymentConditionReason :: Maybe Text
      <*> arbitrary -- v1DeploymentConditionStatus :: Text
      <*> arbitrary -- v1DeploymentConditionType :: Text
    
instance Arbitrary V1DeploymentList where
  arbitrary =
    V1DeploymentList
      <$> arbitrary -- v1DeploymentListApiVersion :: Maybe Text
      <*> arbitrary -- v1DeploymentListItems :: [V1Deployment]
      <*> arbitrary -- v1DeploymentListKind :: Maybe Text
      <*> arbitrary -- v1DeploymentListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1DeploymentSpec where
  arbitrary =
    V1DeploymentSpec
      <$> arbitrary -- v1DeploymentSpecMinReadySeconds :: Maybe Int
      <*> arbitrary -- v1DeploymentSpecPaused :: Maybe Bool
      <*> arbitrary -- v1DeploymentSpecProgressDeadlineSeconds :: Maybe Int
      <*> arbitrary -- v1DeploymentSpecReplicas :: Maybe Int
      <*> arbitrary -- v1DeploymentSpecRevisionHistoryLimit :: Maybe Int
      <*> arbitrary -- v1DeploymentSpecSelector :: V1LabelSelector
      <*> arbitrary -- v1DeploymentSpecStrategy :: Maybe V1DeploymentStrategy
      <*> arbitrary -- v1DeploymentSpecTemplate :: V1PodTemplateSpec
    
instance Arbitrary V1DeploymentStatus where
  arbitrary =
    V1DeploymentStatus
      <$> arbitrary -- v1DeploymentStatusAvailableReplicas :: Maybe Int
      <*> arbitrary -- v1DeploymentStatusCollisionCount :: Maybe Int
      <*> arbitrary -- v1DeploymentStatusConditions :: Maybe [V1DeploymentCondition]
      <*> arbitrary -- v1DeploymentStatusObservedGeneration :: Maybe Integer
      <*> arbitrary -- v1DeploymentStatusReadyReplicas :: Maybe Int
      <*> arbitrary -- v1DeploymentStatusReplicas :: Maybe Int
      <*> arbitrary -- v1DeploymentStatusUnavailableReplicas :: Maybe Int
      <*> arbitrary -- v1DeploymentStatusUpdatedReplicas :: Maybe Int
    
instance Arbitrary V1DeploymentStrategy where
  arbitrary =
    V1DeploymentStrategy
      <$> arbitrary -- v1DeploymentStrategyRollingUpdate :: Maybe V1RollingUpdateDeployment
      <*> arbitrary -- v1DeploymentStrategyType :: Maybe Text
    
instance Arbitrary V1DownwardAPIProjection where
  arbitrary =
    V1DownwardAPIProjection
      <$> arbitrary -- v1DownwardAPIProjectionItems :: Maybe [V1DownwardAPIVolumeFile]
    
instance Arbitrary V1DownwardAPIVolumeFile where
  arbitrary =
    V1DownwardAPIVolumeFile
      <$> arbitrary -- v1DownwardAPIVolumeFileFieldRef :: Maybe V1ObjectFieldSelector
      <*> arbitrary -- v1DownwardAPIVolumeFileMode :: Maybe Int
      <*> arbitrary -- v1DownwardAPIVolumeFilePath :: Text
      <*> arbitrary -- v1DownwardAPIVolumeFileResourceFieldRef :: Maybe V1ResourceFieldSelector
    
instance Arbitrary V1DownwardAPIVolumeSource where
  arbitrary =
    V1DownwardAPIVolumeSource
      <$> arbitrary -- v1DownwardAPIVolumeSourceDefaultMode :: Maybe Int
      <*> arbitrary -- v1DownwardAPIVolumeSourceItems :: Maybe [V1DownwardAPIVolumeFile]
    
instance Arbitrary V1EmptyDirVolumeSource where
  arbitrary =
    V1EmptyDirVolumeSource
      <$> arbitrary -- v1EmptyDirVolumeSourceMedium :: Maybe Text
      <*> arbitrary -- v1EmptyDirVolumeSourceSizeLimit :: Maybe Text
    
instance Arbitrary V1EndpointAddress where
  arbitrary =
    V1EndpointAddress
      <$> arbitrary -- v1EndpointAddressHostname :: Maybe Text
      <*> arbitrary -- v1EndpointAddressIp :: Text
      <*> arbitrary -- v1EndpointAddressNodeName :: Maybe Text
      <*> arbitrary -- v1EndpointAddressTargetRef :: Maybe V1ObjectReference
    
instance Arbitrary V1EndpointPort where
  arbitrary =
    V1EndpointPort
      <$> arbitrary -- v1EndpointPortName :: Maybe Text
      <*> arbitrary -- v1EndpointPortPort :: Int
      <*> arbitrary -- v1EndpointPortProtocol :: Maybe Text
    
instance Arbitrary V1EndpointSubset where
  arbitrary =
    V1EndpointSubset
      <$> arbitrary -- v1EndpointSubsetAddresses :: Maybe [V1EndpointAddress]
      <*> arbitrary -- v1EndpointSubsetNotReadyAddresses :: Maybe [V1EndpointAddress]
      <*> arbitrary -- v1EndpointSubsetPorts :: Maybe [V1EndpointPort]
    
instance Arbitrary V1Endpoints where
  arbitrary =
    V1Endpoints
      <$> arbitrary -- v1EndpointsApiVersion :: Maybe Text
      <*> arbitrary -- v1EndpointsKind :: Maybe Text
      <*> arbitrary -- v1EndpointsMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1EndpointsSubsets :: [V1EndpointSubset]
    
instance Arbitrary V1EndpointsList where
  arbitrary =
    V1EndpointsList
      <$> arbitrary -- v1EndpointsListApiVersion :: Maybe Text
      <*> arbitrary -- v1EndpointsListItems :: [V1Endpoints]
      <*> arbitrary -- v1EndpointsListKind :: Maybe Text
      <*> arbitrary -- v1EndpointsListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1EnvFromSource where
  arbitrary =
    V1EnvFromSource
      <$> arbitrary -- v1EnvFromSourceConfigMapRef :: Maybe V1ConfigMapEnvSource
      <*> arbitrary -- v1EnvFromSourcePrefix :: Maybe Text
      <*> arbitrary -- v1EnvFromSourceSecretRef :: Maybe V1SecretEnvSource
    
instance Arbitrary V1EnvVar where
  arbitrary =
    V1EnvVar
      <$> arbitrary -- v1EnvVarName :: Text
      <*> arbitrary -- v1EnvVarValue :: Maybe Text
      <*> arbitrary -- v1EnvVarValueFrom :: Maybe V1EnvVarSource
    
instance Arbitrary V1EnvVarSource where
  arbitrary =
    V1EnvVarSource
      <$> arbitrary -- v1EnvVarSourceConfigMapKeyRef :: Maybe V1ConfigMapKeySelector
      <*> arbitrary -- v1EnvVarSourceFieldRef :: Maybe V1ObjectFieldSelector
      <*> arbitrary -- v1EnvVarSourceResourceFieldRef :: Maybe V1ResourceFieldSelector
      <*> arbitrary -- v1EnvVarSourceSecretKeyRef :: Maybe V1SecretKeySelector
    
instance Arbitrary V1Event where
  arbitrary =
    V1Event
      <$> arbitrary -- v1EventAction :: Maybe Text
      <*> arbitrary -- v1EventApiVersion :: Maybe Text
      <*> arbitrary -- v1EventCount :: Maybe Int
      <*> arbitrary -- v1EventEventTime :: Maybe DateTime
      <*> arbitrary -- v1EventFirstTimestamp :: Maybe DateTime
      <*> arbitrary -- v1EventInvolvedObject :: V1ObjectReference
      <*> arbitrary -- v1EventKind :: Maybe Text
      <*> arbitrary -- v1EventLastTimestamp :: Maybe DateTime
      <*> arbitrary -- v1EventMessage :: Maybe Text
      <*> arbitrary -- v1EventMetadata :: V1ObjectMeta
      <*> arbitrary -- v1EventReason :: Maybe Text
      <*> arbitrary -- v1EventRelated :: Maybe V1ObjectReference
      <*> arbitrary -- v1EventReportingComponent :: Maybe Text
      <*> arbitrary -- v1EventReportingInstance :: Maybe Text
      <*> arbitrary -- v1EventSeries :: Maybe V1EventSeries
      <*> arbitrary -- v1EventSource :: Maybe V1EventSource
      <*> arbitrary -- v1EventType :: Maybe Text
    
instance Arbitrary V1EventList where
  arbitrary =
    V1EventList
      <$> arbitrary -- v1EventListApiVersion :: Maybe Text
      <*> arbitrary -- v1EventListItems :: [V1Event]
      <*> arbitrary -- v1EventListKind :: Maybe Text
      <*> arbitrary -- v1EventListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1EventSeries where
  arbitrary =
    V1EventSeries
      <$> arbitrary -- v1EventSeriesCount :: Maybe Int
      <*> arbitrary -- v1EventSeriesLastObservedTime :: Maybe DateTime
      <*> arbitrary -- v1EventSeriesState :: Maybe Text
    
instance Arbitrary V1EventSource where
  arbitrary =
    V1EventSource
      <$> arbitrary -- v1EventSourceComponent :: Maybe Text
      <*> arbitrary -- v1EventSourceHost :: Maybe Text
    
instance Arbitrary V1ExecAction where
  arbitrary =
    V1ExecAction
      <$> arbitrary -- v1ExecActionCommand :: Maybe [Text]
    
instance Arbitrary V1FCVolumeSource where
  arbitrary =
    V1FCVolumeSource
      <$> arbitrary -- v1FCVolumeSourceFsType :: Maybe Text
      <*> arbitrary -- v1FCVolumeSourceLun :: Maybe Int
      <*> arbitrary -- v1FCVolumeSourceReadOnly :: Maybe Bool
      <*> arbitrary -- v1FCVolumeSourceTargetWwNs :: Maybe [Text]
      <*> arbitrary -- v1FCVolumeSourceWwids :: Maybe [Text]
    
instance Arbitrary V1FlexVolumeSource where
  arbitrary =
    V1FlexVolumeSource
      <$> arbitrary -- v1FlexVolumeSourceDriver :: Text
      <*> arbitrary -- v1FlexVolumeSourceFsType :: Maybe Text
      <*> arbitrary -- v1FlexVolumeSourceOptions :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1FlexVolumeSourceReadOnly :: Maybe Bool
      <*> arbitrary -- v1FlexVolumeSourceSecretRef :: Maybe V1LocalObjectReference
    
instance Arbitrary V1FlockerVolumeSource where
  arbitrary =
    V1FlockerVolumeSource
      <$> arbitrary -- v1FlockerVolumeSourceDatasetName :: Maybe Text
      <*> arbitrary -- v1FlockerVolumeSourceDatasetUuid :: Maybe Text
    
instance Arbitrary V1GCEPersistentDiskVolumeSource where
  arbitrary =
    V1GCEPersistentDiskVolumeSource
      <$> arbitrary -- v1GCEPersistentDiskVolumeSourceFsType :: Maybe Text
      <*> arbitrary -- v1GCEPersistentDiskVolumeSourcePartition :: Maybe Int
      <*> arbitrary -- v1GCEPersistentDiskVolumeSourcePdName :: Text
      <*> arbitrary -- v1GCEPersistentDiskVolumeSourceReadOnly :: Maybe Bool
    
instance Arbitrary V1GitRepoVolumeSource where
  arbitrary =
    V1GitRepoVolumeSource
      <$> arbitrary -- v1GitRepoVolumeSourceDirectory :: Maybe Text
      <*> arbitrary -- v1GitRepoVolumeSourceRepository :: Text
      <*> arbitrary -- v1GitRepoVolumeSourceRevision :: Maybe Text
    
instance Arbitrary V1GlusterfsVolumeSource where
  arbitrary =
    V1GlusterfsVolumeSource
      <$> arbitrary -- v1GlusterfsVolumeSourceEndpoints :: Text
      <*> arbitrary -- v1GlusterfsVolumeSourcePath :: Text
      <*> arbitrary -- v1GlusterfsVolumeSourceReadOnly :: Maybe Bool
    
instance Arbitrary V1GroupVersionForDiscovery where
  arbitrary =
    V1GroupVersionForDiscovery
      <$> arbitrary -- v1GroupVersionForDiscoveryGroupVersion :: Text
      <*> arbitrary -- v1GroupVersionForDiscoveryVersion :: Text
    
instance Arbitrary V1HTTPGetAction where
  arbitrary =
    V1HTTPGetAction
      <$> arbitrary -- v1HTTPGetActionHost :: Maybe Text
      <*> arbitrary -- v1HTTPGetActionHttpHeaders :: Maybe [V1HTTPHeader]
      <*> arbitrary -- v1HTTPGetActionPath :: Maybe Text
      <*> arbitrary -- v1HTTPGetActionPort :: A.Value
      <*> arbitrary -- v1HTTPGetActionScheme :: Maybe Text
    
instance Arbitrary V1HTTPHeader where
  arbitrary =
    V1HTTPHeader
      <$> arbitrary -- v1HTTPHeaderName :: Text
      <*> arbitrary -- v1HTTPHeaderValue :: Text
    
instance Arbitrary V1Handler where
  arbitrary =
    V1Handler
      <$> arbitrary -- v1HandlerExec :: Maybe V1ExecAction
      <*> arbitrary -- v1HandlerHttpGet :: Maybe V1HTTPGetAction
      <*> arbitrary -- v1HandlerTcpSocket :: Maybe V1TCPSocketAction
    
instance Arbitrary V1HorizontalPodAutoscaler where
  arbitrary =
    V1HorizontalPodAutoscaler
      <$> arbitrary -- v1HorizontalPodAutoscalerApiVersion :: Maybe Text
      <*> arbitrary -- v1HorizontalPodAutoscalerKind :: Maybe Text
      <*> arbitrary -- v1HorizontalPodAutoscalerMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1HorizontalPodAutoscalerSpec :: Maybe V1HorizontalPodAutoscalerSpec
      <*> arbitrary -- v1HorizontalPodAutoscalerStatus :: Maybe V1HorizontalPodAutoscalerStatus
    
instance Arbitrary V1HorizontalPodAutoscalerList where
  arbitrary =
    V1HorizontalPodAutoscalerList
      <$> arbitrary -- v1HorizontalPodAutoscalerListApiVersion :: Maybe Text
      <*> arbitrary -- v1HorizontalPodAutoscalerListItems :: [V1HorizontalPodAutoscaler]
      <*> arbitrary -- v1HorizontalPodAutoscalerListKind :: Maybe Text
      <*> arbitrary -- v1HorizontalPodAutoscalerListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1HorizontalPodAutoscalerSpec where
  arbitrary =
    V1HorizontalPodAutoscalerSpec
      <$> arbitrary -- v1HorizontalPodAutoscalerSpecMaxReplicas :: Int
      <*> arbitrary -- v1HorizontalPodAutoscalerSpecMinReplicas :: Maybe Int
      <*> arbitrary -- v1HorizontalPodAutoscalerSpecScaleTargetRef :: V1CrossVersionObjectReference
      <*> arbitrary -- v1HorizontalPodAutoscalerSpecTargetCpuUtilizationPercentage :: Maybe Int
    
instance Arbitrary V1HorizontalPodAutoscalerStatus where
  arbitrary =
    V1HorizontalPodAutoscalerStatus
      <$> arbitrary -- v1HorizontalPodAutoscalerStatusCurrentCpuUtilizationPercentage :: Maybe Int
      <*> arbitrary -- v1HorizontalPodAutoscalerStatusCurrentReplicas :: Int
      <*> arbitrary -- v1HorizontalPodAutoscalerStatusDesiredReplicas :: Int
      <*> arbitrary -- v1HorizontalPodAutoscalerStatusLastScaleTime :: Maybe DateTime
      <*> arbitrary -- v1HorizontalPodAutoscalerStatusObservedGeneration :: Maybe Integer
    
instance Arbitrary V1HostAlias where
  arbitrary =
    V1HostAlias
      <$> arbitrary -- v1HostAliasHostnames :: Maybe [Text]
      <*> arbitrary -- v1HostAliasIp :: Maybe Text
    
instance Arbitrary V1HostPathVolumeSource where
  arbitrary =
    V1HostPathVolumeSource
      <$> arbitrary -- v1HostPathVolumeSourcePath :: Text
      <*> arbitrary -- v1HostPathVolumeSourceType :: Maybe Text
    
instance Arbitrary V1IPBlock where
  arbitrary =
    V1IPBlock
      <$> arbitrary -- v1IPBlockCidr :: Text
      <*> arbitrary -- v1IPBlockExcept :: Maybe [Text]
    
instance Arbitrary V1ISCSIPersistentVolumeSource where
  arbitrary =
    V1ISCSIPersistentVolumeSource
      <$> arbitrary -- v1ISCSIPersistentVolumeSourceChapAuthDiscovery :: Maybe Bool
      <*> arbitrary -- v1ISCSIPersistentVolumeSourceChapAuthSession :: Maybe Bool
      <*> arbitrary -- v1ISCSIPersistentVolumeSourceFsType :: Maybe Text
      <*> arbitrary -- v1ISCSIPersistentVolumeSourceInitiatorName :: Maybe Text
      <*> arbitrary -- v1ISCSIPersistentVolumeSourceIqn :: Text
      <*> arbitrary -- v1ISCSIPersistentVolumeSourceIscsiInterface :: Maybe Text
      <*> arbitrary -- v1ISCSIPersistentVolumeSourceLun :: Int
      <*> arbitrary -- v1ISCSIPersistentVolumeSourcePortals :: Maybe [Text]
      <*> arbitrary -- v1ISCSIPersistentVolumeSourceReadOnly :: Maybe Bool
      <*> arbitrary -- v1ISCSIPersistentVolumeSourceSecretRef :: Maybe V1SecretReference
      <*> arbitrary -- v1ISCSIPersistentVolumeSourceTargetPortal :: Text
    
instance Arbitrary V1ISCSIVolumeSource where
  arbitrary =
    V1ISCSIVolumeSource
      <$> arbitrary -- v1ISCSIVolumeSourceChapAuthDiscovery :: Maybe Bool
      <*> arbitrary -- v1ISCSIVolumeSourceChapAuthSession :: Maybe Bool
      <*> arbitrary -- v1ISCSIVolumeSourceFsType :: Maybe Text
      <*> arbitrary -- v1ISCSIVolumeSourceInitiatorName :: Maybe Text
      <*> arbitrary -- v1ISCSIVolumeSourceIqn :: Text
      <*> arbitrary -- v1ISCSIVolumeSourceIscsiInterface :: Maybe Text
      <*> arbitrary -- v1ISCSIVolumeSourceLun :: Int
      <*> arbitrary -- v1ISCSIVolumeSourcePortals :: Maybe [Text]
      <*> arbitrary -- v1ISCSIVolumeSourceReadOnly :: Maybe Bool
      <*> arbitrary -- v1ISCSIVolumeSourceSecretRef :: Maybe V1LocalObjectReference
      <*> arbitrary -- v1ISCSIVolumeSourceTargetPortal :: Text
    
instance Arbitrary V1Initializer where
  arbitrary =
    V1Initializer
      <$> arbitrary -- v1InitializerName :: Text
    
instance Arbitrary V1Initializers where
  arbitrary =
    V1Initializers
      <$> arbitrary -- v1InitializersPending :: [V1Initializer]
      <*> arbitrary -- v1InitializersResult :: Maybe V1Status
    
instance Arbitrary V1Job where
  arbitrary =
    V1Job
      <$> arbitrary -- v1JobApiVersion :: Maybe Text
      <*> arbitrary -- v1JobKind :: Maybe Text
      <*> arbitrary -- v1JobMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1JobSpec :: Maybe V1JobSpec
      <*> arbitrary -- v1JobStatus :: Maybe V1JobStatus
    
instance Arbitrary V1JobCondition where
  arbitrary =
    V1JobCondition
      <$> arbitrary -- v1JobConditionLastProbeTime :: Maybe DateTime
      <*> arbitrary -- v1JobConditionLastTransitionTime :: Maybe DateTime
      <*> arbitrary -- v1JobConditionMessage :: Maybe Text
      <*> arbitrary -- v1JobConditionReason :: Maybe Text
      <*> arbitrary -- v1JobConditionStatus :: Text
      <*> arbitrary -- v1JobConditionType :: Text
    
instance Arbitrary V1JobList where
  arbitrary =
    V1JobList
      <$> arbitrary -- v1JobListApiVersion :: Maybe Text
      <*> arbitrary -- v1JobListItems :: [V1Job]
      <*> arbitrary -- v1JobListKind :: Maybe Text
      <*> arbitrary -- v1JobListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1JobSpec where
  arbitrary =
    V1JobSpec
      <$> arbitrary -- v1JobSpecActiveDeadlineSeconds :: Maybe Integer
      <*> arbitrary -- v1JobSpecBackoffLimit :: Maybe Int
      <*> arbitrary -- v1JobSpecCompletions :: Maybe Int
      <*> arbitrary -- v1JobSpecManualSelector :: Maybe Bool
      <*> arbitrary -- v1JobSpecParallelism :: Maybe Int
      <*> arbitrary -- v1JobSpecSelector :: Maybe V1LabelSelector
      <*> arbitrary -- v1JobSpecTemplate :: V1PodTemplateSpec
    
instance Arbitrary V1JobStatus where
  arbitrary =
    V1JobStatus
      <$> arbitrary -- v1JobStatusActive :: Maybe Int
      <*> arbitrary -- v1JobStatusCompletionTime :: Maybe DateTime
      <*> arbitrary -- v1JobStatusConditions :: Maybe [V1JobCondition]
      <*> arbitrary -- v1JobStatusFailed :: Maybe Int
      <*> arbitrary -- v1JobStatusStartTime :: Maybe DateTime
      <*> arbitrary -- v1JobStatusSucceeded :: Maybe Int
    
instance Arbitrary V1KeyToPath where
  arbitrary =
    V1KeyToPath
      <$> arbitrary -- v1KeyToPathKey :: Text
      <*> arbitrary -- v1KeyToPathMode :: Maybe Int
      <*> arbitrary -- v1KeyToPathPath :: Text
    
instance Arbitrary V1LabelSelector where
  arbitrary =
    V1LabelSelector
      <$> arbitrary -- v1LabelSelectorMatchExpressions :: Maybe [V1LabelSelectorRequirement]
      <*> arbitrary -- v1LabelSelectorMatchLabels :: Maybe (Map.Map String Text)
    
instance Arbitrary V1LabelSelectorRequirement where
  arbitrary =
    V1LabelSelectorRequirement
      <$> arbitrary -- v1LabelSelectorRequirementKey :: Text
      <*> arbitrary -- v1LabelSelectorRequirementOperator :: Text
      <*> arbitrary -- v1LabelSelectorRequirementValues :: Maybe [Text]
    
instance Arbitrary V1Lifecycle where
  arbitrary =
    V1Lifecycle
      <$> arbitrary -- v1LifecyclePostStart :: Maybe V1Handler
      <*> arbitrary -- v1LifecyclePreStop :: Maybe V1Handler
    
instance Arbitrary V1LimitRange where
  arbitrary =
    V1LimitRange
      <$> arbitrary -- v1LimitRangeApiVersion :: Maybe Text
      <*> arbitrary -- v1LimitRangeKind :: Maybe Text
      <*> arbitrary -- v1LimitRangeMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1LimitRangeSpec :: Maybe V1LimitRangeSpec
    
instance Arbitrary V1LimitRangeItem where
  arbitrary =
    V1LimitRangeItem
      <$> arbitrary -- v1LimitRangeItemDefault :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1LimitRangeItemDefaultRequest :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1LimitRangeItemMax :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1LimitRangeItemMaxLimitRequestRatio :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1LimitRangeItemMin :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1LimitRangeItemType :: Maybe Text
    
instance Arbitrary V1LimitRangeList where
  arbitrary =
    V1LimitRangeList
      <$> arbitrary -- v1LimitRangeListApiVersion :: Maybe Text
      <*> arbitrary -- v1LimitRangeListItems :: [V1LimitRange]
      <*> arbitrary -- v1LimitRangeListKind :: Maybe Text
      <*> arbitrary -- v1LimitRangeListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1LimitRangeSpec where
  arbitrary =
    V1LimitRangeSpec
      <$> arbitrary -- v1LimitRangeSpecLimits :: [V1LimitRangeItem]
    
instance Arbitrary V1ListMeta where
  arbitrary =
    V1ListMeta
      <$> arbitrary -- v1ListMetaContinue :: Maybe Text
      <*> arbitrary -- v1ListMetaResourceVersion :: Maybe Text
      <*> arbitrary -- v1ListMetaSelfLink :: Maybe Text
    
instance Arbitrary V1LoadBalancerIngress where
  arbitrary =
    V1LoadBalancerIngress
      <$> arbitrary -- v1LoadBalancerIngressHostname :: Maybe Text
      <*> arbitrary -- v1LoadBalancerIngressIp :: Maybe Text
    
instance Arbitrary V1LoadBalancerStatus where
  arbitrary =
    V1LoadBalancerStatus
      <$> arbitrary -- v1LoadBalancerStatusIngress :: Maybe [V1LoadBalancerIngress]
    
instance Arbitrary V1LocalObjectReference where
  arbitrary =
    V1LocalObjectReference
      <$> arbitrary -- v1LocalObjectReferenceName :: Maybe Text
    
instance Arbitrary V1LocalSubjectAccessReview where
  arbitrary =
    V1LocalSubjectAccessReview
      <$> arbitrary -- v1LocalSubjectAccessReviewApiVersion :: Maybe Text
      <*> arbitrary -- v1LocalSubjectAccessReviewKind :: Maybe Text
      <*> arbitrary -- v1LocalSubjectAccessReviewMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1LocalSubjectAccessReviewSpec :: V1SubjectAccessReviewSpec
      <*> arbitrary -- v1LocalSubjectAccessReviewStatus :: Maybe V1SubjectAccessReviewStatus
    
instance Arbitrary V1LocalVolumeSource where
  arbitrary =
    V1LocalVolumeSource
      <$> arbitrary -- v1LocalVolumeSourcePath :: Text
    
instance Arbitrary V1NFSVolumeSource where
  arbitrary =
    V1NFSVolumeSource
      <$> arbitrary -- v1NFSVolumeSourcePath :: Text
      <*> arbitrary -- v1NFSVolumeSourceReadOnly :: Maybe Bool
      <*> arbitrary -- v1NFSVolumeSourceServer :: Text
    
instance Arbitrary V1Namespace where
  arbitrary =
    V1Namespace
      <$> arbitrary -- v1NamespaceApiVersion :: Maybe Text
      <*> arbitrary -- v1NamespaceKind :: Maybe Text
      <*> arbitrary -- v1NamespaceMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1NamespaceSpec :: Maybe V1NamespaceSpec
      <*> arbitrary -- v1NamespaceStatus :: Maybe V1NamespaceStatus
    
instance Arbitrary V1NamespaceList where
  arbitrary =
    V1NamespaceList
      <$> arbitrary -- v1NamespaceListApiVersion :: Maybe Text
      <*> arbitrary -- v1NamespaceListItems :: [V1Namespace]
      <*> arbitrary -- v1NamespaceListKind :: Maybe Text
      <*> arbitrary -- v1NamespaceListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1NamespaceSpec where
  arbitrary =
    V1NamespaceSpec
      <$> arbitrary -- v1NamespaceSpecFinalizers :: Maybe [Text]
    
instance Arbitrary V1NamespaceStatus where
  arbitrary =
    V1NamespaceStatus
      <$> arbitrary -- v1NamespaceStatusPhase :: Maybe Text
    
instance Arbitrary V1NetworkPolicy where
  arbitrary =
    V1NetworkPolicy
      <$> arbitrary -- v1NetworkPolicyApiVersion :: Maybe Text
      <*> arbitrary -- v1NetworkPolicyKind :: Maybe Text
      <*> arbitrary -- v1NetworkPolicyMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1NetworkPolicySpec :: Maybe V1NetworkPolicySpec
    
instance Arbitrary V1NetworkPolicyEgressRule where
  arbitrary =
    V1NetworkPolicyEgressRule
      <$> arbitrary -- v1NetworkPolicyEgressRulePorts :: Maybe [V1NetworkPolicyPort]
      <*> arbitrary -- v1NetworkPolicyEgressRuleTo :: Maybe [V1NetworkPolicyPeer]
    
instance Arbitrary V1NetworkPolicyIngressRule where
  arbitrary =
    V1NetworkPolicyIngressRule
      <$> arbitrary -- v1NetworkPolicyIngressRuleFrom :: Maybe [V1NetworkPolicyPeer]
      <*> arbitrary -- v1NetworkPolicyIngressRulePorts :: Maybe [V1NetworkPolicyPort]
    
instance Arbitrary V1NetworkPolicyList where
  arbitrary =
    V1NetworkPolicyList
      <$> arbitrary -- v1NetworkPolicyListApiVersion :: Maybe Text
      <*> arbitrary -- v1NetworkPolicyListItems :: [V1NetworkPolicy]
      <*> arbitrary -- v1NetworkPolicyListKind :: Maybe Text
      <*> arbitrary -- v1NetworkPolicyListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1NetworkPolicyPeer where
  arbitrary =
    V1NetworkPolicyPeer
      <$> arbitrary -- v1NetworkPolicyPeerIpBlock :: Maybe V1IPBlock
      <*> arbitrary -- v1NetworkPolicyPeerNamespaceSelector :: Maybe V1LabelSelector
      <*> arbitrary -- v1NetworkPolicyPeerPodSelector :: Maybe V1LabelSelector
    
instance Arbitrary V1NetworkPolicyPort where
  arbitrary =
    V1NetworkPolicyPort
      <$> arbitrary -- v1NetworkPolicyPortPort :: Maybe A.Value
      <*> arbitrary -- v1NetworkPolicyPortProtocol :: Maybe Text
    
instance Arbitrary V1NetworkPolicySpec where
  arbitrary =
    V1NetworkPolicySpec
      <$> arbitrary -- v1NetworkPolicySpecEgress :: Maybe [V1NetworkPolicyEgressRule]
      <*> arbitrary -- v1NetworkPolicySpecIngress :: Maybe [V1NetworkPolicyIngressRule]
      <*> arbitrary -- v1NetworkPolicySpecPodSelector :: V1LabelSelector
      <*> arbitrary -- v1NetworkPolicySpecPolicyTypes :: Maybe [Text]
    
instance Arbitrary V1Node where
  arbitrary =
    V1Node
      <$> arbitrary -- v1NodeApiVersion :: Maybe Text
      <*> arbitrary -- v1NodeKind :: Maybe Text
      <*> arbitrary -- v1NodeMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1NodeSpec :: Maybe V1NodeSpec
      <*> arbitrary -- v1NodeStatus :: Maybe V1NodeStatus
    
instance Arbitrary V1NodeAddress where
  arbitrary =
    V1NodeAddress
      <$> arbitrary -- v1NodeAddressAddress :: Text
      <*> arbitrary -- v1NodeAddressType :: Text
    
instance Arbitrary V1NodeAffinity where
  arbitrary =
    V1NodeAffinity
      <$> arbitrary -- v1NodeAffinityPreferredDuringSchedulingIgnoredDuringExecution :: Maybe [V1PreferredSchedulingTerm]
      <*> arbitrary -- v1NodeAffinityRequiredDuringSchedulingIgnoredDuringExecution :: Maybe V1NodeSelector
    
instance Arbitrary V1NodeCondition where
  arbitrary =
    V1NodeCondition
      <$> arbitrary -- v1NodeConditionLastHeartbeatTime :: Maybe DateTime
      <*> arbitrary -- v1NodeConditionLastTransitionTime :: Maybe DateTime
      <*> arbitrary -- v1NodeConditionMessage :: Maybe Text
      <*> arbitrary -- v1NodeConditionReason :: Maybe Text
      <*> arbitrary -- v1NodeConditionStatus :: Text
      <*> arbitrary -- v1NodeConditionType :: Text
    
instance Arbitrary V1NodeConfigSource where
  arbitrary =
    V1NodeConfigSource
      <$> arbitrary -- v1NodeConfigSourceApiVersion :: Maybe Text
      <*> arbitrary -- v1NodeConfigSourceConfigMapRef :: Maybe V1ObjectReference
      <*> arbitrary -- v1NodeConfigSourceKind :: Maybe Text
    
instance Arbitrary V1NodeDaemonEndpoints where
  arbitrary =
    V1NodeDaemonEndpoints
      <$> arbitrary -- v1NodeDaemonEndpointsKubeletEndpoint :: Maybe V1DaemonEndpoint
    
instance Arbitrary V1NodeList where
  arbitrary =
    V1NodeList
      <$> arbitrary -- v1NodeListApiVersion :: Maybe Text
      <*> arbitrary -- v1NodeListItems :: [V1Node]
      <*> arbitrary -- v1NodeListKind :: Maybe Text
      <*> arbitrary -- v1NodeListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1NodeSelector where
  arbitrary =
    V1NodeSelector
      <$> arbitrary -- v1NodeSelectorNodeSelectorTerms :: [V1NodeSelectorTerm]
    
instance Arbitrary V1NodeSelectorRequirement where
  arbitrary =
    V1NodeSelectorRequirement
      <$> arbitrary -- v1NodeSelectorRequirementKey :: Text
      <*> arbitrary -- v1NodeSelectorRequirementOperator :: Text
      <*> arbitrary -- v1NodeSelectorRequirementValues :: Maybe [Text]
    
instance Arbitrary V1NodeSelectorTerm where
  arbitrary =
    V1NodeSelectorTerm
      <$> arbitrary -- v1NodeSelectorTermMatchExpressions :: [V1NodeSelectorRequirement]
    
instance Arbitrary V1NodeSpec where
  arbitrary =
    V1NodeSpec
      <$> arbitrary -- v1NodeSpecConfigSource :: Maybe V1NodeConfigSource
      <*> arbitrary -- v1NodeSpecExternalId :: Maybe Text
      <*> arbitrary -- v1NodeSpecPodCidr :: Maybe Text
      <*> arbitrary -- v1NodeSpecProviderId :: Maybe Text
      <*> arbitrary -- v1NodeSpecTaints :: Maybe [V1Taint]
      <*> arbitrary -- v1NodeSpecUnschedulable :: Maybe Bool
    
instance Arbitrary V1NodeStatus where
  arbitrary =
    V1NodeStatus
      <$> arbitrary -- v1NodeStatusAddresses :: Maybe [V1NodeAddress]
      <*> arbitrary -- v1NodeStatusAllocatable :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1NodeStatusCapacity :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1NodeStatusConditions :: Maybe [V1NodeCondition]
      <*> arbitrary -- v1NodeStatusDaemonEndpoints :: Maybe V1NodeDaemonEndpoints
      <*> arbitrary -- v1NodeStatusImages :: Maybe [V1ContainerImage]
      <*> arbitrary -- v1NodeStatusNodeInfo :: Maybe V1NodeSystemInfo
      <*> arbitrary -- v1NodeStatusPhase :: Maybe Text
      <*> arbitrary -- v1NodeStatusVolumesAttached :: Maybe [V1AttachedVolume]
      <*> arbitrary -- v1NodeStatusVolumesInUse :: Maybe [Text]
    
instance Arbitrary V1NodeSystemInfo where
  arbitrary =
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
  arbitrary =
    V1NonResourceAttributes
      <$> arbitrary -- v1NonResourceAttributesPath :: Maybe Text
      <*> arbitrary -- v1NonResourceAttributesVerb :: Maybe Text
    
instance Arbitrary V1NonResourceRule where
  arbitrary =
    V1NonResourceRule
      <$> arbitrary -- v1NonResourceRuleNonResourceUrLs :: Maybe [Text]
      <*> arbitrary -- v1NonResourceRuleVerbs :: [Text]
    
instance Arbitrary V1ObjectFieldSelector where
  arbitrary =
    V1ObjectFieldSelector
      <$> arbitrary -- v1ObjectFieldSelectorApiVersion :: Maybe Text
      <*> arbitrary -- v1ObjectFieldSelectorFieldPath :: Text
    
instance Arbitrary V1ObjectMeta where
  arbitrary =
    V1ObjectMeta
      <$> arbitrary -- v1ObjectMetaAnnotations :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1ObjectMetaClusterName :: Maybe Text
      <*> arbitrary -- v1ObjectMetaCreationTimestamp :: Maybe DateTime
      <*> arbitrary -- v1ObjectMetaDeletionGracePeriodSeconds :: Maybe Integer
      <*> arbitrary -- v1ObjectMetaDeletionTimestamp :: Maybe DateTime
      <*> arbitrary -- v1ObjectMetaFinalizers :: Maybe [Text]
      <*> arbitrary -- v1ObjectMetaGenerateName :: Maybe Text
      <*> arbitrary -- v1ObjectMetaGeneration :: Maybe Integer
      <*> arbitrary -- v1ObjectMetaInitializers :: Maybe V1Initializers
      <*> arbitrary -- v1ObjectMetaLabels :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1ObjectMetaName :: Maybe Text
      <*> arbitrary -- v1ObjectMetaNamespace :: Maybe Text
      <*> arbitrary -- v1ObjectMetaOwnerReferences :: Maybe [V1OwnerReference]
      <*> arbitrary -- v1ObjectMetaResourceVersion :: Maybe Text
      <*> arbitrary -- v1ObjectMetaSelfLink :: Maybe Text
      <*> arbitrary -- v1ObjectMetaUid :: Maybe Text
    
instance Arbitrary V1ObjectReference where
  arbitrary =
    V1ObjectReference
      <$> arbitrary -- v1ObjectReferenceApiVersion :: Maybe Text
      <*> arbitrary -- v1ObjectReferenceFieldPath :: Maybe Text
      <*> arbitrary -- v1ObjectReferenceKind :: Maybe Text
      <*> arbitrary -- v1ObjectReferenceName :: Maybe Text
      <*> arbitrary -- v1ObjectReferenceNamespace :: Maybe Text
      <*> arbitrary -- v1ObjectReferenceResourceVersion :: Maybe Text
      <*> arbitrary -- v1ObjectReferenceUid :: Maybe Text
    
instance Arbitrary V1OwnerReference where
  arbitrary =
    V1OwnerReference
      <$> arbitrary -- v1OwnerReferenceApiVersion :: Text
      <*> arbitrary -- v1OwnerReferenceBlockOwnerDeletion :: Maybe Bool
      <*> arbitrary -- v1OwnerReferenceController :: Maybe Bool
      <*> arbitrary -- v1OwnerReferenceKind :: Text
      <*> arbitrary -- v1OwnerReferenceName :: Text
      <*> arbitrary -- v1OwnerReferenceUid :: Text
    
instance Arbitrary V1PersistentVolume where
  arbitrary =
    V1PersistentVolume
      <$> arbitrary -- v1PersistentVolumeApiVersion :: Maybe Text
      <*> arbitrary -- v1PersistentVolumeKind :: Maybe Text
      <*> arbitrary -- v1PersistentVolumeMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1PersistentVolumeSpec :: Maybe V1PersistentVolumeSpec
      <*> arbitrary -- v1PersistentVolumeStatus :: Maybe V1PersistentVolumeStatus
    
instance Arbitrary V1PersistentVolumeClaim where
  arbitrary =
    V1PersistentVolumeClaim
      <$> arbitrary -- v1PersistentVolumeClaimApiVersion :: Maybe Text
      <*> arbitrary -- v1PersistentVolumeClaimKind :: Maybe Text
      <*> arbitrary -- v1PersistentVolumeClaimMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1PersistentVolumeClaimSpec :: Maybe V1PersistentVolumeClaimSpec
      <*> arbitrary -- v1PersistentVolumeClaimStatus :: Maybe V1PersistentVolumeClaimStatus
    
instance Arbitrary V1PersistentVolumeClaimCondition where
  arbitrary =
    V1PersistentVolumeClaimCondition
      <$> arbitrary -- v1PersistentVolumeClaimConditionLastProbeTime :: Maybe DateTime
      <*> arbitrary -- v1PersistentVolumeClaimConditionLastTransitionTime :: Maybe DateTime
      <*> arbitrary -- v1PersistentVolumeClaimConditionMessage :: Maybe Text
      <*> arbitrary -- v1PersistentVolumeClaimConditionReason :: Maybe Text
      <*> arbitrary -- v1PersistentVolumeClaimConditionStatus :: Text
      <*> arbitrary -- v1PersistentVolumeClaimConditionType :: Text
    
instance Arbitrary V1PersistentVolumeClaimList where
  arbitrary =
    V1PersistentVolumeClaimList
      <$> arbitrary -- v1PersistentVolumeClaimListApiVersion :: Maybe Text
      <*> arbitrary -- v1PersistentVolumeClaimListItems :: [V1PersistentVolumeClaim]
      <*> arbitrary -- v1PersistentVolumeClaimListKind :: Maybe Text
      <*> arbitrary -- v1PersistentVolumeClaimListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1PersistentVolumeClaimSpec where
  arbitrary =
    V1PersistentVolumeClaimSpec
      <$> arbitrary -- v1PersistentVolumeClaimSpecAccessModes :: Maybe [Text]
      <*> arbitrary -- v1PersistentVolumeClaimSpecResources :: Maybe V1ResourceRequirements
      <*> arbitrary -- v1PersistentVolumeClaimSpecSelector :: Maybe V1LabelSelector
      <*> arbitrary -- v1PersistentVolumeClaimSpecStorageClassName :: Maybe Text
      <*> arbitrary -- v1PersistentVolumeClaimSpecVolumeMode :: Maybe Text
      <*> arbitrary -- v1PersistentVolumeClaimSpecVolumeName :: Maybe Text
    
instance Arbitrary V1PersistentVolumeClaimStatus where
  arbitrary =
    V1PersistentVolumeClaimStatus
      <$> arbitrary -- v1PersistentVolumeClaimStatusAccessModes :: Maybe [Text]
      <*> arbitrary -- v1PersistentVolumeClaimStatusCapacity :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1PersistentVolumeClaimStatusConditions :: Maybe [V1PersistentVolumeClaimCondition]
      <*> arbitrary -- v1PersistentVolumeClaimStatusPhase :: Maybe Text
    
instance Arbitrary V1PersistentVolumeClaimVolumeSource where
  arbitrary =
    V1PersistentVolumeClaimVolumeSource
      <$> arbitrary -- v1PersistentVolumeClaimVolumeSourceClaimName :: Text
      <*> arbitrary -- v1PersistentVolumeClaimVolumeSourceReadOnly :: Maybe Bool
    
instance Arbitrary V1PersistentVolumeList where
  arbitrary =
    V1PersistentVolumeList
      <$> arbitrary -- v1PersistentVolumeListApiVersion :: Maybe Text
      <*> arbitrary -- v1PersistentVolumeListItems :: [V1PersistentVolume]
      <*> arbitrary -- v1PersistentVolumeListKind :: Maybe Text
      <*> arbitrary -- v1PersistentVolumeListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1PersistentVolumeSpec where
  arbitrary =
    V1PersistentVolumeSpec
      <$> arbitrary -- v1PersistentVolumeSpecAccessModes :: Maybe [Text]
      <*> arbitrary -- v1PersistentVolumeSpecAwsElasticBlockStore :: Maybe V1AWSElasticBlockStoreVolumeSource
      <*> arbitrary -- v1PersistentVolumeSpecAzureDisk :: Maybe V1AzureDiskVolumeSource
      <*> arbitrary -- v1PersistentVolumeSpecAzureFile :: Maybe V1AzureFilePersistentVolumeSource
      <*> arbitrary -- v1PersistentVolumeSpecCapacity :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1PersistentVolumeSpecCephfs :: Maybe V1CephFSPersistentVolumeSource
      <*> arbitrary -- v1PersistentVolumeSpecCinder :: Maybe V1CinderVolumeSource
      <*> arbitrary -- v1PersistentVolumeSpecClaimRef :: Maybe V1ObjectReference
      <*> arbitrary -- v1PersistentVolumeSpecCsi :: Maybe V1CSIPersistentVolumeSource
      <*> arbitrary -- v1PersistentVolumeSpecFc :: Maybe V1FCVolumeSource
      <*> arbitrary -- v1PersistentVolumeSpecFlexVolume :: Maybe V1FlexVolumeSource
      <*> arbitrary -- v1PersistentVolumeSpecFlocker :: Maybe V1FlockerVolumeSource
      <*> arbitrary -- v1PersistentVolumeSpecGcePersistentDisk :: Maybe V1GCEPersistentDiskVolumeSource
      <*> arbitrary -- v1PersistentVolumeSpecGlusterfs :: Maybe V1GlusterfsVolumeSource
      <*> arbitrary -- v1PersistentVolumeSpecHostPath :: Maybe V1HostPathVolumeSource
      <*> arbitrary -- v1PersistentVolumeSpecIscsi :: Maybe V1ISCSIPersistentVolumeSource
      <*> arbitrary -- v1PersistentVolumeSpecLocal :: Maybe V1LocalVolumeSource
      <*> arbitrary -- v1PersistentVolumeSpecMountOptions :: Maybe [Text]
      <*> arbitrary -- v1PersistentVolumeSpecNfs :: Maybe V1NFSVolumeSource
      <*> arbitrary -- v1PersistentVolumeSpecPersistentVolumeReclaimPolicy :: Maybe Text
      <*> arbitrary -- v1PersistentVolumeSpecPhotonPersistentDisk :: Maybe V1PhotonPersistentDiskVolumeSource
      <*> arbitrary -- v1PersistentVolumeSpecPortworxVolume :: Maybe V1PortworxVolumeSource
      <*> arbitrary -- v1PersistentVolumeSpecQuobyte :: Maybe V1QuobyteVolumeSource
      <*> arbitrary -- v1PersistentVolumeSpecRbd :: Maybe V1RBDPersistentVolumeSource
      <*> arbitrary -- v1PersistentVolumeSpecScaleIo :: Maybe V1ScaleIOPersistentVolumeSource
      <*> arbitrary -- v1PersistentVolumeSpecStorageClassName :: Maybe Text
      <*> arbitrary -- v1PersistentVolumeSpecStorageos :: Maybe V1StorageOSPersistentVolumeSource
      <*> arbitrary -- v1PersistentVolumeSpecVolumeMode :: Maybe Text
      <*> arbitrary -- v1PersistentVolumeSpecVsphereVolume :: Maybe V1VsphereVirtualDiskVolumeSource
    
instance Arbitrary V1PersistentVolumeStatus where
  arbitrary =
    V1PersistentVolumeStatus
      <$> arbitrary -- v1PersistentVolumeStatusMessage :: Maybe Text
      <*> arbitrary -- v1PersistentVolumeStatusPhase :: Maybe Text
      <*> arbitrary -- v1PersistentVolumeStatusReason :: Maybe Text
    
instance Arbitrary V1PhotonPersistentDiskVolumeSource where
  arbitrary =
    V1PhotonPersistentDiskVolumeSource
      <$> arbitrary -- v1PhotonPersistentDiskVolumeSourceFsType :: Maybe Text
      <*> arbitrary -- v1PhotonPersistentDiskVolumeSourcePdId :: Text
    
instance Arbitrary V1Pod where
  arbitrary =
    V1Pod
      <$> arbitrary -- v1PodApiVersion :: Maybe Text
      <*> arbitrary -- v1PodKind :: Maybe Text
      <*> arbitrary -- v1PodMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1PodSpec :: Maybe V1PodSpec
      <*> arbitrary -- v1PodStatus :: Maybe V1PodStatus
    
instance Arbitrary V1PodAffinity where
  arbitrary =
    V1PodAffinity
      <$> arbitrary -- v1PodAffinityPreferredDuringSchedulingIgnoredDuringExecution :: Maybe [V1WeightedPodAffinityTerm]
      <*> arbitrary -- v1PodAffinityRequiredDuringSchedulingIgnoredDuringExecution :: Maybe [V1PodAffinityTerm]
    
instance Arbitrary V1PodAffinityTerm where
  arbitrary =
    V1PodAffinityTerm
      <$> arbitrary -- v1PodAffinityTermLabelSelector :: Maybe V1LabelSelector
      <*> arbitrary -- v1PodAffinityTermNamespaces :: Maybe [Text]
      <*> arbitrary -- v1PodAffinityTermTopologyKey :: Text
    
instance Arbitrary V1PodAntiAffinity where
  arbitrary =
    V1PodAntiAffinity
      <$> arbitrary -- v1PodAntiAffinityPreferredDuringSchedulingIgnoredDuringExecution :: Maybe [V1WeightedPodAffinityTerm]
      <*> arbitrary -- v1PodAntiAffinityRequiredDuringSchedulingIgnoredDuringExecution :: Maybe [V1PodAffinityTerm]
    
instance Arbitrary V1PodCondition where
  arbitrary =
    V1PodCondition
      <$> arbitrary -- v1PodConditionLastProbeTime :: Maybe DateTime
      <*> arbitrary -- v1PodConditionLastTransitionTime :: Maybe DateTime
      <*> arbitrary -- v1PodConditionMessage :: Maybe Text
      <*> arbitrary -- v1PodConditionReason :: Maybe Text
      <*> arbitrary -- v1PodConditionStatus :: Text
      <*> arbitrary -- v1PodConditionType :: Text
    
instance Arbitrary V1PodDNSConfig where
  arbitrary =
    V1PodDNSConfig
      <$> arbitrary -- v1PodDNSConfigNameservers :: Maybe [Text]
      <*> arbitrary -- v1PodDNSConfigOptions :: Maybe [V1PodDNSConfigOption]
      <*> arbitrary -- v1PodDNSConfigSearches :: Maybe [Text]
    
instance Arbitrary V1PodDNSConfigOption where
  arbitrary =
    V1PodDNSConfigOption
      <$> arbitrary -- v1PodDNSConfigOptionName :: Maybe Text
      <*> arbitrary -- v1PodDNSConfigOptionValue :: Maybe Text
    
instance Arbitrary V1PodList where
  arbitrary =
    V1PodList
      <$> arbitrary -- v1PodListApiVersion :: Maybe Text
      <*> arbitrary -- v1PodListItems :: [V1Pod]
      <*> arbitrary -- v1PodListKind :: Maybe Text
      <*> arbitrary -- v1PodListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1PodSecurityContext where
  arbitrary =
    V1PodSecurityContext
      <$> arbitrary -- v1PodSecurityContextFsGroup :: Maybe Integer
      <*> arbitrary -- v1PodSecurityContextRunAsNonRoot :: Maybe Bool
      <*> arbitrary -- v1PodSecurityContextRunAsUser :: Maybe Integer
      <*> arbitrary -- v1PodSecurityContextSeLinuxOptions :: Maybe V1SELinuxOptions
      <*> arbitrary -- v1PodSecurityContextSupplementalGroups :: Maybe [Integer]
    
instance Arbitrary V1PodSpec where
  arbitrary =
    V1PodSpec
      <$> arbitrary -- v1PodSpecActiveDeadlineSeconds :: Maybe Integer
      <*> arbitrary -- v1PodSpecAffinity :: Maybe V1Affinity
      <*> arbitrary -- v1PodSpecAutomountServiceAccountToken :: Maybe Bool
      <*> arbitrary -- v1PodSpecContainers :: [V1Container]
      <*> arbitrary -- v1PodSpecDnsConfig :: Maybe V1PodDNSConfig
      <*> arbitrary -- v1PodSpecDnsPolicy :: Maybe Text
      <*> arbitrary -- v1PodSpecHostAliases :: Maybe [V1HostAlias]
      <*> arbitrary -- v1PodSpecHostIpc :: Maybe Bool
      <*> arbitrary -- v1PodSpecHostNetwork :: Maybe Bool
      <*> arbitrary -- v1PodSpecHostPid :: Maybe Bool
      <*> arbitrary -- v1PodSpecHostname :: Maybe Text
      <*> arbitrary -- v1PodSpecImagePullSecrets :: Maybe [V1LocalObjectReference]
      <*> arbitrary -- v1PodSpecInitContainers :: Maybe [V1Container]
      <*> arbitrary -- v1PodSpecNodeName :: Maybe Text
      <*> arbitrary -- v1PodSpecNodeSelector :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1PodSpecPriority :: Maybe Int
      <*> arbitrary -- v1PodSpecPriorityClassName :: Maybe Text
      <*> arbitrary -- v1PodSpecRestartPolicy :: Maybe Text
      <*> arbitrary -- v1PodSpecSchedulerName :: Maybe Text
      <*> arbitrary -- v1PodSpecSecurityContext :: Maybe V1PodSecurityContext
      <*> arbitrary -- v1PodSpecServiceAccount :: Maybe Text
      <*> arbitrary -- v1PodSpecServiceAccountName :: Maybe Text
      <*> arbitrary -- v1PodSpecSubdomain :: Maybe Text
      <*> arbitrary -- v1PodSpecTerminationGracePeriodSeconds :: Maybe Integer
      <*> arbitrary -- v1PodSpecTolerations :: Maybe [V1Toleration]
      <*> arbitrary -- v1PodSpecVolumes :: Maybe [V1Volume]
    
instance Arbitrary V1PodStatus where
  arbitrary =
    V1PodStatus
      <$> arbitrary -- v1PodStatusConditions :: Maybe [V1PodCondition]
      <*> arbitrary -- v1PodStatusContainerStatuses :: Maybe [V1ContainerStatus]
      <*> arbitrary -- v1PodStatusHostIp :: Maybe Text
      <*> arbitrary -- v1PodStatusInitContainerStatuses :: Maybe [V1ContainerStatus]
      <*> arbitrary -- v1PodStatusMessage :: Maybe Text
      <*> arbitrary -- v1PodStatusPhase :: Maybe Text
      <*> arbitrary -- v1PodStatusPodIp :: Maybe Text
      <*> arbitrary -- v1PodStatusQosClass :: Maybe Text
      <*> arbitrary -- v1PodStatusReason :: Maybe Text
      <*> arbitrary -- v1PodStatusStartTime :: Maybe DateTime
    
instance Arbitrary V1PodTemplate where
  arbitrary =
    V1PodTemplate
      <$> arbitrary -- v1PodTemplateApiVersion :: Maybe Text
      <*> arbitrary -- v1PodTemplateKind :: Maybe Text
      <*> arbitrary -- v1PodTemplateMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1PodTemplateTemplate :: Maybe V1PodTemplateSpec
    
instance Arbitrary V1PodTemplateList where
  arbitrary =
    V1PodTemplateList
      <$> arbitrary -- v1PodTemplateListApiVersion :: Maybe Text
      <*> arbitrary -- v1PodTemplateListItems :: [V1PodTemplate]
      <*> arbitrary -- v1PodTemplateListKind :: Maybe Text
      <*> arbitrary -- v1PodTemplateListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1PodTemplateSpec where
  arbitrary =
    V1PodTemplateSpec
      <$> arbitrary -- v1PodTemplateSpecMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1PodTemplateSpecSpec :: Maybe V1PodSpec
    
instance Arbitrary V1PolicyRule where
  arbitrary =
    V1PolicyRule
      <$> arbitrary -- v1PolicyRuleApiGroups :: Maybe [Text]
      <*> arbitrary -- v1PolicyRuleNonResourceUrLs :: Maybe [Text]
      <*> arbitrary -- v1PolicyRuleResourceNames :: Maybe [Text]
      <*> arbitrary -- v1PolicyRuleResources :: Maybe [Text]
      <*> arbitrary -- v1PolicyRuleVerbs :: [Text]
    
instance Arbitrary V1PortworxVolumeSource where
  arbitrary =
    V1PortworxVolumeSource
      <$> arbitrary -- v1PortworxVolumeSourceFsType :: Maybe Text
      <*> arbitrary -- v1PortworxVolumeSourceReadOnly :: Maybe Bool
      <*> arbitrary -- v1PortworxVolumeSourceVolumeId :: Text
    
instance Arbitrary V1Preconditions where
  arbitrary =
    V1Preconditions
      <$> arbitrary -- v1PreconditionsUid :: Maybe Text
    
instance Arbitrary V1PreferredSchedulingTerm where
  arbitrary =
    V1PreferredSchedulingTerm
      <$> arbitrary -- v1PreferredSchedulingTermPreference :: V1NodeSelectorTerm
      <*> arbitrary -- v1PreferredSchedulingTermWeight :: Int
    
instance Arbitrary V1Probe where
  arbitrary =
    V1Probe
      <$> arbitrary -- v1ProbeExec :: Maybe V1ExecAction
      <*> arbitrary -- v1ProbeFailureThreshold :: Maybe Int
      <*> arbitrary -- v1ProbeHttpGet :: Maybe V1HTTPGetAction
      <*> arbitrary -- v1ProbeInitialDelaySeconds :: Maybe Int
      <*> arbitrary -- v1ProbePeriodSeconds :: Maybe Int
      <*> arbitrary -- v1ProbeSuccessThreshold :: Maybe Int
      <*> arbitrary -- v1ProbeTcpSocket :: Maybe V1TCPSocketAction
      <*> arbitrary -- v1ProbeTimeoutSeconds :: Maybe Int
    
instance Arbitrary V1ProjectedVolumeSource where
  arbitrary =
    V1ProjectedVolumeSource
      <$> arbitrary -- v1ProjectedVolumeSourceDefaultMode :: Maybe Int
      <*> arbitrary -- v1ProjectedVolumeSourceSources :: [V1VolumeProjection]
    
instance Arbitrary V1QuobyteVolumeSource where
  arbitrary =
    V1QuobyteVolumeSource
      <$> arbitrary -- v1QuobyteVolumeSourceGroup :: Maybe Text
      <*> arbitrary -- v1QuobyteVolumeSourceReadOnly :: Maybe Bool
      <*> arbitrary -- v1QuobyteVolumeSourceRegistry :: Text
      <*> arbitrary -- v1QuobyteVolumeSourceUser :: Maybe Text
      <*> arbitrary -- v1QuobyteVolumeSourceVolume :: Text
    
instance Arbitrary V1RBDPersistentVolumeSource where
  arbitrary =
    V1RBDPersistentVolumeSource
      <$> arbitrary -- v1RBDPersistentVolumeSourceFsType :: Maybe Text
      <*> arbitrary -- v1RBDPersistentVolumeSourceImage :: Text
      <*> arbitrary -- v1RBDPersistentVolumeSourceKeyring :: Maybe Text
      <*> arbitrary -- v1RBDPersistentVolumeSourceMonitors :: [Text]
      <*> arbitrary -- v1RBDPersistentVolumeSourcePool :: Maybe Text
      <*> arbitrary -- v1RBDPersistentVolumeSourceReadOnly :: Maybe Bool
      <*> arbitrary -- v1RBDPersistentVolumeSourceSecretRef :: Maybe V1SecretReference
      <*> arbitrary -- v1RBDPersistentVolumeSourceUser :: Maybe Text
    
instance Arbitrary V1RBDVolumeSource where
  arbitrary =
    V1RBDVolumeSource
      <$> arbitrary -- v1RBDVolumeSourceFsType :: Maybe Text
      <*> arbitrary -- v1RBDVolumeSourceImage :: Text
      <*> arbitrary -- v1RBDVolumeSourceKeyring :: Maybe Text
      <*> arbitrary -- v1RBDVolumeSourceMonitors :: [Text]
      <*> arbitrary -- v1RBDVolumeSourcePool :: Maybe Text
      <*> arbitrary -- v1RBDVolumeSourceReadOnly :: Maybe Bool
      <*> arbitrary -- v1RBDVolumeSourceSecretRef :: Maybe V1LocalObjectReference
      <*> arbitrary -- v1RBDVolumeSourceUser :: Maybe Text
    
instance Arbitrary V1ReplicaSet where
  arbitrary =
    V1ReplicaSet
      <$> arbitrary -- v1ReplicaSetApiVersion :: Maybe Text
      <*> arbitrary -- v1ReplicaSetKind :: Maybe Text
      <*> arbitrary -- v1ReplicaSetMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1ReplicaSetSpec :: Maybe V1ReplicaSetSpec
      <*> arbitrary -- v1ReplicaSetStatus :: Maybe V1ReplicaSetStatus
    
instance Arbitrary V1ReplicaSetCondition where
  arbitrary =
    V1ReplicaSetCondition
      <$> arbitrary -- v1ReplicaSetConditionLastTransitionTime :: Maybe DateTime
      <*> arbitrary -- v1ReplicaSetConditionMessage :: Maybe Text
      <*> arbitrary -- v1ReplicaSetConditionReason :: Maybe Text
      <*> arbitrary -- v1ReplicaSetConditionStatus :: Text
      <*> arbitrary -- v1ReplicaSetConditionType :: Text
    
instance Arbitrary V1ReplicaSetList where
  arbitrary =
    V1ReplicaSetList
      <$> arbitrary -- v1ReplicaSetListApiVersion :: Maybe Text
      <*> arbitrary -- v1ReplicaSetListItems :: [V1ReplicaSet]
      <*> arbitrary -- v1ReplicaSetListKind :: Maybe Text
      <*> arbitrary -- v1ReplicaSetListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1ReplicaSetSpec where
  arbitrary =
    V1ReplicaSetSpec
      <$> arbitrary -- v1ReplicaSetSpecMinReadySeconds :: Maybe Int
      <*> arbitrary -- v1ReplicaSetSpecReplicas :: Maybe Int
      <*> arbitrary -- v1ReplicaSetSpecSelector :: V1LabelSelector
      <*> arbitrary -- v1ReplicaSetSpecTemplate :: Maybe V1PodTemplateSpec
    
instance Arbitrary V1ReplicaSetStatus where
  arbitrary =
    V1ReplicaSetStatus
      <$> arbitrary -- v1ReplicaSetStatusAvailableReplicas :: Maybe Int
      <*> arbitrary -- v1ReplicaSetStatusConditions :: Maybe [V1ReplicaSetCondition]
      <*> arbitrary -- v1ReplicaSetStatusFullyLabeledReplicas :: Maybe Int
      <*> arbitrary -- v1ReplicaSetStatusObservedGeneration :: Maybe Integer
      <*> arbitrary -- v1ReplicaSetStatusReadyReplicas :: Maybe Int
      <*> arbitrary -- v1ReplicaSetStatusReplicas :: Int
    
instance Arbitrary V1ReplicationController where
  arbitrary =
    V1ReplicationController
      <$> arbitrary -- v1ReplicationControllerApiVersion :: Maybe Text
      <*> arbitrary -- v1ReplicationControllerKind :: Maybe Text
      <*> arbitrary -- v1ReplicationControllerMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1ReplicationControllerSpec :: Maybe V1ReplicationControllerSpec
      <*> arbitrary -- v1ReplicationControllerStatus :: Maybe V1ReplicationControllerStatus
    
instance Arbitrary V1ReplicationControllerCondition where
  arbitrary =
    V1ReplicationControllerCondition
      <$> arbitrary -- v1ReplicationControllerConditionLastTransitionTime :: Maybe DateTime
      <*> arbitrary -- v1ReplicationControllerConditionMessage :: Maybe Text
      <*> arbitrary -- v1ReplicationControllerConditionReason :: Maybe Text
      <*> arbitrary -- v1ReplicationControllerConditionStatus :: Text
      <*> arbitrary -- v1ReplicationControllerConditionType :: Text
    
instance Arbitrary V1ReplicationControllerList where
  arbitrary =
    V1ReplicationControllerList
      <$> arbitrary -- v1ReplicationControllerListApiVersion :: Maybe Text
      <*> arbitrary -- v1ReplicationControllerListItems :: [V1ReplicationController]
      <*> arbitrary -- v1ReplicationControllerListKind :: Maybe Text
      <*> arbitrary -- v1ReplicationControllerListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1ReplicationControllerSpec where
  arbitrary =
    V1ReplicationControllerSpec
      <$> arbitrary -- v1ReplicationControllerSpecMinReadySeconds :: Maybe Int
      <*> arbitrary -- v1ReplicationControllerSpecReplicas :: Maybe Int
      <*> arbitrary -- v1ReplicationControllerSpecSelector :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1ReplicationControllerSpecTemplate :: Maybe V1PodTemplateSpec
    
instance Arbitrary V1ReplicationControllerStatus where
  arbitrary =
    V1ReplicationControllerStatus
      <$> arbitrary -- v1ReplicationControllerStatusAvailableReplicas :: Maybe Int
      <*> arbitrary -- v1ReplicationControllerStatusConditions :: Maybe [V1ReplicationControllerCondition]
      <*> arbitrary -- v1ReplicationControllerStatusFullyLabeledReplicas :: Maybe Int
      <*> arbitrary -- v1ReplicationControllerStatusObservedGeneration :: Maybe Integer
      <*> arbitrary -- v1ReplicationControllerStatusReadyReplicas :: Maybe Int
      <*> arbitrary -- v1ReplicationControllerStatusReplicas :: Int
    
instance Arbitrary V1ResourceAttributes where
  arbitrary =
    V1ResourceAttributes
      <$> arbitrary -- v1ResourceAttributesGroup :: Maybe Text
      <*> arbitrary -- v1ResourceAttributesName :: Maybe Text
      <*> arbitrary -- v1ResourceAttributesNamespace :: Maybe Text
      <*> arbitrary -- v1ResourceAttributesResource :: Maybe Text
      <*> arbitrary -- v1ResourceAttributesSubresource :: Maybe Text
      <*> arbitrary -- v1ResourceAttributesVerb :: Maybe Text
      <*> arbitrary -- v1ResourceAttributesVersion :: Maybe Text
    
instance Arbitrary V1ResourceFieldSelector where
  arbitrary =
    V1ResourceFieldSelector
      <$> arbitrary -- v1ResourceFieldSelectorContainerName :: Maybe Text
      <*> arbitrary -- v1ResourceFieldSelectorDivisor :: Maybe Text
      <*> arbitrary -- v1ResourceFieldSelectorResource :: Text
    
instance Arbitrary V1ResourceQuota where
  arbitrary =
    V1ResourceQuota
      <$> arbitrary -- v1ResourceQuotaApiVersion :: Maybe Text
      <*> arbitrary -- v1ResourceQuotaKind :: Maybe Text
      <*> arbitrary -- v1ResourceQuotaMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1ResourceQuotaSpec :: Maybe V1ResourceQuotaSpec
      <*> arbitrary -- v1ResourceQuotaStatus :: Maybe V1ResourceQuotaStatus
    
instance Arbitrary V1ResourceQuotaList where
  arbitrary =
    V1ResourceQuotaList
      <$> arbitrary -- v1ResourceQuotaListApiVersion :: Maybe Text
      <*> arbitrary -- v1ResourceQuotaListItems :: [V1ResourceQuota]
      <*> arbitrary -- v1ResourceQuotaListKind :: Maybe Text
      <*> arbitrary -- v1ResourceQuotaListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1ResourceQuotaSpec where
  arbitrary =
    V1ResourceQuotaSpec
      <$> arbitrary -- v1ResourceQuotaSpecHard :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1ResourceQuotaSpecScopes :: Maybe [Text]
    
instance Arbitrary V1ResourceQuotaStatus where
  arbitrary =
    V1ResourceQuotaStatus
      <$> arbitrary -- v1ResourceQuotaStatusHard :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1ResourceQuotaStatusUsed :: Maybe (Map.Map String Text)
    
instance Arbitrary V1ResourceRequirements where
  arbitrary =
    V1ResourceRequirements
      <$> arbitrary -- v1ResourceRequirementsLimits :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1ResourceRequirementsRequests :: Maybe (Map.Map String Text)
    
instance Arbitrary V1ResourceRule where
  arbitrary =
    V1ResourceRule
      <$> arbitrary -- v1ResourceRuleApiGroups :: Maybe [Text]
      <*> arbitrary -- v1ResourceRuleResourceNames :: Maybe [Text]
      <*> arbitrary -- v1ResourceRuleResources :: Maybe [Text]
      <*> arbitrary -- v1ResourceRuleVerbs :: [Text]
    
instance Arbitrary V1Role where
  arbitrary =
    V1Role
      <$> arbitrary -- v1RoleApiVersion :: Maybe Text
      <*> arbitrary -- v1RoleKind :: Maybe Text
      <*> arbitrary -- v1RoleMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1RoleRules :: [V1PolicyRule]
    
instance Arbitrary V1RoleBinding where
  arbitrary =
    V1RoleBinding
      <$> arbitrary -- v1RoleBindingApiVersion :: Maybe Text
      <*> arbitrary -- v1RoleBindingKind :: Maybe Text
      <*> arbitrary -- v1RoleBindingMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1RoleBindingRoleRef :: V1RoleRef
      <*> arbitrary -- v1RoleBindingSubjects :: [V1Subject]
    
instance Arbitrary V1RoleBindingList where
  arbitrary =
    V1RoleBindingList
      <$> arbitrary -- v1RoleBindingListApiVersion :: Maybe Text
      <*> arbitrary -- v1RoleBindingListItems :: [V1RoleBinding]
      <*> arbitrary -- v1RoleBindingListKind :: Maybe Text
      <*> arbitrary -- v1RoleBindingListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1RoleList where
  arbitrary =
    V1RoleList
      <$> arbitrary -- v1RoleListApiVersion :: Maybe Text
      <*> arbitrary -- v1RoleListItems :: [V1Role]
      <*> arbitrary -- v1RoleListKind :: Maybe Text
      <*> arbitrary -- v1RoleListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1RoleRef where
  arbitrary =
    V1RoleRef
      <$> arbitrary -- v1RoleRefApiGroup :: Text
      <*> arbitrary -- v1RoleRefKind :: Text
      <*> arbitrary -- v1RoleRefName :: Text
    
instance Arbitrary V1RollingUpdateDaemonSet where
  arbitrary =
    V1RollingUpdateDaemonSet
      <$> arbitrary -- v1RollingUpdateDaemonSetMaxUnavailable :: Maybe A.Value
    
instance Arbitrary V1RollingUpdateDeployment where
  arbitrary =
    V1RollingUpdateDeployment
      <$> arbitrary -- v1RollingUpdateDeploymentMaxSurge :: Maybe A.Value
      <*> arbitrary -- v1RollingUpdateDeploymentMaxUnavailable :: Maybe A.Value
    
instance Arbitrary V1RollingUpdateStatefulSetStrategy where
  arbitrary =
    V1RollingUpdateStatefulSetStrategy
      <$> arbitrary -- v1RollingUpdateStatefulSetStrategyPartition :: Maybe Int
    
instance Arbitrary V1SELinuxOptions where
  arbitrary =
    V1SELinuxOptions
      <$> arbitrary -- v1SELinuxOptionsLevel :: Maybe Text
      <*> arbitrary -- v1SELinuxOptionsRole :: Maybe Text
      <*> arbitrary -- v1SELinuxOptionsType :: Maybe Text
      <*> arbitrary -- v1SELinuxOptionsUser :: Maybe Text
    
instance Arbitrary V1Scale where
  arbitrary =
    V1Scale
      <$> arbitrary -- v1ScaleApiVersion :: Maybe Text
      <*> arbitrary -- v1ScaleKind :: Maybe Text
      <*> arbitrary -- v1ScaleMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1ScaleSpec :: Maybe V1ScaleSpec
      <*> arbitrary -- v1ScaleStatus :: Maybe V1ScaleStatus
    
instance Arbitrary V1ScaleIOPersistentVolumeSource where
  arbitrary =
    V1ScaleIOPersistentVolumeSource
      <$> arbitrary -- v1ScaleIOPersistentVolumeSourceFsType :: Maybe Text
      <*> arbitrary -- v1ScaleIOPersistentVolumeSourceGateway :: Text
      <*> arbitrary -- v1ScaleIOPersistentVolumeSourceProtectionDomain :: Maybe Text
      <*> arbitrary -- v1ScaleIOPersistentVolumeSourceReadOnly :: Maybe Bool
      <*> arbitrary -- v1ScaleIOPersistentVolumeSourceSecretRef :: V1SecretReference
      <*> arbitrary -- v1ScaleIOPersistentVolumeSourceSslEnabled :: Maybe Bool
      <*> arbitrary -- v1ScaleIOPersistentVolumeSourceStorageMode :: Maybe Text
      <*> arbitrary -- v1ScaleIOPersistentVolumeSourceStoragePool :: Maybe Text
      <*> arbitrary -- v1ScaleIOPersistentVolumeSourceSystem :: Text
      <*> arbitrary -- v1ScaleIOPersistentVolumeSourceVolumeName :: Maybe Text
    
instance Arbitrary V1ScaleIOVolumeSource where
  arbitrary =
    V1ScaleIOVolumeSource
      <$> arbitrary -- v1ScaleIOVolumeSourceFsType :: Maybe Text
      <*> arbitrary -- v1ScaleIOVolumeSourceGateway :: Text
      <*> arbitrary -- v1ScaleIOVolumeSourceProtectionDomain :: Maybe Text
      <*> arbitrary -- v1ScaleIOVolumeSourceReadOnly :: Maybe Bool
      <*> arbitrary -- v1ScaleIOVolumeSourceSecretRef :: V1LocalObjectReference
      <*> arbitrary -- v1ScaleIOVolumeSourceSslEnabled :: Maybe Bool
      <*> arbitrary -- v1ScaleIOVolumeSourceStorageMode :: Maybe Text
      <*> arbitrary -- v1ScaleIOVolumeSourceStoragePool :: Maybe Text
      <*> arbitrary -- v1ScaleIOVolumeSourceSystem :: Text
      <*> arbitrary -- v1ScaleIOVolumeSourceVolumeName :: Maybe Text
    
instance Arbitrary V1ScaleSpec where
  arbitrary =
    V1ScaleSpec
      <$> arbitrary -- v1ScaleSpecReplicas :: Maybe Int
    
instance Arbitrary V1ScaleStatus where
  arbitrary =
    V1ScaleStatus
      <$> arbitrary -- v1ScaleStatusReplicas :: Int
      <*> arbitrary -- v1ScaleStatusSelector :: Maybe Text
    
instance Arbitrary V1Secret where
  arbitrary =
    V1Secret
      <$> arbitrary -- v1SecretApiVersion :: Maybe Text
      <*> arbitrary -- v1SecretData :: Maybe (Map.Map String ByteArray)
      <*> arbitrary -- v1SecretKind :: Maybe Text
      <*> arbitrary -- v1SecretMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1SecretStringData :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1SecretType :: Maybe Text
    
instance Arbitrary V1SecretEnvSource where
  arbitrary =
    V1SecretEnvSource
      <$> arbitrary -- v1SecretEnvSourceName :: Maybe Text
      <*> arbitrary -- v1SecretEnvSourceOptional :: Maybe Bool
    
instance Arbitrary V1SecretKeySelector where
  arbitrary =
    V1SecretKeySelector
      <$> arbitrary -- v1SecretKeySelectorKey :: Text
      <*> arbitrary -- v1SecretKeySelectorName :: Maybe Text
      <*> arbitrary -- v1SecretKeySelectorOptional :: Maybe Bool
    
instance Arbitrary V1SecretList where
  arbitrary =
    V1SecretList
      <$> arbitrary -- v1SecretListApiVersion :: Maybe Text
      <*> arbitrary -- v1SecretListItems :: [V1Secret]
      <*> arbitrary -- v1SecretListKind :: Maybe Text
      <*> arbitrary -- v1SecretListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1SecretProjection where
  arbitrary =
    V1SecretProjection
      <$> arbitrary -- v1SecretProjectionItems :: Maybe [V1KeyToPath]
      <*> arbitrary -- v1SecretProjectionName :: Maybe Text
      <*> arbitrary -- v1SecretProjectionOptional :: Maybe Bool
    
instance Arbitrary V1SecretReference where
  arbitrary =
    V1SecretReference
      <$> arbitrary -- v1SecretReferenceName :: Maybe Text
      <*> arbitrary -- v1SecretReferenceNamespace :: Maybe Text
    
instance Arbitrary V1SecretVolumeSource where
  arbitrary =
    V1SecretVolumeSource
      <$> arbitrary -- v1SecretVolumeSourceDefaultMode :: Maybe Int
      <*> arbitrary -- v1SecretVolumeSourceItems :: Maybe [V1KeyToPath]
      <*> arbitrary -- v1SecretVolumeSourceOptional :: Maybe Bool
      <*> arbitrary -- v1SecretVolumeSourceSecretName :: Maybe Text
    
instance Arbitrary V1SecurityContext where
  arbitrary =
    V1SecurityContext
      <$> arbitrary -- v1SecurityContextAllowPrivilegeEscalation :: Maybe Bool
      <*> arbitrary -- v1SecurityContextCapabilities :: Maybe V1Capabilities
      <*> arbitrary -- v1SecurityContextPrivileged :: Maybe Bool
      <*> arbitrary -- v1SecurityContextReadOnlyRootFilesystem :: Maybe Bool
      <*> arbitrary -- v1SecurityContextRunAsNonRoot :: Maybe Bool
      <*> arbitrary -- v1SecurityContextRunAsUser :: Maybe Integer
      <*> arbitrary -- v1SecurityContextSeLinuxOptions :: Maybe V1SELinuxOptions
    
instance Arbitrary V1SelfSubjectAccessReview where
  arbitrary =
    V1SelfSubjectAccessReview
      <$> arbitrary -- v1SelfSubjectAccessReviewApiVersion :: Maybe Text
      <*> arbitrary -- v1SelfSubjectAccessReviewKind :: Maybe Text
      <*> arbitrary -- v1SelfSubjectAccessReviewMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1SelfSubjectAccessReviewSpec :: V1SelfSubjectAccessReviewSpec
      <*> arbitrary -- v1SelfSubjectAccessReviewStatus :: Maybe V1SubjectAccessReviewStatus
    
instance Arbitrary V1SelfSubjectAccessReviewSpec where
  arbitrary =
    V1SelfSubjectAccessReviewSpec
      <$> arbitrary -- v1SelfSubjectAccessReviewSpecNonResourceAttributes :: Maybe V1NonResourceAttributes
      <*> arbitrary -- v1SelfSubjectAccessReviewSpecResourceAttributes :: Maybe V1ResourceAttributes
    
instance Arbitrary V1SelfSubjectRulesReview where
  arbitrary =
    V1SelfSubjectRulesReview
      <$> arbitrary -- v1SelfSubjectRulesReviewApiVersion :: Maybe Text
      <*> arbitrary -- v1SelfSubjectRulesReviewKind :: Maybe Text
      <*> arbitrary -- v1SelfSubjectRulesReviewMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1SelfSubjectRulesReviewSpec :: V1SelfSubjectRulesReviewSpec
      <*> arbitrary -- v1SelfSubjectRulesReviewStatus :: Maybe V1SubjectRulesReviewStatus
    
instance Arbitrary V1SelfSubjectRulesReviewSpec where
  arbitrary =
    V1SelfSubjectRulesReviewSpec
      <$> arbitrary -- v1SelfSubjectRulesReviewSpecNamespace :: Maybe Text
    
instance Arbitrary V1ServerAddressByClientCIDR where
  arbitrary =
    V1ServerAddressByClientCIDR
      <$> arbitrary -- v1ServerAddressByClientCIDRClientCidr :: Text
      <*> arbitrary -- v1ServerAddressByClientCIDRServerAddress :: Text
    
instance Arbitrary V1Service where
  arbitrary =
    V1Service
      <$> arbitrary -- v1ServiceApiVersion :: Maybe Text
      <*> arbitrary -- v1ServiceKind :: Maybe Text
      <*> arbitrary -- v1ServiceMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1ServiceSpec :: Maybe V1ServiceSpec
      <*> arbitrary -- v1ServiceStatus :: Maybe V1ServiceStatus
    
instance Arbitrary V1ServiceAccount where
  arbitrary =
    V1ServiceAccount
      <$> arbitrary -- v1ServiceAccountApiVersion :: Maybe Text
      <*> arbitrary -- v1ServiceAccountAutomountServiceAccountToken :: Maybe Bool
      <*> arbitrary -- v1ServiceAccountImagePullSecrets :: Maybe [V1LocalObjectReference]
      <*> arbitrary -- v1ServiceAccountKind :: Maybe Text
      <*> arbitrary -- v1ServiceAccountMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1ServiceAccountSecrets :: Maybe [V1ObjectReference]
    
instance Arbitrary V1ServiceAccountList where
  arbitrary =
    V1ServiceAccountList
      <$> arbitrary -- v1ServiceAccountListApiVersion :: Maybe Text
      <*> arbitrary -- v1ServiceAccountListItems :: [V1ServiceAccount]
      <*> arbitrary -- v1ServiceAccountListKind :: Maybe Text
      <*> arbitrary -- v1ServiceAccountListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1ServiceList where
  arbitrary =
    V1ServiceList
      <$> arbitrary -- v1ServiceListApiVersion :: Maybe Text
      <*> arbitrary -- v1ServiceListItems :: [V1Service]
      <*> arbitrary -- v1ServiceListKind :: Maybe Text
      <*> arbitrary -- v1ServiceListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1ServicePort where
  arbitrary =
    V1ServicePort
      <$> arbitrary -- v1ServicePortName :: Maybe Text
      <*> arbitrary -- v1ServicePortNodePort :: Maybe Int
      <*> arbitrary -- v1ServicePortPort :: Int
      <*> arbitrary -- v1ServicePortProtocol :: Maybe Text
      <*> arbitrary -- v1ServicePortTargetPort :: Maybe A.Value
    
instance Arbitrary V1ServiceSpec where
  arbitrary =
    V1ServiceSpec
      <$> arbitrary -- v1ServiceSpecClusterIp :: Maybe Text
      <*> arbitrary -- v1ServiceSpecExternalIPs :: Maybe [Text]
      <*> arbitrary -- v1ServiceSpecExternalName :: Maybe Text
      <*> arbitrary -- v1ServiceSpecExternalTrafficPolicy :: Maybe Text
      <*> arbitrary -- v1ServiceSpecHealthCheckNodePort :: Maybe Int
      <*> arbitrary -- v1ServiceSpecLoadBalancerIp :: Maybe Text
      <*> arbitrary -- v1ServiceSpecLoadBalancerSourceRanges :: Maybe [Text]
      <*> arbitrary -- v1ServiceSpecPorts :: Maybe [V1ServicePort]
      <*> arbitrary -- v1ServiceSpecPublishNotReadyAddresses :: Maybe Bool
      <*> arbitrary -- v1ServiceSpecSelector :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1ServiceSpecSessionAffinity :: Maybe Text
      <*> arbitrary -- v1ServiceSpecSessionAffinityConfig :: Maybe V1SessionAffinityConfig
      <*> arbitrary -- v1ServiceSpecType :: Maybe Text
    
instance Arbitrary V1ServiceStatus where
  arbitrary =
    V1ServiceStatus
      <$> arbitrary -- v1ServiceStatusLoadBalancer :: Maybe V1LoadBalancerStatus
    
instance Arbitrary V1SessionAffinityConfig where
  arbitrary =
    V1SessionAffinityConfig
      <$> arbitrary -- v1SessionAffinityConfigClientIp :: Maybe V1ClientIPConfig
    
instance Arbitrary V1StatefulSet where
  arbitrary =
    V1StatefulSet
      <$> arbitrary -- v1StatefulSetApiVersion :: Maybe Text
      <*> arbitrary -- v1StatefulSetKind :: Maybe Text
      <*> arbitrary -- v1StatefulSetMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1StatefulSetSpec :: Maybe V1StatefulSetSpec
      <*> arbitrary -- v1StatefulSetStatus :: Maybe V1StatefulSetStatus
    
instance Arbitrary V1StatefulSetCondition where
  arbitrary =
    V1StatefulSetCondition
      <$> arbitrary -- v1StatefulSetConditionLastTransitionTime :: Maybe DateTime
      <*> arbitrary -- v1StatefulSetConditionMessage :: Maybe Text
      <*> arbitrary -- v1StatefulSetConditionReason :: Maybe Text
      <*> arbitrary -- v1StatefulSetConditionStatus :: Text
      <*> arbitrary -- v1StatefulSetConditionType :: Text
    
instance Arbitrary V1StatefulSetList where
  arbitrary =
    V1StatefulSetList
      <$> arbitrary -- v1StatefulSetListApiVersion :: Maybe Text
      <*> arbitrary -- v1StatefulSetListItems :: [V1StatefulSet]
      <*> arbitrary -- v1StatefulSetListKind :: Maybe Text
      <*> arbitrary -- v1StatefulSetListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1StatefulSetSpec where
  arbitrary =
    V1StatefulSetSpec
      <$> arbitrary -- v1StatefulSetSpecPodManagementPolicy :: Maybe Text
      <*> arbitrary -- v1StatefulSetSpecReplicas :: Maybe Int
      <*> arbitrary -- v1StatefulSetSpecRevisionHistoryLimit :: Maybe Int
      <*> arbitrary -- v1StatefulSetSpecSelector :: V1LabelSelector
      <*> arbitrary -- v1StatefulSetSpecServiceName :: Text
      <*> arbitrary -- v1StatefulSetSpecTemplate :: V1PodTemplateSpec
      <*> arbitrary -- v1StatefulSetSpecUpdateStrategy :: Maybe V1StatefulSetUpdateStrategy
      <*> arbitrary -- v1StatefulSetSpecVolumeClaimTemplates :: Maybe [V1PersistentVolumeClaim]
    
instance Arbitrary V1StatefulSetStatus where
  arbitrary =
    V1StatefulSetStatus
      <$> arbitrary -- v1StatefulSetStatusCollisionCount :: Maybe Int
      <*> arbitrary -- v1StatefulSetStatusConditions :: Maybe [V1StatefulSetCondition]
      <*> arbitrary -- v1StatefulSetStatusCurrentReplicas :: Maybe Int
      <*> arbitrary -- v1StatefulSetStatusCurrentRevision :: Maybe Text
      <*> arbitrary -- v1StatefulSetStatusObservedGeneration :: Maybe Integer
      <*> arbitrary -- v1StatefulSetStatusReadyReplicas :: Maybe Int
      <*> arbitrary -- v1StatefulSetStatusReplicas :: Int
      <*> arbitrary -- v1StatefulSetStatusUpdateRevision :: Maybe Text
      <*> arbitrary -- v1StatefulSetStatusUpdatedReplicas :: Maybe Int
    
instance Arbitrary V1StatefulSetUpdateStrategy where
  arbitrary =
    V1StatefulSetUpdateStrategy
      <$> arbitrary -- v1StatefulSetUpdateStrategyRollingUpdate :: Maybe V1RollingUpdateStatefulSetStrategy
      <*> arbitrary -- v1StatefulSetUpdateStrategyType :: Maybe Text
    
instance Arbitrary V1Status where
  arbitrary =
    V1Status
      <$> arbitrary -- v1StatusApiVersion :: Maybe Text
      <*> arbitrary -- v1StatusCode :: Maybe Int
      <*> arbitrary -- v1StatusDetails :: Maybe V1StatusDetails
      <*> arbitrary -- v1StatusKind :: Maybe Text
      <*> arbitrary -- v1StatusMessage :: Maybe Text
      <*> arbitrary -- v1StatusMetadata :: Maybe V1ListMeta
      <*> arbitrary -- v1StatusReason :: Maybe Text
      <*> arbitrary -- v1StatusStatus :: Maybe Text
    
instance Arbitrary V1StatusCause where
  arbitrary =
    V1StatusCause
      <$> arbitrary -- v1StatusCauseField :: Maybe Text
      <*> arbitrary -- v1StatusCauseMessage :: Maybe Text
      <*> arbitrary -- v1StatusCauseReason :: Maybe Text
    
instance Arbitrary V1StatusDetails where
  arbitrary =
    V1StatusDetails
      <$> arbitrary -- v1StatusDetailsCauses :: Maybe [V1StatusCause]
      <*> arbitrary -- v1StatusDetailsGroup :: Maybe Text
      <*> arbitrary -- v1StatusDetailsKind :: Maybe Text
      <*> arbitrary -- v1StatusDetailsName :: Maybe Text
      <*> arbitrary -- v1StatusDetailsRetryAfterSeconds :: Maybe Int
      <*> arbitrary -- v1StatusDetailsUid :: Maybe Text
    
instance Arbitrary V1StorageClass where
  arbitrary =
    V1StorageClass
      <$> arbitrary -- v1StorageClassAllowVolumeExpansion :: Maybe Bool
      <*> arbitrary -- v1StorageClassApiVersion :: Maybe Text
      <*> arbitrary -- v1StorageClassKind :: Maybe Text
      <*> arbitrary -- v1StorageClassMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1StorageClassMountOptions :: Maybe [Text]
      <*> arbitrary -- v1StorageClassParameters :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1StorageClassProvisioner :: Text
      <*> arbitrary -- v1StorageClassReclaimPolicy :: Maybe Text
      <*> arbitrary -- v1StorageClassVolumeBindingMode :: Maybe Text
    
instance Arbitrary V1StorageClassList where
  arbitrary =
    V1StorageClassList
      <$> arbitrary -- v1StorageClassListApiVersion :: Maybe Text
      <*> arbitrary -- v1StorageClassListItems :: [V1StorageClass]
      <*> arbitrary -- v1StorageClassListKind :: Maybe Text
      <*> arbitrary -- v1StorageClassListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1StorageOSPersistentVolumeSource where
  arbitrary =
    V1StorageOSPersistentVolumeSource
      <$> arbitrary -- v1StorageOSPersistentVolumeSourceFsType :: Maybe Text
      <*> arbitrary -- v1StorageOSPersistentVolumeSourceReadOnly :: Maybe Bool
      <*> arbitrary -- v1StorageOSPersistentVolumeSourceSecretRef :: Maybe V1ObjectReference
      <*> arbitrary -- v1StorageOSPersistentVolumeSourceVolumeName :: Maybe Text
      <*> arbitrary -- v1StorageOSPersistentVolumeSourceVolumeNamespace :: Maybe Text
    
instance Arbitrary V1StorageOSVolumeSource where
  arbitrary =
    V1StorageOSVolumeSource
      <$> arbitrary -- v1StorageOSVolumeSourceFsType :: Maybe Text
      <*> arbitrary -- v1StorageOSVolumeSourceReadOnly :: Maybe Bool
      <*> arbitrary -- v1StorageOSVolumeSourceSecretRef :: Maybe V1LocalObjectReference
      <*> arbitrary -- v1StorageOSVolumeSourceVolumeName :: Maybe Text
      <*> arbitrary -- v1StorageOSVolumeSourceVolumeNamespace :: Maybe Text
    
instance Arbitrary V1Subject where
  arbitrary =
    V1Subject
      <$> arbitrary -- v1SubjectApiGroup :: Maybe Text
      <*> arbitrary -- v1SubjectKind :: Text
      <*> arbitrary -- v1SubjectName :: Text
      <*> arbitrary -- v1SubjectNamespace :: Maybe Text
    
instance Arbitrary V1SubjectAccessReview where
  arbitrary =
    V1SubjectAccessReview
      <$> arbitrary -- v1SubjectAccessReviewApiVersion :: Maybe Text
      <*> arbitrary -- v1SubjectAccessReviewKind :: Maybe Text
      <*> arbitrary -- v1SubjectAccessReviewMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1SubjectAccessReviewSpec :: V1SubjectAccessReviewSpec
      <*> arbitrary -- v1SubjectAccessReviewStatus :: Maybe V1SubjectAccessReviewStatus
    
instance Arbitrary V1SubjectAccessReviewSpec where
  arbitrary =
    V1SubjectAccessReviewSpec
      <$> arbitrary -- v1SubjectAccessReviewSpecExtra :: Maybe (Map.Map String [Text])
      <*> arbitrary -- v1SubjectAccessReviewSpecGroups :: Maybe [Text]
      <*> arbitrary -- v1SubjectAccessReviewSpecNonResourceAttributes :: Maybe V1NonResourceAttributes
      <*> arbitrary -- v1SubjectAccessReviewSpecResourceAttributes :: Maybe V1ResourceAttributes
      <*> arbitrary -- v1SubjectAccessReviewSpecUid :: Maybe Text
      <*> arbitrary -- v1SubjectAccessReviewSpecUser :: Maybe Text
    
instance Arbitrary V1SubjectAccessReviewStatus where
  arbitrary =
    V1SubjectAccessReviewStatus
      <$> arbitrary -- v1SubjectAccessReviewStatusAllowed :: Bool
      <*> arbitrary -- v1SubjectAccessReviewStatusDenied :: Maybe Bool
      <*> arbitrary -- v1SubjectAccessReviewStatusEvaluationError :: Maybe Text
      <*> arbitrary -- v1SubjectAccessReviewStatusReason :: Maybe Text
    
instance Arbitrary V1SubjectRulesReviewStatus where
  arbitrary =
    V1SubjectRulesReviewStatus
      <$> arbitrary -- v1SubjectRulesReviewStatusEvaluationError :: Maybe Text
      <*> arbitrary -- v1SubjectRulesReviewStatusIncomplete :: Bool
      <*> arbitrary -- v1SubjectRulesReviewStatusNonResourceRules :: [V1NonResourceRule]
      <*> arbitrary -- v1SubjectRulesReviewStatusResourceRules :: [V1ResourceRule]
    
instance Arbitrary V1TCPSocketAction where
  arbitrary =
    V1TCPSocketAction
      <$> arbitrary -- v1TCPSocketActionHost :: Maybe Text
      <*> arbitrary -- v1TCPSocketActionPort :: A.Value
    
instance Arbitrary V1Taint where
  arbitrary =
    V1Taint
      <$> arbitrary -- v1TaintEffect :: Text
      <*> arbitrary -- v1TaintKey :: Text
      <*> arbitrary -- v1TaintTimeAdded :: Maybe DateTime
      <*> arbitrary -- v1TaintValue :: Maybe Text
    
instance Arbitrary V1TokenReview where
  arbitrary =
    V1TokenReview
      <$> arbitrary -- v1TokenReviewApiVersion :: Maybe Text
      <*> arbitrary -- v1TokenReviewKind :: Maybe Text
      <*> arbitrary -- v1TokenReviewMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1TokenReviewSpec :: V1TokenReviewSpec
      <*> arbitrary -- v1TokenReviewStatus :: Maybe V1TokenReviewStatus
    
instance Arbitrary V1TokenReviewSpec where
  arbitrary =
    V1TokenReviewSpec
      <$> arbitrary -- v1TokenReviewSpecToken :: Maybe Text
    
instance Arbitrary V1TokenReviewStatus where
  arbitrary =
    V1TokenReviewStatus
      <$> arbitrary -- v1TokenReviewStatusAuthenticated :: Maybe Bool
      <*> arbitrary -- v1TokenReviewStatusError :: Maybe Text
      <*> arbitrary -- v1TokenReviewStatusUser :: Maybe V1UserInfo
    
instance Arbitrary V1Toleration where
  arbitrary =
    V1Toleration
      <$> arbitrary -- v1TolerationEffect :: Maybe Text
      <*> arbitrary -- v1TolerationKey :: Maybe Text
      <*> arbitrary -- v1TolerationOperator :: Maybe Text
      <*> arbitrary -- v1TolerationTolerationSeconds :: Maybe Integer
      <*> arbitrary -- v1TolerationValue :: Maybe Text
    
instance Arbitrary V1UserInfo where
  arbitrary =
    V1UserInfo
      <$> arbitrary -- v1UserInfoExtra :: Maybe (Map.Map String [Text])
      <*> arbitrary -- v1UserInfoGroups :: Maybe [Text]
      <*> arbitrary -- v1UserInfoUid :: Maybe Text
      <*> arbitrary -- v1UserInfoUsername :: Maybe Text
    
instance Arbitrary V1Volume where
  arbitrary =
    V1Volume
      <$> arbitrary -- v1VolumeAwsElasticBlockStore :: Maybe V1AWSElasticBlockStoreVolumeSource
      <*> arbitrary -- v1VolumeAzureDisk :: Maybe V1AzureDiskVolumeSource
      <*> arbitrary -- v1VolumeAzureFile :: Maybe V1AzureFileVolumeSource
      <*> arbitrary -- v1VolumeCephfs :: Maybe V1CephFSVolumeSource
      <*> arbitrary -- v1VolumeCinder :: Maybe V1CinderVolumeSource
      <*> arbitrary -- v1VolumeConfigMap :: Maybe V1ConfigMapVolumeSource
      <*> arbitrary -- v1VolumeDownwardApi :: Maybe V1DownwardAPIVolumeSource
      <*> arbitrary -- v1VolumeEmptyDir :: Maybe V1EmptyDirVolumeSource
      <*> arbitrary -- v1VolumeFc :: Maybe V1FCVolumeSource
      <*> arbitrary -- v1VolumeFlexVolume :: Maybe V1FlexVolumeSource
      <*> arbitrary -- v1VolumeFlocker :: Maybe V1FlockerVolumeSource
      <*> arbitrary -- v1VolumeGcePersistentDisk :: Maybe V1GCEPersistentDiskVolumeSource
      <*> arbitrary -- v1VolumeGitRepo :: Maybe V1GitRepoVolumeSource
      <*> arbitrary -- v1VolumeGlusterfs :: Maybe V1GlusterfsVolumeSource
      <*> arbitrary -- v1VolumeHostPath :: Maybe V1HostPathVolumeSource
      <*> arbitrary -- v1VolumeIscsi :: Maybe V1ISCSIVolumeSource
      <*> arbitrary -- v1VolumeName :: Text
      <*> arbitrary -- v1VolumeNfs :: Maybe V1NFSVolumeSource
      <*> arbitrary -- v1VolumePersistentVolumeClaim :: Maybe V1PersistentVolumeClaimVolumeSource
      <*> arbitrary -- v1VolumePhotonPersistentDisk :: Maybe V1PhotonPersistentDiskVolumeSource
      <*> arbitrary -- v1VolumePortworxVolume :: Maybe V1PortworxVolumeSource
      <*> arbitrary -- v1VolumeProjected :: Maybe V1ProjectedVolumeSource
      <*> arbitrary -- v1VolumeQuobyte :: Maybe V1QuobyteVolumeSource
      <*> arbitrary -- v1VolumeRbd :: Maybe V1RBDVolumeSource
      <*> arbitrary -- v1VolumeScaleIo :: Maybe V1ScaleIOVolumeSource
      <*> arbitrary -- v1VolumeSecret :: Maybe V1SecretVolumeSource
      <*> arbitrary -- v1VolumeStorageos :: Maybe V1StorageOSVolumeSource
      <*> arbitrary -- v1VolumeVsphereVolume :: Maybe V1VsphereVirtualDiskVolumeSource
    
instance Arbitrary V1VolumeDevice where
  arbitrary =
    V1VolumeDevice
      <$> arbitrary -- v1VolumeDeviceDevicePath :: Text
      <*> arbitrary -- v1VolumeDeviceName :: Text
    
instance Arbitrary V1VolumeMount where
  arbitrary =
    V1VolumeMount
      <$> arbitrary -- v1VolumeMountMountPath :: Text
      <*> arbitrary -- v1VolumeMountMountPropagation :: Maybe Text
      <*> arbitrary -- v1VolumeMountName :: Text
      <*> arbitrary -- v1VolumeMountReadOnly :: Maybe Bool
      <*> arbitrary -- v1VolumeMountSubPath :: Maybe Text
    
instance Arbitrary V1VolumeProjection where
  arbitrary =
    V1VolumeProjection
      <$> arbitrary -- v1VolumeProjectionConfigMap :: Maybe V1ConfigMapProjection
      <*> arbitrary -- v1VolumeProjectionDownwardApi :: Maybe V1DownwardAPIProjection
      <*> arbitrary -- v1VolumeProjectionSecret :: Maybe V1SecretProjection
    
instance Arbitrary V1VsphereVirtualDiskVolumeSource where
  arbitrary =
    V1VsphereVirtualDiskVolumeSource
      <$> arbitrary -- v1VsphereVirtualDiskVolumeSourceFsType :: Maybe Text
      <*> arbitrary -- v1VsphereVirtualDiskVolumeSourceStoragePolicyId :: Maybe Text
      <*> arbitrary -- v1VsphereVirtualDiskVolumeSourceStoragePolicyName :: Maybe Text
      <*> arbitrary -- v1VsphereVirtualDiskVolumeSourceVolumePath :: Text
    
instance Arbitrary V1WatchEvent where
  arbitrary =
    V1WatchEvent
      <$> arbitrary -- v1WatchEventObject :: RuntimeRawExtension
      <*> arbitrary -- v1WatchEventType :: Text
    
instance Arbitrary V1WeightedPodAffinityTerm where
  arbitrary =
    V1WeightedPodAffinityTerm
      <$> arbitrary -- v1WeightedPodAffinityTermPodAffinityTerm :: V1PodAffinityTerm
      <*> arbitrary -- v1WeightedPodAffinityTermWeight :: Int
    
instance Arbitrary V1alpha1AggregationRule where
  arbitrary =
    V1alpha1AggregationRule
      <$> arbitrary -- v1alpha1AggregationRuleClusterRoleSelectors :: Maybe [V1LabelSelector]
    
instance Arbitrary V1alpha1ClusterRole where
  arbitrary =
    V1alpha1ClusterRole
      <$> arbitrary -- v1alpha1ClusterRoleAggregationRule :: Maybe V1alpha1AggregationRule
      <*> arbitrary -- v1alpha1ClusterRoleApiVersion :: Maybe Text
      <*> arbitrary -- v1alpha1ClusterRoleKind :: Maybe Text
      <*> arbitrary -- v1alpha1ClusterRoleMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1alpha1ClusterRoleRules :: [V1alpha1PolicyRule]
    
instance Arbitrary V1alpha1ClusterRoleBinding where
  arbitrary =
    V1alpha1ClusterRoleBinding
      <$> arbitrary -- v1alpha1ClusterRoleBindingApiVersion :: Maybe Text
      <*> arbitrary -- v1alpha1ClusterRoleBindingKind :: Maybe Text
      <*> arbitrary -- v1alpha1ClusterRoleBindingMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1alpha1ClusterRoleBindingRoleRef :: V1alpha1RoleRef
      <*> arbitrary -- v1alpha1ClusterRoleBindingSubjects :: [V1alpha1Subject]
    
instance Arbitrary V1alpha1ClusterRoleBindingList where
  arbitrary =
    V1alpha1ClusterRoleBindingList
      <$> arbitrary -- v1alpha1ClusterRoleBindingListApiVersion :: Maybe Text
      <*> arbitrary -- v1alpha1ClusterRoleBindingListItems :: [V1alpha1ClusterRoleBinding]
      <*> arbitrary -- v1alpha1ClusterRoleBindingListKind :: Maybe Text
      <*> arbitrary -- v1alpha1ClusterRoleBindingListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1alpha1ClusterRoleList where
  arbitrary =
    V1alpha1ClusterRoleList
      <$> arbitrary -- v1alpha1ClusterRoleListApiVersion :: Maybe Text
      <*> arbitrary -- v1alpha1ClusterRoleListItems :: [V1alpha1ClusterRole]
      <*> arbitrary -- v1alpha1ClusterRoleListKind :: Maybe Text
      <*> arbitrary -- v1alpha1ClusterRoleListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1alpha1Initializer where
  arbitrary =
    V1alpha1Initializer
      <$> arbitrary -- v1alpha1InitializerName :: Text
      <*> arbitrary -- v1alpha1InitializerRules :: Maybe [V1alpha1Rule]
    
instance Arbitrary V1alpha1InitializerConfiguration where
  arbitrary =
    V1alpha1InitializerConfiguration
      <$> arbitrary -- v1alpha1InitializerConfigurationApiVersion :: Maybe Text
      <*> arbitrary -- v1alpha1InitializerConfigurationInitializers :: Maybe [V1alpha1Initializer]
      <*> arbitrary -- v1alpha1InitializerConfigurationKind :: Maybe Text
      <*> arbitrary -- v1alpha1InitializerConfigurationMetadata :: Maybe V1ObjectMeta
    
instance Arbitrary V1alpha1InitializerConfigurationList where
  arbitrary =
    V1alpha1InitializerConfigurationList
      <$> arbitrary -- v1alpha1InitializerConfigurationListApiVersion :: Maybe Text
      <*> arbitrary -- v1alpha1InitializerConfigurationListItems :: [V1alpha1InitializerConfiguration]
      <*> arbitrary -- v1alpha1InitializerConfigurationListKind :: Maybe Text
      <*> arbitrary -- v1alpha1InitializerConfigurationListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1alpha1PodPreset where
  arbitrary =
    V1alpha1PodPreset
      <$> arbitrary -- v1alpha1PodPresetApiVersion :: Maybe Text
      <*> arbitrary -- v1alpha1PodPresetKind :: Maybe Text
      <*> arbitrary -- v1alpha1PodPresetMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1alpha1PodPresetSpec :: Maybe V1alpha1PodPresetSpec
    
instance Arbitrary V1alpha1PodPresetList where
  arbitrary =
    V1alpha1PodPresetList
      <$> arbitrary -- v1alpha1PodPresetListApiVersion :: Maybe Text
      <*> arbitrary -- v1alpha1PodPresetListItems :: [V1alpha1PodPreset]
      <*> arbitrary -- v1alpha1PodPresetListKind :: Maybe Text
      <*> arbitrary -- v1alpha1PodPresetListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1alpha1PodPresetSpec where
  arbitrary =
    V1alpha1PodPresetSpec
      <$> arbitrary -- v1alpha1PodPresetSpecEnv :: Maybe [V1EnvVar]
      <*> arbitrary -- v1alpha1PodPresetSpecEnvFrom :: Maybe [V1EnvFromSource]
      <*> arbitrary -- v1alpha1PodPresetSpecSelector :: Maybe V1LabelSelector
      <*> arbitrary -- v1alpha1PodPresetSpecVolumeMounts :: Maybe [V1VolumeMount]
      <*> arbitrary -- v1alpha1PodPresetSpecVolumes :: Maybe [V1Volume]
    
instance Arbitrary V1alpha1PolicyRule where
  arbitrary =
    V1alpha1PolicyRule
      <$> arbitrary -- v1alpha1PolicyRuleApiGroups :: Maybe [Text]
      <*> arbitrary -- v1alpha1PolicyRuleNonResourceUrLs :: Maybe [Text]
      <*> arbitrary -- v1alpha1PolicyRuleResourceNames :: Maybe [Text]
      <*> arbitrary -- v1alpha1PolicyRuleResources :: Maybe [Text]
      <*> arbitrary -- v1alpha1PolicyRuleVerbs :: [Text]
    
instance Arbitrary V1alpha1PriorityClass where
  arbitrary =
    V1alpha1PriorityClass
      <$> arbitrary -- v1alpha1PriorityClassApiVersion :: Maybe Text
      <*> arbitrary -- v1alpha1PriorityClassDescription :: Maybe Text
      <*> arbitrary -- v1alpha1PriorityClassGlobalDefault :: Maybe Bool
      <*> arbitrary -- v1alpha1PriorityClassKind :: Maybe Text
      <*> arbitrary -- v1alpha1PriorityClassMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1alpha1PriorityClassValue :: Int
    
instance Arbitrary V1alpha1PriorityClassList where
  arbitrary =
    V1alpha1PriorityClassList
      <$> arbitrary -- v1alpha1PriorityClassListApiVersion :: Maybe Text
      <*> arbitrary -- v1alpha1PriorityClassListItems :: [V1alpha1PriorityClass]
      <*> arbitrary -- v1alpha1PriorityClassListKind :: Maybe Text
      <*> arbitrary -- v1alpha1PriorityClassListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1alpha1Role where
  arbitrary =
    V1alpha1Role
      <$> arbitrary -- v1alpha1RoleApiVersion :: Maybe Text
      <*> arbitrary -- v1alpha1RoleKind :: Maybe Text
      <*> arbitrary -- v1alpha1RoleMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1alpha1RoleRules :: [V1alpha1PolicyRule]
    
instance Arbitrary V1alpha1RoleBinding where
  arbitrary =
    V1alpha1RoleBinding
      <$> arbitrary -- v1alpha1RoleBindingApiVersion :: Maybe Text
      <*> arbitrary -- v1alpha1RoleBindingKind :: Maybe Text
      <*> arbitrary -- v1alpha1RoleBindingMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1alpha1RoleBindingRoleRef :: V1alpha1RoleRef
      <*> arbitrary -- v1alpha1RoleBindingSubjects :: [V1alpha1Subject]
    
instance Arbitrary V1alpha1RoleBindingList where
  arbitrary =
    V1alpha1RoleBindingList
      <$> arbitrary -- v1alpha1RoleBindingListApiVersion :: Maybe Text
      <*> arbitrary -- v1alpha1RoleBindingListItems :: [V1alpha1RoleBinding]
      <*> arbitrary -- v1alpha1RoleBindingListKind :: Maybe Text
      <*> arbitrary -- v1alpha1RoleBindingListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1alpha1RoleList where
  arbitrary =
    V1alpha1RoleList
      <$> arbitrary -- v1alpha1RoleListApiVersion :: Maybe Text
      <*> arbitrary -- v1alpha1RoleListItems :: [V1alpha1Role]
      <*> arbitrary -- v1alpha1RoleListKind :: Maybe Text
      <*> arbitrary -- v1alpha1RoleListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1alpha1RoleRef where
  arbitrary =
    V1alpha1RoleRef
      <$> arbitrary -- v1alpha1RoleRefApiGroup :: Text
      <*> arbitrary -- v1alpha1RoleRefKind :: Text
      <*> arbitrary -- v1alpha1RoleRefName :: Text
    
instance Arbitrary V1alpha1Rule where
  arbitrary =
    V1alpha1Rule
      <$> arbitrary -- v1alpha1RuleApiGroups :: Maybe [Text]
      <*> arbitrary -- v1alpha1RuleApiVersions :: Maybe [Text]
      <*> arbitrary -- v1alpha1RuleResources :: Maybe [Text]
    
instance Arbitrary V1alpha1Subject where
  arbitrary =
    V1alpha1Subject
      <$> arbitrary -- v1alpha1SubjectApiVersion :: Maybe Text
      <*> arbitrary -- v1alpha1SubjectKind :: Text
      <*> arbitrary -- v1alpha1SubjectName :: Text
      <*> arbitrary -- v1alpha1SubjectNamespace :: Maybe Text
    
instance Arbitrary V1alpha1VolumeAttachment where
  arbitrary =
    V1alpha1VolumeAttachment
      <$> arbitrary -- v1alpha1VolumeAttachmentApiVersion :: Maybe Text
      <*> arbitrary -- v1alpha1VolumeAttachmentKind :: Maybe Text
      <*> arbitrary -- v1alpha1VolumeAttachmentMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1alpha1VolumeAttachmentSpec :: V1alpha1VolumeAttachmentSpec
      <*> arbitrary -- v1alpha1VolumeAttachmentStatus :: Maybe V1alpha1VolumeAttachmentStatus
    
instance Arbitrary V1alpha1VolumeAttachmentList where
  arbitrary =
    V1alpha1VolumeAttachmentList
      <$> arbitrary -- v1alpha1VolumeAttachmentListApiVersion :: Maybe Text
      <*> arbitrary -- v1alpha1VolumeAttachmentListItems :: [V1alpha1VolumeAttachment]
      <*> arbitrary -- v1alpha1VolumeAttachmentListKind :: Maybe Text
      <*> arbitrary -- v1alpha1VolumeAttachmentListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1alpha1VolumeAttachmentSource where
  arbitrary =
    V1alpha1VolumeAttachmentSource
      <$> arbitrary -- v1alpha1VolumeAttachmentSourcePersistentVolumeName :: Maybe Text
    
instance Arbitrary V1alpha1VolumeAttachmentSpec where
  arbitrary =
    V1alpha1VolumeAttachmentSpec
      <$> arbitrary -- v1alpha1VolumeAttachmentSpecAttacher :: Text
      <*> arbitrary -- v1alpha1VolumeAttachmentSpecNodeName :: Text
      <*> arbitrary -- v1alpha1VolumeAttachmentSpecSource :: V1alpha1VolumeAttachmentSource
    
instance Arbitrary V1alpha1VolumeAttachmentStatus where
  arbitrary =
    V1alpha1VolumeAttachmentStatus
      <$> arbitrary -- v1alpha1VolumeAttachmentStatusAttachError :: Maybe V1alpha1VolumeError
      <*> arbitrary -- v1alpha1VolumeAttachmentStatusAttached :: Bool
      <*> arbitrary -- v1alpha1VolumeAttachmentStatusAttachmentMetadata :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1alpha1VolumeAttachmentStatusDetachError :: Maybe V1alpha1VolumeError
    
instance Arbitrary V1alpha1VolumeError where
  arbitrary =
    V1alpha1VolumeError
      <$> arbitrary -- v1alpha1VolumeErrorMessage :: Maybe Text
      <*> arbitrary -- v1alpha1VolumeErrorTime :: Maybe DateTime
    
instance Arbitrary V1beta1APIService where
  arbitrary =
    V1beta1APIService
      <$> arbitrary -- v1beta1APIServiceApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1APIServiceKind :: Maybe Text
      <*> arbitrary -- v1beta1APIServiceMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1APIServiceSpec :: Maybe V1beta1APIServiceSpec
      <*> arbitrary -- v1beta1APIServiceStatus :: Maybe V1beta1APIServiceStatus
    
instance Arbitrary V1beta1APIServiceCondition where
  arbitrary =
    V1beta1APIServiceCondition
      <$> arbitrary -- v1beta1APIServiceConditionLastTransitionTime :: Maybe DateTime
      <*> arbitrary -- v1beta1APIServiceConditionMessage :: Maybe Text
      <*> arbitrary -- v1beta1APIServiceConditionReason :: Maybe Text
      <*> arbitrary -- v1beta1APIServiceConditionStatus :: Text
      <*> arbitrary -- v1beta1APIServiceConditionType :: Text
    
instance Arbitrary V1beta1APIServiceList where
  arbitrary =
    V1beta1APIServiceList
      <$> arbitrary -- v1beta1APIServiceListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1APIServiceListItems :: [V1beta1APIService]
      <*> arbitrary -- v1beta1APIServiceListKind :: Maybe Text
      <*> arbitrary -- v1beta1APIServiceListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta1APIServiceSpec where
  arbitrary =
    V1beta1APIServiceSpec
      <$> arbitrary -- v1beta1APIServiceSpecCaBundle :: ByteArray
      <*> arbitrary -- v1beta1APIServiceSpecGroup :: Maybe Text
      <*> arbitrary -- v1beta1APIServiceSpecGroupPriorityMinimum :: Int
      <*> arbitrary -- v1beta1APIServiceSpecInsecureSkipTlsVerify :: Maybe Bool
      <*> arbitrary -- v1beta1APIServiceSpecService :: ApiregistrationV1beta1ServiceReference
      <*> arbitrary -- v1beta1APIServiceSpecVersion :: Maybe Text
      <*> arbitrary -- v1beta1APIServiceSpecVersionPriority :: Int
    
instance Arbitrary V1beta1APIServiceStatus where
  arbitrary =
    V1beta1APIServiceStatus
      <$> arbitrary -- v1beta1APIServiceStatusConditions :: Maybe [V1beta1APIServiceCondition]
    
instance Arbitrary V1beta1AggregationRule where
  arbitrary =
    V1beta1AggregationRule
      <$> arbitrary -- v1beta1AggregationRuleClusterRoleSelectors :: Maybe [V1LabelSelector]
    
instance Arbitrary V1beta1AllowedFlexVolume where
  arbitrary =
    V1beta1AllowedFlexVolume
      <$> arbitrary -- v1beta1AllowedFlexVolumeDriver :: Text
    
instance Arbitrary V1beta1AllowedHostPath where
  arbitrary =
    V1beta1AllowedHostPath
      <$> arbitrary -- v1beta1AllowedHostPathPathPrefix :: Maybe Text
    
instance Arbitrary V1beta1CertificateSigningRequest where
  arbitrary =
    V1beta1CertificateSigningRequest
      <$> arbitrary -- v1beta1CertificateSigningRequestApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1CertificateSigningRequestKind :: Maybe Text
      <*> arbitrary -- v1beta1CertificateSigningRequestMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1CertificateSigningRequestSpec :: Maybe V1beta1CertificateSigningRequestSpec
      <*> arbitrary -- v1beta1CertificateSigningRequestStatus :: Maybe V1beta1CertificateSigningRequestStatus
    
instance Arbitrary V1beta1CertificateSigningRequestCondition where
  arbitrary =
    V1beta1CertificateSigningRequestCondition
      <$> arbitrary -- v1beta1CertificateSigningRequestConditionLastUpdateTime :: Maybe DateTime
      <*> arbitrary -- v1beta1CertificateSigningRequestConditionMessage :: Maybe Text
      <*> arbitrary -- v1beta1CertificateSigningRequestConditionReason :: Maybe Text
      <*> arbitrary -- v1beta1CertificateSigningRequestConditionType :: Text
    
instance Arbitrary V1beta1CertificateSigningRequestList where
  arbitrary =
    V1beta1CertificateSigningRequestList
      <$> arbitrary -- v1beta1CertificateSigningRequestListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1CertificateSigningRequestListItems :: [V1beta1CertificateSigningRequest]
      <*> arbitrary -- v1beta1CertificateSigningRequestListKind :: Maybe Text
      <*> arbitrary -- v1beta1CertificateSigningRequestListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta1CertificateSigningRequestSpec where
  arbitrary =
    V1beta1CertificateSigningRequestSpec
      <$> arbitrary -- v1beta1CertificateSigningRequestSpecExtra :: Maybe (Map.Map String [Text])
      <*> arbitrary -- v1beta1CertificateSigningRequestSpecGroups :: Maybe [Text]
      <*> arbitrary -- v1beta1CertificateSigningRequestSpecRequest :: ByteArray
      <*> arbitrary -- v1beta1CertificateSigningRequestSpecUid :: Maybe Text
      <*> arbitrary -- v1beta1CertificateSigningRequestSpecUsages :: Maybe [Text]
      <*> arbitrary -- v1beta1CertificateSigningRequestSpecUsername :: Maybe Text
    
instance Arbitrary V1beta1CertificateSigningRequestStatus where
  arbitrary =
    V1beta1CertificateSigningRequestStatus
      <$> arbitrary -- v1beta1CertificateSigningRequestStatusCertificate :: Maybe ByteArray
      <*> arbitrary -- v1beta1CertificateSigningRequestStatusConditions :: Maybe [V1beta1CertificateSigningRequestCondition]
    
instance Arbitrary V1beta1ClusterRole where
  arbitrary =
    V1beta1ClusterRole
      <$> arbitrary -- v1beta1ClusterRoleAggregationRule :: Maybe V1beta1AggregationRule
      <*> arbitrary -- v1beta1ClusterRoleApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1ClusterRoleKind :: Maybe Text
      <*> arbitrary -- v1beta1ClusterRoleMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1ClusterRoleRules :: [V1beta1PolicyRule]
    
instance Arbitrary V1beta1ClusterRoleBinding where
  arbitrary =
    V1beta1ClusterRoleBinding
      <$> arbitrary -- v1beta1ClusterRoleBindingApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1ClusterRoleBindingKind :: Maybe Text
      <*> arbitrary -- v1beta1ClusterRoleBindingMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1ClusterRoleBindingRoleRef :: V1beta1RoleRef
      <*> arbitrary -- v1beta1ClusterRoleBindingSubjects :: [V1beta1Subject]
    
instance Arbitrary V1beta1ClusterRoleBindingList where
  arbitrary =
    V1beta1ClusterRoleBindingList
      <$> arbitrary -- v1beta1ClusterRoleBindingListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1ClusterRoleBindingListItems :: [V1beta1ClusterRoleBinding]
      <*> arbitrary -- v1beta1ClusterRoleBindingListKind :: Maybe Text
      <*> arbitrary -- v1beta1ClusterRoleBindingListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta1ClusterRoleList where
  arbitrary =
    V1beta1ClusterRoleList
      <$> arbitrary -- v1beta1ClusterRoleListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1ClusterRoleListItems :: [V1beta1ClusterRole]
      <*> arbitrary -- v1beta1ClusterRoleListKind :: Maybe Text
      <*> arbitrary -- v1beta1ClusterRoleListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta1ControllerRevision where
  arbitrary =
    V1beta1ControllerRevision
      <$> arbitrary -- v1beta1ControllerRevisionApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1ControllerRevisionData :: Maybe RuntimeRawExtension
      <*> arbitrary -- v1beta1ControllerRevisionKind :: Maybe Text
      <*> arbitrary -- v1beta1ControllerRevisionMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1ControllerRevisionRevision :: Integer
    
instance Arbitrary V1beta1ControllerRevisionList where
  arbitrary =
    V1beta1ControllerRevisionList
      <$> arbitrary -- v1beta1ControllerRevisionListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1ControllerRevisionListItems :: [V1beta1ControllerRevision]
      <*> arbitrary -- v1beta1ControllerRevisionListKind :: Maybe Text
      <*> arbitrary -- v1beta1ControllerRevisionListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta1CronJob where
  arbitrary =
    V1beta1CronJob
      <$> arbitrary -- v1beta1CronJobApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1CronJobKind :: Maybe Text
      <*> arbitrary -- v1beta1CronJobMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1CronJobSpec :: Maybe V1beta1CronJobSpec
      <*> arbitrary -- v1beta1CronJobStatus :: Maybe V1beta1CronJobStatus
    
instance Arbitrary V1beta1CronJobList where
  arbitrary =
    V1beta1CronJobList
      <$> arbitrary -- v1beta1CronJobListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1CronJobListItems :: [V1beta1CronJob]
      <*> arbitrary -- v1beta1CronJobListKind :: Maybe Text
      <*> arbitrary -- v1beta1CronJobListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta1CronJobSpec where
  arbitrary =
    V1beta1CronJobSpec
      <$> arbitrary -- v1beta1CronJobSpecConcurrencyPolicy :: Maybe Text
      <*> arbitrary -- v1beta1CronJobSpecFailedJobsHistoryLimit :: Maybe Int
      <*> arbitrary -- v1beta1CronJobSpecJobTemplate :: V1beta1JobTemplateSpec
      <*> arbitrary -- v1beta1CronJobSpecSchedule :: Text
      <*> arbitrary -- v1beta1CronJobSpecStartingDeadlineSeconds :: Maybe Integer
      <*> arbitrary -- v1beta1CronJobSpecSuccessfulJobsHistoryLimit :: Maybe Int
      <*> arbitrary -- v1beta1CronJobSpecSuspend :: Maybe Bool
    
instance Arbitrary V1beta1CronJobStatus where
  arbitrary =
    V1beta1CronJobStatus
      <$> arbitrary -- v1beta1CronJobStatusActive :: Maybe [V1ObjectReference]
      <*> arbitrary -- v1beta1CronJobStatusLastScheduleTime :: Maybe DateTime
    
instance Arbitrary V1beta1CustomResourceDefinition where
  arbitrary =
    V1beta1CustomResourceDefinition
      <$> arbitrary -- v1beta1CustomResourceDefinitionApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1CustomResourceDefinitionKind :: Maybe Text
      <*> arbitrary -- v1beta1CustomResourceDefinitionMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1CustomResourceDefinitionSpec :: Maybe V1beta1CustomResourceDefinitionSpec
      <*> arbitrary -- v1beta1CustomResourceDefinitionStatus :: Maybe V1beta1CustomResourceDefinitionStatus
    
instance Arbitrary V1beta1CustomResourceDefinitionCondition where
  arbitrary =
    V1beta1CustomResourceDefinitionCondition
      <$> arbitrary -- v1beta1CustomResourceDefinitionConditionLastTransitionTime :: Maybe DateTime
      <*> arbitrary -- v1beta1CustomResourceDefinitionConditionMessage :: Maybe Text
      <*> arbitrary -- v1beta1CustomResourceDefinitionConditionReason :: Maybe Text
      <*> arbitrary -- v1beta1CustomResourceDefinitionConditionStatus :: Text
      <*> arbitrary -- v1beta1CustomResourceDefinitionConditionType :: Text
    
instance Arbitrary V1beta1CustomResourceDefinitionList where
  arbitrary =
    V1beta1CustomResourceDefinitionList
      <$> arbitrary -- v1beta1CustomResourceDefinitionListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1CustomResourceDefinitionListItems :: [V1beta1CustomResourceDefinition]
      <*> arbitrary -- v1beta1CustomResourceDefinitionListKind :: Maybe Text
      <*> arbitrary -- v1beta1CustomResourceDefinitionListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta1CustomResourceDefinitionNames where
  arbitrary =
    V1beta1CustomResourceDefinitionNames
      <$> arbitrary -- v1beta1CustomResourceDefinitionNamesKind :: Text
      <*> arbitrary -- v1beta1CustomResourceDefinitionNamesListKind :: Maybe Text
      <*> arbitrary -- v1beta1CustomResourceDefinitionNamesPlural :: Text
      <*> arbitrary -- v1beta1CustomResourceDefinitionNamesShortNames :: Maybe [Text]
      <*> arbitrary -- v1beta1CustomResourceDefinitionNamesSingular :: Maybe Text
    
instance Arbitrary V1beta1CustomResourceDefinitionSpec where
  arbitrary =
    V1beta1CustomResourceDefinitionSpec
      <$> arbitrary -- v1beta1CustomResourceDefinitionSpecGroup :: Text
      <*> arbitrary -- v1beta1CustomResourceDefinitionSpecNames :: V1beta1CustomResourceDefinitionNames
      <*> arbitrary -- v1beta1CustomResourceDefinitionSpecScope :: Text
      <*> arbitrary -- v1beta1CustomResourceDefinitionSpecValidation :: Maybe V1beta1CustomResourceValidation
      <*> arbitrary -- v1beta1CustomResourceDefinitionSpecVersion :: Text
    
instance Arbitrary V1beta1CustomResourceDefinitionStatus where
  arbitrary =
    V1beta1CustomResourceDefinitionStatus
      <$> arbitrary -- v1beta1CustomResourceDefinitionStatusAcceptedNames :: V1beta1CustomResourceDefinitionNames
      <*> arbitrary -- v1beta1CustomResourceDefinitionStatusConditions :: [V1beta1CustomResourceDefinitionCondition]
    
instance Arbitrary V1beta1CustomResourceValidation where
  arbitrary =
    V1beta1CustomResourceValidation
      <$> arbitrary -- v1beta1CustomResourceValidationOpenApiv3Schema :: Maybe V1beta1JSONSchemaProps
    
instance Arbitrary V1beta1DaemonSet where
  arbitrary =
    V1beta1DaemonSet
      <$> arbitrary -- v1beta1DaemonSetApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1DaemonSetKind :: Maybe Text
      <*> arbitrary -- v1beta1DaemonSetMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1DaemonSetSpec :: Maybe V1beta1DaemonSetSpec
      <*> arbitrary -- v1beta1DaemonSetStatus :: Maybe V1beta1DaemonSetStatus
    
instance Arbitrary V1beta1DaemonSetCondition where
  arbitrary =
    V1beta1DaemonSetCondition
      <$> arbitrary -- v1beta1DaemonSetConditionLastTransitionTime :: Maybe DateTime
      <*> arbitrary -- v1beta1DaemonSetConditionMessage :: Maybe Text
      <*> arbitrary -- v1beta1DaemonSetConditionReason :: Maybe Text
      <*> arbitrary -- v1beta1DaemonSetConditionStatus :: Text
      <*> arbitrary -- v1beta1DaemonSetConditionType :: Text
    
instance Arbitrary V1beta1DaemonSetList where
  arbitrary =
    V1beta1DaemonSetList
      <$> arbitrary -- v1beta1DaemonSetListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1DaemonSetListItems :: [V1beta1DaemonSet]
      <*> arbitrary -- v1beta1DaemonSetListKind :: Maybe Text
      <*> arbitrary -- v1beta1DaemonSetListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta1DaemonSetSpec where
  arbitrary =
    V1beta1DaemonSetSpec
      <$> arbitrary -- v1beta1DaemonSetSpecMinReadySeconds :: Maybe Int
      <*> arbitrary -- v1beta1DaemonSetSpecRevisionHistoryLimit :: Maybe Int
      <*> arbitrary -- v1beta1DaemonSetSpecSelector :: Maybe V1LabelSelector
      <*> arbitrary -- v1beta1DaemonSetSpecTemplate :: V1PodTemplateSpec
      <*> arbitrary -- v1beta1DaemonSetSpecTemplateGeneration :: Maybe Integer
      <*> arbitrary -- v1beta1DaemonSetSpecUpdateStrategy :: Maybe V1beta1DaemonSetUpdateStrategy
    
instance Arbitrary V1beta1DaemonSetStatus where
  arbitrary =
    V1beta1DaemonSetStatus
      <$> arbitrary -- v1beta1DaemonSetStatusCollisionCount :: Maybe Int
      <*> arbitrary -- v1beta1DaemonSetStatusConditions :: Maybe [V1beta1DaemonSetCondition]
      <*> arbitrary -- v1beta1DaemonSetStatusCurrentNumberScheduled :: Int
      <*> arbitrary -- v1beta1DaemonSetStatusDesiredNumberScheduled :: Int
      <*> arbitrary -- v1beta1DaemonSetStatusNumberAvailable :: Maybe Int
      <*> arbitrary -- v1beta1DaemonSetStatusNumberMisscheduled :: Int
      <*> arbitrary -- v1beta1DaemonSetStatusNumberReady :: Int
      <*> arbitrary -- v1beta1DaemonSetStatusNumberUnavailable :: Maybe Int
      <*> arbitrary -- v1beta1DaemonSetStatusObservedGeneration :: Maybe Integer
      <*> arbitrary -- v1beta1DaemonSetStatusUpdatedNumberScheduled :: Maybe Int
    
instance Arbitrary V1beta1DaemonSetUpdateStrategy where
  arbitrary =
    V1beta1DaemonSetUpdateStrategy
      <$> arbitrary -- v1beta1DaemonSetUpdateStrategyRollingUpdate :: Maybe V1beta1RollingUpdateDaemonSet
      <*> arbitrary -- v1beta1DaemonSetUpdateStrategyType :: Maybe Text
    
instance Arbitrary V1beta1Event where
  arbitrary =
    V1beta1Event
      <$> arbitrary -- v1beta1EventAction :: Maybe Text
      <*> arbitrary -- v1beta1EventApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1EventDeprecatedCount :: Maybe Int
      <*> arbitrary -- v1beta1EventDeprecatedFirstTimestamp :: Maybe DateTime
      <*> arbitrary -- v1beta1EventDeprecatedLastTimestamp :: Maybe DateTime
      <*> arbitrary -- v1beta1EventDeprecatedSource :: Maybe V1EventSource
      <*> arbitrary -- v1beta1EventEventTime :: DateTime
      <*> arbitrary -- v1beta1EventKind :: Maybe Text
      <*> arbitrary -- v1beta1EventMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1EventNote :: Maybe Text
      <*> arbitrary -- v1beta1EventReason :: Maybe Text
      <*> arbitrary -- v1beta1EventRegarding :: Maybe V1ObjectReference
      <*> arbitrary -- v1beta1EventRelated :: Maybe V1ObjectReference
      <*> arbitrary -- v1beta1EventReportingController :: Maybe Text
      <*> arbitrary -- v1beta1EventReportingInstance :: Maybe Text
      <*> arbitrary -- v1beta1EventSeries :: Maybe V1beta1EventSeries
      <*> arbitrary -- v1beta1EventType :: Maybe Text
    
instance Arbitrary V1beta1EventList where
  arbitrary =
    V1beta1EventList
      <$> arbitrary -- v1beta1EventListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1EventListItems :: [V1beta1Event]
      <*> arbitrary -- v1beta1EventListKind :: Maybe Text
      <*> arbitrary -- v1beta1EventListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta1EventSeries where
  arbitrary =
    V1beta1EventSeries
      <$> arbitrary -- v1beta1EventSeriesCount :: Int
      <*> arbitrary -- v1beta1EventSeriesLastObservedTime :: DateTime
      <*> arbitrary -- v1beta1EventSeriesState :: Text
    
instance Arbitrary V1beta1Eviction where
  arbitrary =
    V1beta1Eviction
      <$> arbitrary -- v1beta1EvictionApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1EvictionDeleteOptions :: Maybe V1DeleteOptions
      <*> arbitrary -- v1beta1EvictionKind :: Maybe Text
      <*> arbitrary -- v1beta1EvictionMetadata :: Maybe V1ObjectMeta
    
instance Arbitrary V1beta1ExternalDocumentation where
  arbitrary =
    V1beta1ExternalDocumentation
      <$> arbitrary -- v1beta1ExternalDocumentationDescription :: Maybe Text
      <*> arbitrary -- v1beta1ExternalDocumentationUrl :: Maybe Text
    
instance Arbitrary V1beta1FSGroupStrategyOptions where
  arbitrary =
    V1beta1FSGroupStrategyOptions
      <$> arbitrary -- v1beta1FSGroupStrategyOptionsRanges :: Maybe [V1beta1IDRange]
      <*> arbitrary -- v1beta1FSGroupStrategyOptionsRule :: Maybe Text
    
instance Arbitrary V1beta1HTTPIngressPath where
  arbitrary =
    V1beta1HTTPIngressPath
      <$> arbitrary -- v1beta1HTTPIngressPathBackend :: V1beta1IngressBackend
      <*> arbitrary -- v1beta1HTTPIngressPathPath :: Maybe Text
    
instance Arbitrary V1beta1HTTPIngressRuleValue where
  arbitrary =
    V1beta1HTTPIngressRuleValue
      <$> arbitrary -- v1beta1HTTPIngressRuleValuePaths :: [V1beta1HTTPIngressPath]
    
instance Arbitrary V1beta1HostPortRange where
  arbitrary =
    V1beta1HostPortRange
      <$> arbitrary -- v1beta1HostPortRangeMax :: Int
      <*> arbitrary -- v1beta1HostPortRangeMin :: Int
    
instance Arbitrary V1beta1IDRange where
  arbitrary =
    V1beta1IDRange
      <$> arbitrary -- v1beta1IDRangeMax :: Integer
      <*> arbitrary -- v1beta1IDRangeMin :: Integer
    
instance Arbitrary V1beta1IPBlock where
  arbitrary =
    V1beta1IPBlock
      <$> arbitrary -- v1beta1IPBlockCidr :: Text
      <*> arbitrary -- v1beta1IPBlockExcept :: Maybe [Text]
    
instance Arbitrary V1beta1Ingress where
  arbitrary =
    V1beta1Ingress
      <$> arbitrary -- v1beta1IngressApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1IngressKind :: Maybe Text
      <*> arbitrary -- v1beta1IngressMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1IngressSpec :: Maybe V1beta1IngressSpec
      <*> arbitrary -- v1beta1IngressStatus :: Maybe V1beta1IngressStatus
    
instance Arbitrary V1beta1IngressBackend where
  arbitrary =
    V1beta1IngressBackend
      <$> arbitrary -- v1beta1IngressBackendServiceName :: Text
      <*> arbitrary -- v1beta1IngressBackendServicePort :: A.Value
    
instance Arbitrary V1beta1IngressList where
  arbitrary =
    V1beta1IngressList
      <$> arbitrary -- v1beta1IngressListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1IngressListItems :: [V1beta1Ingress]
      <*> arbitrary -- v1beta1IngressListKind :: Maybe Text
      <*> arbitrary -- v1beta1IngressListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta1IngressRule where
  arbitrary =
    V1beta1IngressRule
      <$> arbitrary -- v1beta1IngressRuleHost :: Maybe Text
      <*> arbitrary -- v1beta1IngressRuleHttp :: Maybe V1beta1HTTPIngressRuleValue
    
instance Arbitrary V1beta1IngressSpec where
  arbitrary =
    V1beta1IngressSpec
      <$> arbitrary -- v1beta1IngressSpecBackend :: Maybe V1beta1IngressBackend
      <*> arbitrary -- v1beta1IngressSpecRules :: Maybe [V1beta1IngressRule]
      <*> arbitrary -- v1beta1IngressSpecTls :: Maybe [V1beta1IngressTLS]
    
instance Arbitrary V1beta1IngressStatus where
  arbitrary =
    V1beta1IngressStatus
      <$> arbitrary -- v1beta1IngressStatusLoadBalancer :: Maybe V1LoadBalancerStatus
    
instance Arbitrary V1beta1IngressTLS where
  arbitrary =
    V1beta1IngressTLS
      <$> arbitrary -- v1beta1IngressTLSHosts :: Maybe [Text]
      <*> arbitrary -- v1beta1IngressTLSSecretName :: Maybe Text
    
instance Arbitrary V1beta1JSON where
  arbitrary =
    V1beta1JSON
      <$> arbitrary -- v1beta1JSONRaw :: ByteArray
    
instance Arbitrary V1beta1JSONSchemaProps where
  arbitrary =
    V1beta1JSONSchemaProps
      <$> arbitrary -- v1beta1JSONSchemaPropsRef :: Maybe Text
      <*> arbitrary -- v1beta1JSONSchemaPropsSchema :: Maybe Text
      <*> arbitrary -- v1beta1JSONSchemaPropsAdditionalItems :: Maybe V1beta1JSONSchemaPropsOrBool
      <*> arbitrary -- v1beta1JSONSchemaPropsAdditionalProperties :: Maybe V1beta1JSONSchemaPropsOrBool
      <*> arbitrary -- v1beta1JSONSchemaPropsAllOf :: Maybe [V1beta1JSONSchemaProps]
      <*> arbitrary -- v1beta1JSONSchemaPropsAnyOf :: Maybe [V1beta1JSONSchemaProps]
      <*> arbitrary -- v1beta1JSONSchemaPropsDefault :: Maybe V1beta1JSON
      <*> arbitrary -- v1beta1JSONSchemaPropsDefinitions :: Maybe (Map.Map String V1beta1JSONSchemaProps)
      <*> arbitrary -- v1beta1JSONSchemaPropsDependencies :: Maybe (Map.Map String V1beta1JSONSchemaPropsOrStringArray)
      <*> arbitrary -- v1beta1JSONSchemaPropsDescription :: Maybe Text
      <*> arbitrary -- v1beta1JSONSchemaPropsEnum :: Maybe [V1beta1JSON]
      <*> arbitrary -- v1beta1JSONSchemaPropsExample :: Maybe V1beta1JSON
      <*> arbitrary -- v1beta1JSONSchemaPropsExclusiveMaximum :: Maybe Bool
      <*> arbitrary -- v1beta1JSONSchemaPropsExclusiveMinimum :: Maybe Bool
      <*> arbitrary -- v1beta1JSONSchemaPropsExternalDocs :: Maybe V1beta1ExternalDocumentation
      <*> arbitrary -- v1beta1JSONSchemaPropsFormat :: Maybe Text
      <*> arbitrary -- v1beta1JSONSchemaPropsId :: Maybe Text
      <*> arbitrary -- v1beta1JSONSchemaPropsItems :: Maybe V1beta1JSONSchemaPropsOrArray
      <*> arbitrary -- v1beta1JSONSchemaPropsMaxItems :: Maybe Integer
      <*> arbitrary -- v1beta1JSONSchemaPropsMaxLength :: Maybe Integer
      <*> arbitrary -- v1beta1JSONSchemaPropsMaxProperties :: Maybe Integer
      <*> arbitrary -- v1beta1JSONSchemaPropsMaximum :: Maybe Double
      <*> arbitrary -- v1beta1JSONSchemaPropsMinItems :: Maybe Integer
      <*> arbitrary -- v1beta1JSONSchemaPropsMinLength :: Maybe Integer
      <*> arbitrary -- v1beta1JSONSchemaPropsMinProperties :: Maybe Integer
      <*> arbitrary -- v1beta1JSONSchemaPropsMinimum :: Maybe Double
      <*> arbitrary -- v1beta1JSONSchemaPropsMultipleOf :: Maybe Double
      <*> arbitrary -- v1beta1JSONSchemaPropsNot :: Maybe V1beta1JSONSchemaProps
      <*> arbitrary -- v1beta1JSONSchemaPropsOneOf :: Maybe [V1beta1JSONSchemaProps]
      <*> arbitrary -- v1beta1JSONSchemaPropsPattern :: Maybe Text
      <*> arbitrary -- v1beta1JSONSchemaPropsPatternProperties :: Maybe (Map.Map String V1beta1JSONSchemaProps)
      <*> arbitrary -- v1beta1JSONSchemaPropsProperties :: Maybe (Map.Map String V1beta1JSONSchemaProps)
      <*> arbitrary -- v1beta1JSONSchemaPropsRequired :: Maybe [Text]
      <*> arbitrary -- v1beta1JSONSchemaPropsTitle :: Maybe Text
      <*> arbitrary -- v1beta1JSONSchemaPropsType :: Maybe Text
      <*> arbitrary -- v1beta1JSONSchemaPropsUniqueItems :: Maybe Bool
    
instance Arbitrary V1beta1JSONSchemaPropsOrArray where
  arbitrary =
    V1beta1JSONSchemaPropsOrArray
      <$> arbitrary -- v1beta1JSONSchemaPropsOrArrayJsonSchemas :: [V1beta1JSONSchemaProps]
      <*> arbitrary -- v1beta1JSONSchemaPropsOrArraySchema :: V1beta1JSONSchemaProps
    
instance Arbitrary V1beta1JSONSchemaPropsOrBool where
  arbitrary =
    V1beta1JSONSchemaPropsOrBool
      <$> arbitrary -- v1beta1JSONSchemaPropsOrBoolAllows :: Bool
      <*> arbitrary -- v1beta1JSONSchemaPropsOrBoolSchema :: V1beta1JSONSchemaProps
    
instance Arbitrary V1beta1JSONSchemaPropsOrStringArray where
  arbitrary =
    V1beta1JSONSchemaPropsOrStringArray
      <$> arbitrary -- v1beta1JSONSchemaPropsOrStringArrayProperty :: [Text]
      <*> arbitrary -- v1beta1JSONSchemaPropsOrStringArraySchema :: V1beta1JSONSchemaProps
    
instance Arbitrary V1beta1JobTemplateSpec where
  arbitrary =
    V1beta1JobTemplateSpec
      <$> arbitrary -- v1beta1JobTemplateSpecMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1JobTemplateSpecSpec :: Maybe V1JobSpec
    
instance Arbitrary V1beta1LocalSubjectAccessReview where
  arbitrary =
    V1beta1LocalSubjectAccessReview
      <$> arbitrary -- v1beta1LocalSubjectAccessReviewApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1LocalSubjectAccessReviewKind :: Maybe Text
      <*> arbitrary -- v1beta1LocalSubjectAccessReviewMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1LocalSubjectAccessReviewSpec :: V1beta1SubjectAccessReviewSpec
      <*> arbitrary -- v1beta1LocalSubjectAccessReviewStatus :: Maybe V1beta1SubjectAccessReviewStatus
    
instance Arbitrary V1beta1MutatingWebhookConfiguration where
  arbitrary =
    V1beta1MutatingWebhookConfiguration
      <$> arbitrary -- v1beta1MutatingWebhookConfigurationApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1MutatingWebhookConfigurationKind :: Maybe Text
      <*> arbitrary -- v1beta1MutatingWebhookConfigurationMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1MutatingWebhookConfigurationWebhooks :: Maybe [V1beta1Webhook]
    
instance Arbitrary V1beta1MutatingWebhookConfigurationList where
  arbitrary =
    V1beta1MutatingWebhookConfigurationList
      <$> arbitrary -- v1beta1MutatingWebhookConfigurationListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1MutatingWebhookConfigurationListItems :: [V1beta1MutatingWebhookConfiguration]
      <*> arbitrary -- v1beta1MutatingWebhookConfigurationListKind :: Maybe Text
      <*> arbitrary -- v1beta1MutatingWebhookConfigurationListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta1NetworkPolicy where
  arbitrary =
    V1beta1NetworkPolicy
      <$> arbitrary -- v1beta1NetworkPolicyApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1NetworkPolicyKind :: Maybe Text
      <*> arbitrary -- v1beta1NetworkPolicyMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1NetworkPolicySpec :: Maybe V1beta1NetworkPolicySpec
    
instance Arbitrary V1beta1NetworkPolicyEgressRule where
  arbitrary =
    V1beta1NetworkPolicyEgressRule
      <$> arbitrary -- v1beta1NetworkPolicyEgressRulePorts :: Maybe [V1beta1NetworkPolicyPort]
      <*> arbitrary -- v1beta1NetworkPolicyEgressRuleTo :: Maybe [V1beta1NetworkPolicyPeer]
    
instance Arbitrary V1beta1NetworkPolicyIngressRule where
  arbitrary =
    V1beta1NetworkPolicyIngressRule
      <$> arbitrary -- v1beta1NetworkPolicyIngressRuleFrom :: Maybe [V1beta1NetworkPolicyPeer]
      <*> arbitrary -- v1beta1NetworkPolicyIngressRulePorts :: Maybe [V1beta1NetworkPolicyPort]
    
instance Arbitrary V1beta1NetworkPolicyList where
  arbitrary =
    V1beta1NetworkPolicyList
      <$> arbitrary -- v1beta1NetworkPolicyListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1NetworkPolicyListItems :: [V1beta1NetworkPolicy]
      <*> arbitrary -- v1beta1NetworkPolicyListKind :: Maybe Text
      <*> arbitrary -- v1beta1NetworkPolicyListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta1NetworkPolicyPeer where
  arbitrary =
    V1beta1NetworkPolicyPeer
      <$> arbitrary -- v1beta1NetworkPolicyPeerIpBlock :: Maybe V1beta1IPBlock
      <*> arbitrary -- v1beta1NetworkPolicyPeerNamespaceSelector :: Maybe V1LabelSelector
      <*> arbitrary -- v1beta1NetworkPolicyPeerPodSelector :: Maybe V1LabelSelector
    
instance Arbitrary V1beta1NetworkPolicyPort where
  arbitrary =
    V1beta1NetworkPolicyPort
      <$> arbitrary -- v1beta1NetworkPolicyPortPort :: Maybe A.Value
      <*> arbitrary -- v1beta1NetworkPolicyPortProtocol :: Maybe Text
    
instance Arbitrary V1beta1NetworkPolicySpec where
  arbitrary =
    V1beta1NetworkPolicySpec
      <$> arbitrary -- v1beta1NetworkPolicySpecEgress :: Maybe [V1beta1NetworkPolicyEgressRule]
      <*> arbitrary -- v1beta1NetworkPolicySpecIngress :: Maybe [V1beta1NetworkPolicyIngressRule]
      <*> arbitrary -- v1beta1NetworkPolicySpecPodSelector :: V1LabelSelector
      <*> arbitrary -- v1beta1NetworkPolicySpecPolicyTypes :: Maybe [Text]
    
instance Arbitrary V1beta1NonResourceAttributes where
  arbitrary =
    V1beta1NonResourceAttributes
      <$> arbitrary -- v1beta1NonResourceAttributesPath :: Maybe Text
      <*> arbitrary -- v1beta1NonResourceAttributesVerb :: Maybe Text
    
instance Arbitrary V1beta1NonResourceRule where
  arbitrary =
    V1beta1NonResourceRule
      <$> arbitrary -- v1beta1NonResourceRuleNonResourceUrLs :: Maybe [Text]
      <*> arbitrary -- v1beta1NonResourceRuleVerbs :: [Text]
    
instance Arbitrary V1beta1PodDisruptionBudget where
  arbitrary =
    V1beta1PodDisruptionBudget
      <$> arbitrary -- v1beta1PodDisruptionBudgetApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1PodDisruptionBudgetKind :: Maybe Text
      <*> arbitrary -- v1beta1PodDisruptionBudgetMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1PodDisruptionBudgetSpec :: Maybe V1beta1PodDisruptionBudgetSpec
      <*> arbitrary -- v1beta1PodDisruptionBudgetStatus :: Maybe V1beta1PodDisruptionBudgetStatus
    
instance Arbitrary V1beta1PodDisruptionBudgetList where
  arbitrary =
    V1beta1PodDisruptionBudgetList
      <$> arbitrary -- v1beta1PodDisruptionBudgetListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1PodDisruptionBudgetListItems :: [V1beta1PodDisruptionBudget]
      <*> arbitrary -- v1beta1PodDisruptionBudgetListKind :: Maybe Text
      <*> arbitrary -- v1beta1PodDisruptionBudgetListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta1PodDisruptionBudgetSpec where
  arbitrary =
    V1beta1PodDisruptionBudgetSpec
      <$> arbitrary -- v1beta1PodDisruptionBudgetSpecMaxUnavailable :: Maybe A.Value
      <*> arbitrary -- v1beta1PodDisruptionBudgetSpecMinAvailable :: Maybe A.Value
      <*> arbitrary -- v1beta1PodDisruptionBudgetSpecSelector :: Maybe V1LabelSelector
    
instance Arbitrary V1beta1PodDisruptionBudgetStatus where
  arbitrary =
    V1beta1PodDisruptionBudgetStatus
      <$> arbitrary -- v1beta1PodDisruptionBudgetStatusCurrentHealthy :: Int
      <*> arbitrary -- v1beta1PodDisruptionBudgetStatusDesiredHealthy :: Int
      <*> arbitrary -- v1beta1PodDisruptionBudgetStatusDisruptedPods :: (Map.Map String DateTime)
      <*> arbitrary -- v1beta1PodDisruptionBudgetStatusDisruptionsAllowed :: Int
      <*> arbitrary -- v1beta1PodDisruptionBudgetStatusExpectedPods :: Int
      <*> arbitrary -- v1beta1PodDisruptionBudgetStatusObservedGeneration :: Maybe Integer
    
instance Arbitrary V1beta1PodSecurityPolicy where
  arbitrary =
    V1beta1PodSecurityPolicy
      <$> arbitrary -- v1beta1PodSecurityPolicyApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1PodSecurityPolicyKind :: Maybe Text
      <*> arbitrary -- v1beta1PodSecurityPolicyMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1PodSecurityPolicySpec :: Maybe V1beta1PodSecurityPolicySpec
    
instance Arbitrary V1beta1PodSecurityPolicyList where
  arbitrary =
    V1beta1PodSecurityPolicyList
      <$> arbitrary -- v1beta1PodSecurityPolicyListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1PodSecurityPolicyListItems :: [V1beta1PodSecurityPolicy]
      <*> arbitrary -- v1beta1PodSecurityPolicyListKind :: Maybe Text
      <*> arbitrary -- v1beta1PodSecurityPolicyListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta1PodSecurityPolicySpec where
  arbitrary =
    V1beta1PodSecurityPolicySpec
      <$> arbitrary -- v1beta1PodSecurityPolicySpecAllowPrivilegeEscalation :: Maybe Bool
      <*> arbitrary -- v1beta1PodSecurityPolicySpecAllowedCapabilities :: Maybe [Text]
      <*> arbitrary -- v1beta1PodSecurityPolicySpecAllowedFlexVolumes :: Maybe [V1beta1AllowedFlexVolume]
      <*> arbitrary -- v1beta1PodSecurityPolicySpecAllowedHostPaths :: Maybe [V1beta1AllowedHostPath]
      <*> arbitrary -- v1beta1PodSecurityPolicySpecDefaultAddCapabilities :: Maybe [Text]
      <*> arbitrary -- v1beta1PodSecurityPolicySpecDefaultAllowPrivilegeEscalation :: Maybe Bool
      <*> arbitrary -- v1beta1PodSecurityPolicySpecFsGroup :: V1beta1FSGroupStrategyOptions
      <*> arbitrary -- v1beta1PodSecurityPolicySpecHostIpc :: Maybe Bool
      <*> arbitrary -- v1beta1PodSecurityPolicySpecHostNetwork :: Maybe Bool
      <*> arbitrary -- v1beta1PodSecurityPolicySpecHostPid :: Maybe Bool
      <*> arbitrary -- v1beta1PodSecurityPolicySpecHostPorts :: Maybe [V1beta1HostPortRange]
      <*> arbitrary -- v1beta1PodSecurityPolicySpecPrivileged :: Maybe Bool
      <*> arbitrary -- v1beta1PodSecurityPolicySpecReadOnlyRootFilesystem :: Maybe Bool
      <*> arbitrary -- v1beta1PodSecurityPolicySpecRequiredDropCapabilities :: Maybe [Text]
      <*> arbitrary -- v1beta1PodSecurityPolicySpecRunAsUser :: V1beta1RunAsUserStrategyOptions
      <*> arbitrary -- v1beta1PodSecurityPolicySpecSeLinux :: V1beta1SELinuxStrategyOptions
      <*> arbitrary -- v1beta1PodSecurityPolicySpecSupplementalGroups :: V1beta1SupplementalGroupsStrategyOptions
      <*> arbitrary -- v1beta1PodSecurityPolicySpecVolumes :: Maybe [Text]
    
instance Arbitrary V1beta1PolicyRule where
  arbitrary =
    V1beta1PolicyRule
      <$> arbitrary -- v1beta1PolicyRuleApiGroups :: Maybe [Text]
      <*> arbitrary -- v1beta1PolicyRuleNonResourceUrLs :: Maybe [Text]
      <*> arbitrary -- v1beta1PolicyRuleResourceNames :: Maybe [Text]
      <*> arbitrary -- v1beta1PolicyRuleResources :: Maybe [Text]
      <*> arbitrary -- v1beta1PolicyRuleVerbs :: [Text]
    
instance Arbitrary V1beta1ReplicaSet where
  arbitrary =
    V1beta1ReplicaSet
      <$> arbitrary -- v1beta1ReplicaSetApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1ReplicaSetKind :: Maybe Text
      <*> arbitrary -- v1beta1ReplicaSetMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1ReplicaSetSpec :: Maybe V1beta1ReplicaSetSpec
      <*> arbitrary -- v1beta1ReplicaSetStatus :: Maybe V1beta1ReplicaSetStatus
    
instance Arbitrary V1beta1ReplicaSetCondition where
  arbitrary =
    V1beta1ReplicaSetCondition
      <$> arbitrary -- v1beta1ReplicaSetConditionLastTransitionTime :: Maybe DateTime
      <*> arbitrary -- v1beta1ReplicaSetConditionMessage :: Maybe Text
      <*> arbitrary -- v1beta1ReplicaSetConditionReason :: Maybe Text
      <*> arbitrary -- v1beta1ReplicaSetConditionStatus :: Text
      <*> arbitrary -- v1beta1ReplicaSetConditionType :: Text
    
instance Arbitrary V1beta1ReplicaSetList where
  arbitrary =
    V1beta1ReplicaSetList
      <$> arbitrary -- v1beta1ReplicaSetListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1ReplicaSetListItems :: [V1beta1ReplicaSet]
      <*> arbitrary -- v1beta1ReplicaSetListKind :: Maybe Text
      <*> arbitrary -- v1beta1ReplicaSetListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta1ReplicaSetSpec where
  arbitrary =
    V1beta1ReplicaSetSpec
      <$> arbitrary -- v1beta1ReplicaSetSpecMinReadySeconds :: Maybe Int
      <*> arbitrary -- v1beta1ReplicaSetSpecReplicas :: Maybe Int
      <*> arbitrary -- v1beta1ReplicaSetSpecSelector :: Maybe V1LabelSelector
      <*> arbitrary -- v1beta1ReplicaSetSpecTemplate :: Maybe V1PodTemplateSpec
    
instance Arbitrary V1beta1ReplicaSetStatus where
  arbitrary =
    V1beta1ReplicaSetStatus
      <$> arbitrary -- v1beta1ReplicaSetStatusAvailableReplicas :: Maybe Int
      <*> arbitrary -- v1beta1ReplicaSetStatusConditions :: Maybe [V1beta1ReplicaSetCondition]
      <*> arbitrary -- v1beta1ReplicaSetStatusFullyLabeledReplicas :: Maybe Int
      <*> arbitrary -- v1beta1ReplicaSetStatusObservedGeneration :: Maybe Integer
      <*> arbitrary -- v1beta1ReplicaSetStatusReadyReplicas :: Maybe Int
      <*> arbitrary -- v1beta1ReplicaSetStatusReplicas :: Int
    
instance Arbitrary V1beta1ResourceAttributes where
  arbitrary =
    V1beta1ResourceAttributes
      <$> arbitrary -- v1beta1ResourceAttributesGroup :: Maybe Text
      <*> arbitrary -- v1beta1ResourceAttributesName :: Maybe Text
      <*> arbitrary -- v1beta1ResourceAttributesNamespace :: Maybe Text
      <*> arbitrary -- v1beta1ResourceAttributesResource :: Maybe Text
      <*> arbitrary -- v1beta1ResourceAttributesSubresource :: Maybe Text
      <*> arbitrary -- v1beta1ResourceAttributesVerb :: Maybe Text
      <*> arbitrary -- v1beta1ResourceAttributesVersion :: Maybe Text
    
instance Arbitrary V1beta1ResourceRule where
  arbitrary =
    V1beta1ResourceRule
      <$> arbitrary -- v1beta1ResourceRuleApiGroups :: Maybe [Text]
      <*> arbitrary -- v1beta1ResourceRuleResourceNames :: Maybe [Text]
      <*> arbitrary -- v1beta1ResourceRuleResources :: Maybe [Text]
      <*> arbitrary -- v1beta1ResourceRuleVerbs :: [Text]
    
instance Arbitrary V1beta1Role where
  arbitrary =
    V1beta1Role
      <$> arbitrary -- v1beta1RoleApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1RoleKind :: Maybe Text
      <*> arbitrary -- v1beta1RoleMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1RoleRules :: [V1beta1PolicyRule]
    
instance Arbitrary V1beta1RoleBinding where
  arbitrary =
    V1beta1RoleBinding
      <$> arbitrary -- v1beta1RoleBindingApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1RoleBindingKind :: Maybe Text
      <*> arbitrary -- v1beta1RoleBindingMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1RoleBindingRoleRef :: V1beta1RoleRef
      <*> arbitrary -- v1beta1RoleBindingSubjects :: [V1beta1Subject]
    
instance Arbitrary V1beta1RoleBindingList where
  arbitrary =
    V1beta1RoleBindingList
      <$> arbitrary -- v1beta1RoleBindingListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1RoleBindingListItems :: [V1beta1RoleBinding]
      <*> arbitrary -- v1beta1RoleBindingListKind :: Maybe Text
      <*> arbitrary -- v1beta1RoleBindingListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta1RoleList where
  arbitrary =
    V1beta1RoleList
      <$> arbitrary -- v1beta1RoleListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1RoleListItems :: [V1beta1Role]
      <*> arbitrary -- v1beta1RoleListKind :: Maybe Text
      <*> arbitrary -- v1beta1RoleListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta1RoleRef where
  arbitrary =
    V1beta1RoleRef
      <$> arbitrary -- v1beta1RoleRefApiGroup :: Text
      <*> arbitrary -- v1beta1RoleRefKind :: Text
      <*> arbitrary -- v1beta1RoleRefName :: Text
    
instance Arbitrary V1beta1RollingUpdateDaemonSet where
  arbitrary =
    V1beta1RollingUpdateDaemonSet
      <$> arbitrary -- v1beta1RollingUpdateDaemonSetMaxUnavailable :: Maybe A.Value
    
instance Arbitrary V1beta1RollingUpdateStatefulSetStrategy where
  arbitrary =
    V1beta1RollingUpdateStatefulSetStrategy
      <$> arbitrary -- v1beta1RollingUpdateStatefulSetStrategyPartition :: Maybe Int
    
instance Arbitrary V1beta1RuleWithOperations where
  arbitrary =
    V1beta1RuleWithOperations
      <$> arbitrary -- v1beta1RuleWithOperationsApiGroups :: Maybe [Text]
      <*> arbitrary -- v1beta1RuleWithOperationsApiVersions :: Maybe [Text]
      <*> arbitrary -- v1beta1RuleWithOperationsOperations :: Maybe [Text]
      <*> arbitrary -- v1beta1RuleWithOperationsResources :: Maybe [Text]
    
instance Arbitrary V1beta1RunAsUserStrategyOptions where
  arbitrary =
    V1beta1RunAsUserStrategyOptions
      <$> arbitrary -- v1beta1RunAsUserStrategyOptionsRanges :: Maybe [V1beta1IDRange]
      <*> arbitrary -- v1beta1RunAsUserStrategyOptionsRule :: Text
    
instance Arbitrary V1beta1SELinuxStrategyOptions where
  arbitrary =
    V1beta1SELinuxStrategyOptions
      <$> arbitrary -- v1beta1SELinuxStrategyOptionsRule :: Text
      <*> arbitrary -- v1beta1SELinuxStrategyOptionsSeLinuxOptions :: Maybe V1SELinuxOptions
    
instance Arbitrary V1beta1SelfSubjectAccessReview where
  arbitrary =
    V1beta1SelfSubjectAccessReview
      <$> arbitrary -- v1beta1SelfSubjectAccessReviewApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1SelfSubjectAccessReviewKind :: Maybe Text
      <*> arbitrary -- v1beta1SelfSubjectAccessReviewMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1SelfSubjectAccessReviewSpec :: V1beta1SelfSubjectAccessReviewSpec
      <*> arbitrary -- v1beta1SelfSubjectAccessReviewStatus :: Maybe V1beta1SubjectAccessReviewStatus
    
instance Arbitrary V1beta1SelfSubjectAccessReviewSpec where
  arbitrary =
    V1beta1SelfSubjectAccessReviewSpec
      <$> arbitrary -- v1beta1SelfSubjectAccessReviewSpecNonResourceAttributes :: Maybe V1beta1NonResourceAttributes
      <*> arbitrary -- v1beta1SelfSubjectAccessReviewSpecResourceAttributes :: Maybe V1beta1ResourceAttributes
    
instance Arbitrary V1beta1SelfSubjectRulesReview where
  arbitrary =
    V1beta1SelfSubjectRulesReview
      <$> arbitrary -- v1beta1SelfSubjectRulesReviewApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1SelfSubjectRulesReviewKind :: Maybe Text
      <*> arbitrary -- v1beta1SelfSubjectRulesReviewMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1SelfSubjectRulesReviewSpec :: V1beta1SelfSubjectRulesReviewSpec
      <*> arbitrary -- v1beta1SelfSubjectRulesReviewStatus :: Maybe V1beta1SubjectRulesReviewStatus
    
instance Arbitrary V1beta1SelfSubjectRulesReviewSpec where
  arbitrary =
    V1beta1SelfSubjectRulesReviewSpec
      <$> arbitrary -- v1beta1SelfSubjectRulesReviewSpecNamespace :: Maybe Text
    
instance Arbitrary V1beta1StatefulSet where
  arbitrary =
    V1beta1StatefulSet
      <$> arbitrary -- v1beta1StatefulSetApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1StatefulSetKind :: Maybe Text
      <*> arbitrary -- v1beta1StatefulSetMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1StatefulSetSpec :: Maybe V1beta1StatefulSetSpec
      <*> arbitrary -- v1beta1StatefulSetStatus :: Maybe V1beta1StatefulSetStatus
    
instance Arbitrary V1beta1StatefulSetCondition where
  arbitrary =
    V1beta1StatefulSetCondition
      <$> arbitrary -- v1beta1StatefulSetConditionLastTransitionTime :: Maybe DateTime
      <*> arbitrary -- v1beta1StatefulSetConditionMessage :: Maybe Text
      <*> arbitrary -- v1beta1StatefulSetConditionReason :: Maybe Text
      <*> arbitrary -- v1beta1StatefulSetConditionStatus :: Text
      <*> arbitrary -- v1beta1StatefulSetConditionType :: Text
    
instance Arbitrary V1beta1StatefulSetList where
  arbitrary =
    V1beta1StatefulSetList
      <$> arbitrary -- v1beta1StatefulSetListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1StatefulSetListItems :: [V1beta1StatefulSet]
      <*> arbitrary -- v1beta1StatefulSetListKind :: Maybe Text
      <*> arbitrary -- v1beta1StatefulSetListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta1StatefulSetSpec where
  arbitrary =
    V1beta1StatefulSetSpec
      <$> arbitrary -- v1beta1StatefulSetSpecPodManagementPolicy :: Maybe Text
      <*> arbitrary -- v1beta1StatefulSetSpecReplicas :: Maybe Int
      <*> arbitrary -- v1beta1StatefulSetSpecRevisionHistoryLimit :: Maybe Int
      <*> arbitrary -- v1beta1StatefulSetSpecSelector :: Maybe V1LabelSelector
      <*> arbitrary -- v1beta1StatefulSetSpecServiceName :: Text
      <*> arbitrary -- v1beta1StatefulSetSpecTemplate :: V1PodTemplateSpec
      <*> arbitrary -- v1beta1StatefulSetSpecUpdateStrategy :: Maybe V1beta1StatefulSetUpdateStrategy
      <*> arbitrary -- v1beta1StatefulSetSpecVolumeClaimTemplates :: Maybe [V1PersistentVolumeClaim]
    
instance Arbitrary V1beta1StatefulSetStatus where
  arbitrary =
    V1beta1StatefulSetStatus
      <$> arbitrary -- v1beta1StatefulSetStatusCollisionCount :: Maybe Int
      <*> arbitrary -- v1beta1StatefulSetStatusConditions :: Maybe [V1beta1StatefulSetCondition]
      <*> arbitrary -- v1beta1StatefulSetStatusCurrentReplicas :: Maybe Int
      <*> arbitrary -- v1beta1StatefulSetStatusCurrentRevision :: Maybe Text
      <*> arbitrary -- v1beta1StatefulSetStatusObservedGeneration :: Maybe Integer
      <*> arbitrary -- v1beta1StatefulSetStatusReadyReplicas :: Maybe Int
      <*> arbitrary -- v1beta1StatefulSetStatusReplicas :: Int
      <*> arbitrary -- v1beta1StatefulSetStatusUpdateRevision :: Maybe Text
      <*> arbitrary -- v1beta1StatefulSetStatusUpdatedReplicas :: Maybe Int
    
instance Arbitrary V1beta1StatefulSetUpdateStrategy where
  arbitrary =
    V1beta1StatefulSetUpdateStrategy
      <$> arbitrary -- v1beta1StatefulSetUpdateStrategyRollingUpdate :: Maybe V1beta1RollingUpdateStatefulSetStrategy
      <*> arbitrary -- v1beta1StatefulSetUpdateStrategyType :: Maybe Text
    
instance Arbitrary V1beta1StorageClass where
  arbitrary =
    V1beta1StorageClass
      <$> arbitrary -- v1beta1StorageClassAllowVolumeExpansion :: Maybe Bool
      <*> arbitrary -- v1beta1StorageClassApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1StorageClassKind :: Maybe Text
      <*> arbitrary -- v1beta1StorageClassMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1StorageClassMountOptions :: Maybe [Text]
      <*> arbitrary -- v1beta1StorageClassParameters :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1beta1StorageClassProvisioner :: Text
      <*> arbitrary -- v1beta1StorageClassReclaimPolicy :: Maybe Text
      <*> arbitrary -- v1beta1StorageClassVolumeBindingMode :: Maybe Text
    
instance Arbitrary V1beta1StorageClassList where
  arbitrary =
    V1beta1StorageClassList
      <$> arbitrary -- v1beta1StorageClassListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1StorageClassListItems :: [V1beta1StorageClass]
      <*> arbitrary -- v1beta1StorageClassListKind :: Maybe Text
      <*> arbitrary -- v1beta1StorageClassListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta1Subject where
  arbitrary =
    V1beta1Subject
      <$> arbitrary -- v1beta1SubjectApiGroup :: Maybe Text
      <*> arbitrary -- v1beta1SubjectKind :: Text
      <*> arbitrary -- v1beta1SubjectName :: Text
      <*> arbitrary -- v1beta1SubjectNamespace :: Maybe Text
    
instance Arbitrary V1beta1SubjectAccessReview where
  arbitrary =
    V1beta1SubjectAccessReview
      <$> arbitrary -- v1beta1SubjectAccessReviewApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1SubjectAccessReviewKind :: Maybe Text
      <*> arbitrary -- v1beta1SubjectAccessReviewMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1SubjectAccessReviewSpec :: V1beta1SubjectAccessReviewSpec
      <*> arbitrary -- v1beta1SubjectAccessReviewStatus :: Maybe V1beta1SubjectAccessReviewStatus
    
instance Arbitrary V1beta1SubjectAccessReviewSpec where
  arbitrary =
    V1beta1SubjectAccessReviewSpec
      <$> arbitrary -- v1beta1SubjectAccessReviewSpecExtra :: Maybe (Map.Map String [Text])
      <*> arbitrary -- v1beta1SubjectAccessReviewSpecGroup :: Maybe [Text]
      <*> arbitrary -- v1beta1SubjectAccessReviewSpecNonResourceAttributes :: Maybe V1beta1NonResourceAttributes
      <*> arbitrary -- v1beta1SubjectAccessReviewSpecResourceAttributes :: Maybe V1beta1ResourceAttributes
      <*> arbitrary -- v1beta1SubjectAccessReviewSpecUid :: Maybe Text
      <*> arbitrary -- v1beta1SubjectAccessReviewSpecUser :: Maybe Text
    
instance Arbitrary V1beta1SubjectAccessReviewStatus where
  arbitrary =
    V1beta1SubjectAccessReviewStatus
      <$> arbitrary -- v1beta1SubjectAccessReviewStatusAllowed :: Bool
      <*> arbitrary -- v1beta1SubjectAccessReviewStatusDenied :: Maybe Bool
      <*> arbitrary -- v1beta1SubjectAccessReviewStatusEvaluationError :: Maybe Text
      <*> arbitrary -- v1beta1SubjectAccessReviewStatusReason :: Maybe Text
    
instance Arbitrary V1beta1SubjectRulesReviewStatus where
  arbitrary =
    V1beta1SubjectRulesReviewStatus
      <$> arbitrary -- v1beta1SubjectRulesReviewStatusEvaluationError :: Maybe Text
      <*> arbitrary -- v1beta1SubjectRulesReviewStatusIncomplete :: Bool
      <*> arbitrary -- v1beta1SubjectRulesReviewStatusNonResourceRules :: [V1beta1NonResourceRule]
      <*> arbitrary -- v1beta1SubjectRulesReviewStatusResourceRules :: [V1beta1ResourceRule]
    
instance Arbitrary V1beta1SupplementalGroupsStrategyOptions where
  arbitrary =
    V1beta1SupplementalGroupsStrategyOptions
      <$> arbitrary -- v1beta1SupplementalGroupsStrategyOptionsRanges :: Maybe [V1beta1IDRange]
      <*> arbitrary -- v1beta1SupplementalGroupsStrategyOptionsRule :: Maybe Text
    
instance Arbitrary V1beta1TokenReview where
  arbitrary =
    V1beta1TokenReview
      <$> arbitrary -- v1beta1TokenReviewApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1TokenReviewKind :: Maybe Text
      <*> arbitrary -- v1beta1TokenReviewMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1TokenReviewSpec :: V1beta1TokenReviewSpec
      <*> arbitrary -- v1beta1TokenReviewStatus :: Maybe V1beta1TokenReviewStatus
    
instance Arbitrary V1beta1TokenReviewSpec where
  arbitrary =
    V1beta1TokenReviewSpec
      <$> arbitrary -- v1beta1TokenReviewSpecToken :: Maybe Text
    
instance Arbitrary V1beta1TokenReviewStatus where
  arbitrary =
    V1beta1TokenReviewStatus
      <$> arbitrary -- v1beta1TokenReviewStatusAuthenticated :: Maybe Bool
      <*> arbitrary -- v1beta1TokenReviewStatusError :: Maybe Text
      <*> arbitrary -- v1beta1TokenReviewStatusUser :: Maybe V1beta1UserInfo
    
instance Arbitrary V1beta1UserInfo where
  arbitrary =
    V1beta1UserInfo
      <$> arbitrary -- v1beta1UserInfoExtra :: Maybe (Map.Map String [Text])
      <*> arbitrary -- v1beta1UserInfoGroups :: Maybe [Text]
      <*> arbitrary -- v1beta1UserInfoUid :: Maybe Text
      <*> arbitrary -- v1beta1UserInfoUsername :: Maybe Text
    
instance Arbitrary V1beta1ValidatingWebhookConfiguration where
  arbitrary =
    V1beta1ValidatingWebhookConfiguration
      <$> arbitrary -- v1beta1ValidatingWebhookConfigurationApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1ValidatingWebhookConfigurationKind :: Maybe Text
      <*> arbitrary -- v1beta1ValidatingWebhookConfigurationMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta1ValidatingWebhookConfigurationWebhooks :: Maybe [V1beta1Webhook]
    
instance Arbitrary V1beta1ValidatingWebhookConfigurationList where
  arbitrary =
    V1beta1ValidatingWebhookConfigurationList
      <$> arbitrary -- v1beta1ValidatingWebhookConfigurationListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta1ValidatingWebhookConfigurationListItems :: [V1beta1ValidatingWebhookConfiguration]
      <*> arbitrary -- v1beta1ValidatingWebhookConfigurationListKind :: Maybe Text
      <*> arbitrary -- v1beta1ValidatingWebhookConfigurationListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta1Webhook where
  arbitrary =
    V1beta1Webhook
      <$> arbitrary -- v1beta1WebhookClientConfig :: V1beta1WebhookClientConfig
      <*> arbitrary -- v1beta1WebhookFailurePolicy :: Maybe Text
      <*> arbitrary -- v1beta1WebhookName :: Text
      <*> arbitrary -- v1beta1WebhookNamespaceSelector :: Maybe V1LabelSelector
      <*> arbitrary -- v1beta1WebhookRules :: Maybe [V1beta1RuleWithOperations]
    
instance Arbitrary V1beta1WebhookClientConfig where
  arbitrary =
    V1beta1WebhookClientConfig
      <$> arbitrary -- v1beta1WebhookClientConfigCaBundle :: ByteArray
      <*> arbitrary -- v1beta1WebhookClientConfigService :: Maybe AdmissionregistrationV1beta1ServiceReference
      <*> arbitrary -- v1beta1WebhookClientConfigUrl :: Maybe Text
    
instance Arbitrary V1beta2ControllerRevision where
  arbitrary =
    V1beta2ControllerRevision
      <$> arbitrary -- v1beta2ControllerRevisionApiVersion :: Maybe Text
      <*> arbitrary -- v1beta2ControllerRevisionData :: Maybe RuntimeRawExtension
      <*> arbitrary -- v1beta2ControllerRevisionKind :: Maybe Text
      <*> arbitrary -- v1beta2ControllerRevisionMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta2ControllerRevisionRevision :: Integer
    
instance Arbitrary V1beta2ControllerRevisionList where
  arbitrary =
    V1beta2ControllerRevisionList
      <$> arbitrary -- v1beta2ControllerRevisionListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta2ControllerRevisionListItems :: [V1beta2ControllerRevision]
      <*> arbitrary -- v1beta2ControllerRevisionListKind :: Maybe Text
      <*> arbitrary -- v1beta2ControllerRevisionListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta2DaemonSet where
  arbitrary =
    V1beta2DaemonSet
      <$> arbitrary -- v1beta2DaemonSetApiVersion :: Maybe Text
      <*> arbitrary -- v1beta2DaemonSetKind :: Maybe Text
      <*> arbitrary -- v1beta2DaemonSetMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta2DaemonSetSpec :: Maybe V1beta2DaemonSetSpec
      <*> arbitrary -- v1beta2DaemonSetStatus :: Maybe V1beta2DaemonSetStatus
    
instance Arbitrary V1beta2DaemonSetCondition where
  arbitrary =
    V1beta2DaemonSetCondition
      <$> arbitrary -- v1beta2DaemonSetConditionLastTransitionTime :: Maybe DateTime
      <*> arbitrary -- v1beta2DaemonSetConditionMessage :: Maybe Text
      <*> arbitrary -- v1beta2DaemonSetConditionReason :: Maybe Text
      <*> arbitrary -- v1beta2DaemonSetConditionStatus :: Text
      <*> arbitrary -- v1beta2DaemonSetConditionType :: Text
    
instance Arbitrary V1beta2DaemonSetList where
  arbitrary =
    V1beta2DaemonSetList
      <$> arbitrary -- v1beta2DaemonSetListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta2DaemonSetListItems :: [V1beta2DaemonSet]
      <*> arbitrary -- v1beta2DaemonSetListKind :: Maybe Text
      <*> arbitrary -- v1beta2DaemonSetListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta2DaemonSetSpec where
  arbitrary =
    V1beta2DaemonSetSpec
      <$> arbitrary -- v1beta2DaemonSetSpecMinReadySeconds :: Maybe Int
      <*> arbitrary -- v1beta2DaemonSetSpecRevisionHistoryLimit :: Maybe Int
      <*> arbitrary -- v1beta2DaemonSetSpecSelector :: V1LabelSelector
      <*> arbitrary -- v1beta2DaemonSetSpecTemplate :: V1PodTemplateSpec
      <*> arbitrary -- v1beta2DaemonSetSpecUpdateStrategy :: Maybe V1beta2DaemonSetUpdateStrategy
    
instance Arbitrary V1beta2DaemonSetStatus where
  arbitrary =
    V1beta2DaemonSetStatus
      <$> arbitrary -- v1beta2DaemonSetStatusCollisionCount :: Maybe Int
      <*> arbitrary -- v1beta2DaemonSetStatusConditions :: Maybe [V1beta2DaemonSetCondition]
      <*> arbitrary -- v1beta2DaemonSetStatusCurrentNumberScheduled :: Int
      <*> arbitrary -- v1beta2DaemonSetStatusDesiredNumberScheduled :: Int
      <*> arbitrary -- v1beta2DaemonSetStatusNumberAvailable :: Maybe Int
      <*> arbitrary -- v1beta2DaemonSetStatusNumberMisscheduled :: Int
      <*> arbitrary -- v1beta2DaemonSetStatusNumberReady :: Int
      <*> arbitrary -- v1beta2DaemonSetStatusNumberUnavailable :: Maybe Int
      <*> arbitrary -- v1beta2DaemonSetStatusObservedGeneration :: Maybe Integer
      <*> arbitrary -- v1beta2DaemonSetStatusUpdatedNumberScheduled :: Maybe Int
    
instance Arbitrary V1beta2DaemonSetUpdateStrategy where
  arbitrary =
    V1beta2DaemonSetUpdateStrategy
      <$> arbitrary -- v1beta2DaemonSetUpdateStrategyRollingUpdate :: Maybe V1beta2RollingUpdateDaemonSet
      <*> arbitrary -- v1beta2DaemonSetUpdateStrategyType :: Maybe Text
    
instance Arbitrary V1beta2Deployment where
  arbitrary =
    V1beta2Deployment
      <$> arbitrary -- v1beta2DeploymentApiVersion :: Maybe Text
      <*> arbitrary -- v1beta2DeploymentKind :: Maybe Text
      <*> arbitrary -- v1beta2DeploymentMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta2DeploymentSpec :: Maybe V1beta2DeploymentSpec
      <*> arbitrary -- v1beta2DeploymentStatus :: Maybe V1beta2DeploymentStatus
    
instance Arbitrary V1beta2DeploymentCondition where
  arbitrary =
    V1beta2DeploymentCondition
      <$> arbitrary -- v1beta2DeploymentConditionLastTransitionTime :: Maybe DateTime
      <*> arbitrary -- v1beta2DeploymentConditionLastUpdateTime :: Maybe DateTime
      <*> arbitrary -- v1beta2DeploymentConditionMessage :: Maybe Text
      <*> arbitrary -- v1beta2DeploymentConditionReason :: Maybe Text
      <*> arbitrary -- v1beta2DeploymentConditionStatus :: Text
      <*> arbitrary -- v1beta2DeploymentConditionType :: Text
    
instance Arbitrary V1beta2DeploymentList where
  arbitrary =
    V1beta2DeploymentList
      <$> arbitrary -- v1beta2DeploymentListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta2DeploymentListItems :: [V1beta2Deployment]
      <*> arbitrary -- v1beta2DeploymentListKind :: Maybe Text
      <*> arbitrary -- v1beta2DeploymentListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta2DeploymentSpec where
  arbitrary =
    V1beta2DeploymentSpec
      <$> arbitrary -- v1beta2DeploymentSpecMinReadySeconds :: Maybe Int
      <*> arbitrary -- v1beta2DeploymentSpecPaused :: Maybe Bool
      <*> arbitrary -- v1beta2DeploymentSpecProgressDeadlineSeconds :: Maybe Int
      <*> arbitrary -- v1beta2DeploymentSpecReplicas :: Maybe Int
      <*> arbitrary -- v1beta2DeploymentSpecRevisionHistoryLimit :: Maybe Int
      <*> arbitrary -- v1beta2DeploymentSpecSelector :: V1LabelSelector
      <*> arbitrary -- v1beta2DeploymentSpecStrategy :: Maybe V1beta2DeploymentStrategy
      <*> arbitrary -- v1beta2DeploymentSpecTemplate :: V1PodTemplateSpec
    
instance Arbitrary V1beta2DeploymentStatus where
  arbitrary =
    V1beta2DeploymentStatus
      <$> arbitrary -- v1beta2DeploymentStatusAvailableReplicas :: Maybe Int
      <*> arbitrary -- v1beta2DeploymentStatusCollisionCount :: Maybe Int
      <*> arbitrary -- v1beta2DeploymentStatusConditions :: Maybe [V1beta2DeploymentCondition]
      <*> arbitrary -- v1beta2DeploymentStatusObservedGeneration :: Maybe Integer
      <*> arbitrary -- v1beta2DeploymentStatusReadyReplicas :: Maybe Int
      <*> arbitrary -- v1beta2DeploymentStatusReplicas :: Maybe Int
      <*> arbitrary -- v1beta2DeploymentStatusUnavailableReplicas :: Maybe Int
      <*> arbitrary -- v1beta2DeploymentStatusUpdatedReplicas :: Maybe Int
    
instance Arbitrary V1beta2DeploymentStrategy where
  arbitrary =
    V1beta2DeploymentStrategy
      <$> arbitrary -- v1beta2DeploymentStrategyRollingUpdate :: Maybe V1beta2RollingUpdateDeployment
      <*> arbitrary -- v1beta2DeploymentStrategyType :: Maybe Text
    
instance Arbitrary V1beta2ReplicaSet where
  arbitrary =
    V1beta2ReplicaSet
      <$> arbitrary -- v1beta2ReplicaSetApiVersion :: Maybe Text
      <*> arbitrary -- v1beta2ReplicaSetKind :: Maybe Text
      <*> arbitrary -- v1beta2ReplicaSetMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta2ReplicaSetSpec :: Maybe V1beta2ReplicaSetSpec
      <*> arbitrary -- v1beta2ReplicaSetStatus :: Maybe V1beta2ReplicaSetStatus
    
instance Arbitrary V1beta2ReplicaSetCondition where
  arbitrary =
    V1beta2ReplicaSetCondition
      <$> arbitrary -- v1beta2ReplicaSetConditionLastTransitionTime :: Maybe DateTime
      <*> arbitrary -- v1beta2ReplicaSetConditionMessage :: Maybe Text
      <*> arbitrary -- v1beta2ReplicaSetConditionReason :: Maybe Text
      <*> arbitrary -- v1beta2ReplicaSetConditionStatus :: Text
      <*> arbitrary -- v1beta2ReplicaSetConditionType :: Text
    
instance Arbitrary V1beta2ReplicaSetList where
  arbitrary =
    V1beta2ReplicaSetList
      <$> arbitrary -- v1beta2ReplicaSetListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta2ReplicaSetListItems :: [V1beta2ReplicaSet]
      <*> arbitrary -- v1beta2ReplicaSetListKind :: Maybe Text
      <*> arbitrary -- v1beta2ReplicaSetListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta2ReplicaSetSpec where
  arbitrary =
    V1beta2ReplicaSetSpec
      <$> arbitrary -- v1beta2ReplicaSetSpecMinReadySeconds :: Maybe Int
      <*> arbitrary -- v1beta2ReplicaSetSpecReplicas :: Maybe Int
      <*> arbitrary -- v1beta2ReplicaSetSpecSelector :: V1LabelSelector
      <*> arbitrary -- v1beta2ReplicaSetSpecTemplate :: Maybe V1PodTemplateSpec
    
instance Arbitrary V1beta2ReplicaSetStatus where
  arbitrary =
    V1beta2ReplicaSetStatus
      <$> arbitrary -- v1beta2ReplicaSetStatusAvailableReplicas :: Maybe Int
      <*> arbitrary -- v1beta2ReplicaSetStatusConditions :: Maybe [V1beta2ReplicaSetCondition]
      <*> arbitrary -- v1beta2ReplicaSetStatusFullyLabeledReplicas :: Maybe Int
      <*> arbitrary -- v1beta2ReplicaSetStatusObservedGeneration :: Maybe Integer
      <*> arbitrary -- v1beta2ReplicaSetStatusReadyReplicas :: Maybe Int
      <*> arbitrary -- v1beta2ReplicaSetStatusReplicas :: Int
    
instance Arbitrary V1beta2RollingUpdateDaemonSet where
  arbitrary =
    V1beta2RollingUpdateDaemonSet
      <$> arbitrary -- v1beta2RollingUpdateDaemonSetMaxUnavailable :: Maybe A.Value
    
instance Arbitrary V1beta2RollingUpdateDeployment where
  arbitrary =
    V1beta2RollingUpdateDeployment
      <$> arbitrary -- v1beta2RollingUpdateDeploymentMaxSurge :: Maybe A.Value
      <*> arbitrary -- v1beta2RollingUpdateDeploymentMaxUnavailable :: Maybe A.Value
    
instance Arbitrary V1beta2RollingUpdateStatefulSetStrategy where
  arbitrary =
    V1beta2RollingUpdateStatefulSetStrategy
      <$> arbitrary -- v1beta2RollingUpdateStatefulSetStrategyPartition :: Maybe Int
    
instance Arbitrary V1beta2Scale where
  arbitrary =
    V1beta2Scale
      <$> arbitrary -- v1beta2ScaleApiVersion :: Maybe Text
      <*> arbitrary -- v1beta2ScaleKind :: Maybe Text
      <*> arbitrary -- v1beta2ScaleMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta2ScaleSpec :: Maybe V1beta2ScaleSpec
      <*> arbitrary -- v1beta2ScaleStatus :: Maybe V1beta2ScaleStatus
    
instance Arbitrary V1beta2ScaleSpec where
  arbitrary =
    V1beta2ScaleSpec
      <$> arbitrary -- v1beta2ScaleSpecReplicas :: Maybe Int
    
instance Arbitrary V1beta2ScaleStatus where
  arbitrary =
    V1beta2ScaleStatus
      <$> arbitrary -- v1beta2ScaleStatusReplicas :: Int
      <*> arbitrary -- v1beta2ScaleStatusSelector :: Maybe (Map.Map String Text)
      <*> arbitrary -- v1beta2ScaleStatusTargetSelector :: Maybe Text
    
instance Arbitrary V1beta2StatefulSet where
  arbitrary =
    V1beta2StatefulSet
      <$> arbitrary -- v1beta2StatefulSetApiVersion :: Maybe Text
      <*> arbitrary -- v1beta2StatefulSetKind :: Maybe Text
      <*> arbitrary -- v1beta2StatefulSetMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v1beta2StatefulSetSpec :: Maybe V1beta2StatefulSetSpec
      <*> arbitrary -- v1beta2StatefulSetStatus :: Maybe V1beta2StatefulSetStatus
    
instance Arbitrary V1beta2StatefulSetCondition where
  arbitrary =
    V1beta2StatefulSetCondition
      <$> arbitrary -- v1beta2StatefulSetConditionLastTransitionTime :: Maybe DateTime
      <*> arbitrary -- v1beta2StatefulSetConditionMessage :: Maybe Text
      <*> arbitrary -- v1beta2StatefulSetConditionReason :: Maybe Text
      <*> arbitrary -- v1beta2StatefulSetConditionStatus :: Text
      <*> arbitrary -- v1beta2StatefulSetConditionType :: Text
    
instance Arbitrary V1beta2StatefulSetList where
  arbitrary =
    V1beta2StatefulSetList
      <$> arbitrary -- v1beta2StatefulSetListApiVersion :: Maybe Text
      <*> arbitrary -- v1beta2StatefulSetListItems :: [V1beta2StatefulSet]
      <*> arbitrary -- v1beta2StatefulSetListKind :: Maybe Text
      <*> arbitrary -- v1beta2StatefulSetListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V1beta2StatefulSetSpec where
  arbitrary =
    V1beta2StatefulSetSpec
      <$> arbitrary -- v1beta2StatefulSetSpecPodManagementPolicy :: Maybe Text
      <*> arbitrary -- v1beta2StatefulSetSpecReplicas :: Maybe Int
      <*> arbitrary -- v1beta2StatefulSetSpecRevisionHistoryLimit :: Maybe Int
      <*> arbitrary -- v1beta2StatefulSetSpecSelector :: V1LabelSelector
      <*> arbitrary -- v1beta2StatefulSetSpecServiceName :: Text
      <*> arbitrary -- v1beta2StatefulSetSpecTemplate :: V1PodTemplateSpec
      <*> arbitrary -- v1beta2StatefulSetSpecUpdateStrategy :: Maybe V1beta2StatefulSetUpdateStrategy
      <*> arbitrary -- v1beta2StatefulSetSpecVolumeClaimTemplates :: Maybe [V1PersistentVolumeClaim]
    
instance Arbitrary V1beta2StatefulSetStatus where
  arbitrary =
    V1beta2StatefulSetStatus
      <$> arbitrary -- v1beta2StatefulSetStatusCollisionCount :: Maybe Int
      <*> arbitrary -- v1beta2StatefulSetStatusConditions :: Maybe [V1beta2StatefulSetCondition]
      <*> arbitrary -- v1beta2StatefulSetStatusCurrentReplicas :: Maybe Int
      <*> arbitrary -- v1beta2StatefulSetStatusCurrentRevision :: Maybe Text
      <*> arbitrary -- v1beta2StatefulSetStatusObservedGeneration :: Maybe Integer
      <*> arbitrary -- v1beta2StatefulSetStatusReadyReplicas :: Maybe Int
      <*> arbitrary -- v1beta2StatefulSetStatusReplicas :: Int
      <*> arbitrary -- v1beta2StatefulSetStatusUpdateRevision :: Maybe Text
      <*> arbitrary -- v1beta2StatefulSetStatusUpdatedReplicas :: Maybe Int
    
instance Arbitrary V1beta2StatefulSetUpdateStrategy where
  arbitrary =
    V1beta2StatefulSetUpdateStrategy
      <$> arbitrary -- v1beta2StatefulSetUpdateStrategyRollingUpdate :: Maybe V1beta2RollingUpdateStatefulSetStrategy
      <*> arbitrary -- v1beta2StatefulSetUpdateStrategyType :: Maybe Text
    
instance Arbitrary V2alpha1CronJob where
  arbitrary =
    V2alpha1CronJob
      <$> arbitrary -- v2alpha1CronJobApiVersion :: Maybe Text
      <*> arbitrary -- v2alpha1CronJobKind :: Maybe Text
      <*> arbitrary -- v2alpha1CronJobMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v2alpha1CronJobSpec :: Maybe V2alpha1CronJobSpec
      <*> arbitrary -- v2alpha1CronJobStatus :: Maybe V2alpha1CronJobStatus
    
instance Arbitrary V2alpha1CronJobList where
  arbitrary =
    V2alpha1CronJobList
      <$> arbitrary -- v2alpha1CronJobListApiVersion :: Maybe Text
      <*> arbitrary -- v2alpha1CronJobListItems :: [V2alpha1CronJob]
      <*> arbitrary -- v2alpha1CronJobListKind :: Maybe Text
      <*> arbitrary -- v2alpha1CronJobListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V2alpha1CronJobSpec where
  arbitrary =
    V2alpha1CronJobSpec
      <$> arbitrary -- v2alpha1CronJobSpecConcurrencyPolicy :: Maybe Text
      <*> arbitrary -- v2alpha1CronJobSpecFailedJobsHistoryLimit :: Maybe Int
      <*> arbitrary -- v2alpha1CronJobSpecJobTemplate :: V2alpha1JobTemplateSpec
      <*> arbitrary -- v2alpha1CronJobSpecSchedule :: Text
      <*> arbitrary -- v2alpha1CronJobSpecStartingDeadlineSeconds :: Maybe Integer
      <*> arbitrary -- v2alpha1CronJobSpecSuccessfulJobsHistoryLimit :: Maybe Int
      <*> arbitrary -- v2alpha1CronJobSpecSuspend :: Maybe Bool
    
instance Arbitrary V2alpha1CronJobStatus where
  arbitrary =
    V2alpha1CronJobStatus
      <$> arbitrary -- v2alpha1CronJobStatusActive :: Maybe [V1ObjectReference]
      <*> arbitrary -- v2alpha1CronJobStatusLastScheduleTime :: Maybe DateTime
    
instance Arbitrary V2alpha1JobTemplateSpec where
  arbitrary =
    V2alpha1JobTemplateSpec
      <$> arbitrary -- v2alpha1JobTemplateSpecMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v2alpha1JobTemplateSpecSpec :: Maybe V1JobSpec
    
instance Arbitrary V2beta1CrossVersionObjectReference where
  arbitrary =
    V2beta1CrossVersionObjectReference
      <$> arbitrary -- v2beta1CrossVersionObjectReferenceApiVersion :: Maybe Text
      <*> arbitrary -- v2beta1CrossVersionObjectReferenceKind :: Text
      <*> arbitrary -- v2beta1CrossVersionObjectReferenceName :: Text
    
instance Arbitrary V2beta1HorizontalPodAutoscaler where
  arbitrary =
    V2beta1HorizontalPodAutoscaler
      <$> arbitrary -- v2beta1HorizontalPodAutoscalerApiVersion :: Maybe Text
      <*> arbitrary -- v2beta1HorizontalPodAutoscalerKind :: Maybe Text
      <*> arbitrary -- v2beta1HorizontalPodAutoscalerMetadata :: Maybe V1ObjectMeta
      <*> arbitrary -- v2beta1HorizontalPodAutoscalerSpec :: Maybe V2beta1HorizontalPodAutoscalerSpec
      <*> arbitrary -- v2beta1HorizontalPodAutoscalerStatus :: Maybe V2beta1HorizontalPodAutoscalerStatus
    
instance Arbitrary V2beta1HorizontalPodAutoscalerCondition where
  arbitrary =
    V2beta1HorizontalPodAutoscalerCondition
      <$> arbitrary -- v2beta1HorizontalPodAutoscalerConditionLastTransitionTime :: Maybe DateTime
      <*> arbitrary -- v2beta1HorizontalPodAutoscalerConditionMessage :: Maybe Text
      <*> arbitrary -- v2beta1HorizontalPodAutoscalerConditionReason :: Maybe Text
      <*> arbitrary -- v2beta1HorizontalPodAutoscalerConditionStatus :: Text
      <*> arbitrary -- v2beta1HorizontalPodAutoscalerConditionType :: Text
    
instance Arbitrary V2beta1HorizontalPodAutoscalerList where
  arbitrary =
    V2beta1HorizontalPodAutoscalerList
      <$> arbitrary -- v2beta1HorizontalPodAutoscalerListApiVersion :: Maybe Text
      <*> arbitrary -- v2beta1HorizontalPodAutoscalerListItems :: [V2beta1HorizontalPodAutoscaler]
      <*> arbitrary -- v2beta1HorizontalPodAutoscalerListKind :: Maybe Text
      <*> arbitrary -- v2beta1HorizontalPodAutoscalerListMetadata :: Maybe V1ListMeta
    
instance Arbitrary V2beta1HorizontalPodAutoscalerSpec where
  arbitrary =
    V2beta1HorizontalPodAutoscalerSpec
      <$> arbitrary -- v2beta1HorizontalPodAutoscalerSpecMaxReplicas :: Int
      <*> arbitrary -- v2beta1HorizontalPodAutoscalerSpecMetrics :: Maybe [V2beta1MetricSpec]
      <*> arbitrary -- v2beta1HorizontalPodAutoscalerSpecMinReplicas :: Maybe Int
      <*> arbitrary -- v2beta1HorizontalPodAutoscalerSpecScaleTargetRef :: V2beta1CrossVersionObjectReference
    
instance Arbitrary V2beta1HorizontalPodAutoscalerStatus where
  arbitrary =
    V2beta1HorizontalPodAutoscalerStatus
      <$> arbitrary -- v2beta1HorizontalPodAutoscalerStatusConditions :: [V2beta1HorizontalPodAutoscalerCondition]
      <*> arbitrary -- v2beta1HorizontalPodAutoscalerStatusCurrentMetrics :: [V2beta1MetricStatus]
      <*> arbitrary -- v2beta1HorizontalPodAutoscalerStatusCurrentReplicas :: Int
      <*> arbitrary -- v2beta1HorizontalPodAutoscalerStatusDesiredReplicas :: Int
      <*> arbitrary -- v2beta1HorizontalPodAutoscalerStatusLastScaleTime :: Maybe DateTime
      <*> arbitrary -- v2beta1HorizontalPodAutoscalerStatusObservedGeneration :: Maybe Integer
    
instance Arbitrary V2beta1MetricSpec where
  arbitrary =
    V2beta1MetricSpec
      <$> arbitrary -- v2beta1MetricSpecObject :: Maybe V2beta1ObjectMetricSource
      <*> arbitrary -- v2beta1MetricSpecPods :: Maybe V2beta1PodsMetricSource
      <*> arbitrary -- v2beta1MetricSpecResource :: Maybe V2beta1ResourceMetricSource
      <*> arbitrary -- v2beta1MetricSpecType :: Text
    
instance Arbitrary V2beta1MetricStatus where
  arbitrary =
    V2beta1MetricStatus
      <$> arbitrary -- v2beta1MetricStatusObject :: Maybe V2beta1ObjectMetricStatus
      <*> arbitrary -- v2beta1MetricStatusPods :: Maybe V2beta1PodsMetricStatus
      <*> arbitrary -- v2beta1MetricStatusResource :: Maybe V2beta1ResourceMetricStatus
      <*> arbitrary -- v2beta1MetricStatusType :: Text
    
instance Arbitrary V2beta1ObjectMetricSource where
  arbitrary =
    V2beta1ObjectMetricSource
      <$> arbitrary -- v2beta1ObjectMetricSourceMetricName :: Text
      <*> arbitrary -- v2beta1ObjectMetricSourceTarget :: V2beta1CrossVersionObjectReference
      <*> arbitrary -- v2beta1ObjectMetricSourceTargetValue :: Text
    
instance Arbitrary V2beta1ObjectMetricStatus where
  arbitrary =
    V2beta1ObjectMetricStatus
      <$> arbitrary -- v2beta1ObjectMetricStatusCurrentValue :: Text
      <*> arbitrary -- v2beta1ObjectMetricStatusMetricName :: Text
      <*> arbitrary -- v2beta1ObjectMetricStatusTarget :: V2beta1CrossVersionObjectReference
    
instance Arbitrary V2beta1PodsMetricSource where
  arbitrary =
    V2beta1PodsMetricSource
      <$> arbitrary -- v2beta1PodsMetricSourceMetricName :: Text
      <*> arbitrary -- v2beta1PodsMetricSourceTargetAverageValue :: Text
    
instance Arbitrary V2beta1PodsMetricStatus where
  arbitrary =
    V2beta1PodsMetricStatus
      <$> arbitrary -- v2beta1PodsMetricStatusCurrentAverageValue :: Text
      <*> arbitrary -- v2beta1PodsMetricStatusMetricName :: Text
    
instance Arbitrary V2beta1ResourceMetricSource where
  arbitrary =
    V2beta1ResourceMetricSource
      <$> arbitrary -- v2beta1ResourceMetricSourceName :: Text
      <*> arbitrary -- v2beta1ResourceMetricSourceTargetAverageUtilization :: Maybe Int
      <*> arbitrary -- v2beta1ResourceMetricSourceTargetAverageValue :: Maybe Text
    
instance Arbitrary V2beta1ResourceMetricStatus where
  arbitrary =
    V2beta1ResourceMetricStatus
      <$> arbitrary -- v2beta1ResourceMetricStatusCurrentAverageUtilization :: Maybe Int
      <*> arbitrary -- v2beta1ResourceMetricStatusCurrentAverageValue :: Text
      <*> arbitrary -- v2beta1ResourceMetricStatusName :: Text
    
instance Arbitrary VersionInfo where
  arbitrary =
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
    


