{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import Kubernetes.OpenAPI.Model
import Kubernetes.OpenAPI.Core
import CustomInstances ()

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V
import Data.String (fromString)

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

#if MIN_VERSION_aeson(2,0,0)
#else
-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = arbitraryValue
#endif

arbitraryValue :: Gen A.Value
arbitraryValue =
  frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (fromString k, v)
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

instance Arbitrary AdmissionregistrationV1ServiceReference where
  arbitrary = sized genAdmissionregistrationV1ServiceReference

genAdmissionregistrationV1ServiceReference :: Int -> Gen AdmissionregistrationV1ServiceReference
genAdmissionregistrationV1ServiceReference n =
  AdmissionregistrationV1ServiceReference
    <$> arbitrary -- admissionregistrationV1ServiceReferenceName :: Text
    <*> arbitrary -- admissionregistrationV1ServiceReferenceNamespace :: Text
    <*> arbitraryReducedMaybe n -- admissionregistrationV1ServiceReferencePath :: Maybe Text
    <*> arbitraryReducedMaybe n -- admissionregistrationV1ServiceReferencePort :: Maybe Int
  
instance Arbitrary AdmissionregistrationV1WebhookClientConfig where
  arbitrary = sized genAdmissionregistrationV1WebhookClientConfig

genAdmissionregistrationV1WebhookClientConfig :: Int -> Gen AdmissionregistrationV1WebhookClientConfig
genAdmissionregistrationV1WebhookClientConfig n =
  AdmissionregistrationV1WebhookClientConfig
    <$> arbitraryReducedMaybe n -- admissionregistrationV1WebhookClientConfigCaBundle :: Maybe ByteArray
    <*> arbitraryReducedMaybe n -- admissionregistrationV1WebhookClientConfigService :: Maybe AdmissionregistrationV1ServiceReference
    <*> arbitraryReducedMaybe n -- admissionregistrationV1WebhookClientConfigUrl :: Maybe Text
  
instance Arbitrary ApiextensionsV1ServiceReference where
  arbitrary = sized genApiextensionsV1ServiceReference

genApiextensionsV1ServiceReference :: Int -> Gen ApiextensionsV1ServiceReference
genApiextensionsV1ServiceReference n =
  ApiextensionsV1ServiceReference
    <$> arbitrary -- apiextensionsV1ServiceReferenceName :: Text
    <*> arbitrary -- apiextensionsV1ServiceReferenceNamespace :: Text
    <*> arbitraryReducedMaybe n -- apiextensionsV1ServiceReferencePath :: Maybe Text
    <*> arbitraryReducedMaybe n -- apiextensionsV1ServiceReferencePort :: Maybe Int
  
instance Arbitrary ApiextensionsV1WebhookClientConfig where
  arbitrary = sized genApiextensionsV1WebhookClientConfig

genApiextensionsV1WebhookClientConfig :: Int -> Gen ApiextensionsV1WebhookClientConfig
genApiextensionsV1WebhookClientConfig n =
  ApiextensionsV1WebhookClientConfig
    <$> arbitraryReducedMaybe n -- apiextensionsV1WebhookClientConfigCaBundle :: Maybe ByteArray
    <*> arbitraryReducedMaybe n -- apiextensionsV1WebhookClientConfigService :: Maybe ApiextensionsV1ServiceReference
    <*> arbitraryReducedMaybe n -- apiextensionsV1WebhookClientConfigUrl :: Maybe Text
  
instance Arbitrary ApiregistrationV1ServiceReference where
  arbitrary = sized genApiregistrationV1ServiceReference

genApiregistrationV1ServiceReference :: Int -> Gen ApiregistrationV1ServiceReference
genApiregistrationV1ServiceReference n =
  ApiregistrationV1ServiceReference
    <$> arbitraryReducedMaybe n -- apiregistrationV1ServiceReferenceName :: Maybe Text
    <*> arbitraryReducedMaybe n -- apiregistrationV1ServiceReferenceNamespace :: Maybe Text
    <*> arbitraryReducedMaybe n -- apiregistrationV1ServiceReferencePort :: Maybe Int
  
instance Arbitrary AuthenticationV1TokenRequest where
  arbitrary = sized genAuthenticationV1TokenRequest

genAuthenticationV1TokenRequest :: Int -> Gen AuthenticationV1TokenRequest
genAuthenticationV1TokenRequest n =
  AuthenticationV1TokenRequest
    <$> arbitraryReducedMaybe n -- authenticationV1TokenRequestApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- authenticationV1TokenRequestKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- authenticationV1TokenRequestMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- authenticationV1TokenRequestSpec :: V1TokenRequestSpec
    <*> arbitraryReducedMaybe n -- authenticationV1TokenRequestStatus :: Maybe V1TokenRequestStatus
  
instance Arbitrary CoreV1EndpointPort where
  arbitrary = sized genCoreV1EndpointPort

genCoreV1EndpointPort :: Int -> Gen CoreV1EndpointPort
genCoreV1EndpointPort n =
  CoreV1EndpointPort
    <$> arbitraryReducedMaybe n -- coreV1EndpointPortAppProtocol :: Maybe Text
    <*> arbitraryReducedMaybe n -- coreV1EndpointPortName :: Maybe Text
    <*> arbitrary -- coreV1EndpointPortPort :: Int
    <*> arbitraryReducedMaybe n -- coreV1EndpointPortProtocol :: Maybe Text
  
instance Arbitrary CoreV1Event where
  arbitrary = sized genCoreV1Event

genCoreV1Event :: Int -> Gen CoreV1Event
genCoreV1Event n =
  CoreV1Event
    <$> arbitraryReducedMaybe n -- coreV1EventAction :: Maybe Text
    <*> arbitraryReducedMaybe n -- coreV1EventApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- coreV1EventCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- coreV1EventEventTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- coreV1EventFirstTimestamp :: Maybe DateTime
    <*> arbitraryReduced n -- coreV1EventInvolvedObject :: V1ObjectReference
    <*> arbitraryReducedMaybe n -- coreV1EventKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- coreV1EventLastTimestamp :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- coreV1EventMessage :: Maybe Text
    <*> arbitraryReduced n -- coreV1EventMetadata :: V1ObjectMeta
    <*> arbitraryReducedMaybe n -- coreV1EventReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- coreV1EventRelated :: Maybe V1ObjectReference
    <*> arbitraryReducedMaybe n -- coreV1EventReportingComponent :: Maybe Text
    <*> arbitraryReducedMaybe n -- coreV1EventReportingInstance :: Maybe Text
    <*> arbitraryReducedMaybe n -- coreV1EventSeries :: Maybe CoreV1EventSeries
    <*> arbitraryReducedMaybe n -- coreV1EventSource :: Maybe V1EventSource
    <*> arbitraryReducedMaybe n -- coreV1EventType :: Maybe Text
  
instance Arbitrary CoreV1EventList where
  arbitrary = sized genCoreV1EventList

genCoreV1EventList :: Int -> Gen CoreV1EventList
genCoreV1EventList n =
  CoreV1EventList
    <$> arbitraryReducedMaybe n -- coreV1EventListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- coreV1EventListItems :: [CoreV1Event]
    <*> arbitraryReducedMaybe n -- coreV1EventListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- coreV1EventListMetadata :: Maybe V1ListMeta
  
instance Arbitrary CoreV1EventSeries where
  arbitrary = sized genCoreV1EventSeries

genCoreV1EventSeries :: Int -> Gen CoreV1EventSeries
genCoreV1EventSeries n =
  CoreV1EventSeries
    <$> arbitraryReducedMaybe n -- coreV1EventSeriesCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- coreV1EventSeriesLastObservedTime :: Maybe DateTime
  
instance Arbitrary DiscoveryV1EndpointPort where
  arbitrary = sized genDiscoveryV1EndpointPort

genDiscoveryV1EndpointPort :: Int -> Gen DiscoveryV1EndpointPort
genDiscoveryV1EndpointPort n =
  DiscoveryV1EndpointPort
    <$> arbitraryReducedMaybe n -- discoveryV1EndpointPortAppProtocol :: Maybe Text
    <*> arbitraryReducedMaybe n -- discoveryV1EndpointPortName :: Maybe Text
    <*> arbitraryReducedMaybe n -- discoveryV1EndpointPortPort :: Maybe Int
    <*> arbitraryReducedMaybe n -- discoveryV1EndpointPortProtocol :: Maybe Text
  
instance Arbitrary EventsV1Event where
  arbitrary = sized genEventsV1Event

genEventsV1Event :: Int -> Gen EventsV1Event
genEventsV1Event n =
  EventsV1Event
    <$> arbitraryReducedMaybe n -- eventsV1EventAction :: Maybe Text
    <*> arbitraryReducedMaybe n -- eventsV1EventApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- eventsV1EventDeprecatedCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- eventsV1EventDeprecatedFirstTimestamp :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- eventsV1EventDeprecatedLastTimestamp :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- eventsV1EventDeprecatedSource :: Maybe V1EventSource
    <*> arbitraryReduced n -- eventsV1EventEventTime :: DateTime
    <*> arbitraryReducedMaybe n -- eventsV1EventKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- eventsV1EventMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- eventsV1EventNote :: Maybe Text
    <*> arbitraryReducedMaybe n -- eventsV1EventReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- eventsV1EventRegarding :: Maybe V1ObjectReference
    <*> arbitraryReducedMaybe n -- eventsV1EventRelated :: Maybe V1ObjectReference
    <*> arbitraryReducedMaybe n -- eventsV1EventReportingController :: Maybe Text
    <*> arbitraryReducedMaybe n -- eventsV1EventReportingInstance :: Maybe Text
    <*> arbitraryReducedMaybe n -- eventsV1EventSeries :: Maybe EventsV1EventSeries
    <*> arbitraryReducedMaybe n -- eventsV1EventType :: Maybe Text
  
instance Arbitrary EventsV1EventList where
  arbitrary = sized genEventsV1EventList

genEventsV1EventList :: Int -> Gen EventsV1EventList
genEventsV1EventList n =
  EventsV1EventList
    <$> arbitraryReducedMaybe n -- eventsV1EventListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- eventsV1EventListItems :: [EventsV1Event]
    <*> arbitraryReducedMaybe n -- eventsV1EventListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- eventsV1EventListMetadata :: Maybe V1ListMeta
  
instance Arbitrary EventsV1EventSeries where
  arbitrary = sized genEventsV1EventSeries

genEventsV1EventSeries :: Int -> Gen EventsV1EventSeries
genEventsV1EventSeries n =
  EventsV1EventSeries
    <$> arbitrary -- eventsV1EventSeriesCount :: Int
    <*> arbitraryReduced n -- eventsV1EventSeriesLastObservedTime :: DateTime
  
instance Arbitrary FlowcontrolV1Subject where
  arbitrary = sized genFlowcontrolV1Subject

genFlowcontrolV1Subject :: Int -> Gen FlowcontrolV1Subject
genFlowcontrolV1Subject n =
  FlowcontrolV1Subject
    <$> arbitraryReducedMaybe n -- flowcontrolV1SubjectGroup :: Maybe V1GroupSubject
    <*> arbitrary -- flowcontrolV1SubjectKind :: Text
    <*> arbitraryReducedMaybe n -- flowcontrolV1SubjectServiceAccount :: Maybe V1ServiceAccountSubject
    <*> arbitraryReducedMaybe n -- flowcontrolV1SubjectUser :: Maybe V1UserSubject
  
instance Arbitrary RbacV1Subject where
  arbitrary = sized genRbacV1Subject

genRbacV1Subject :: Int -> Gen RbacV1Subject
genRbacV1Subject n =
  RbacV1Subject
    <$> arbitraryReducedMaybe n -- rbacV1SubjectApiGroup :: Maybe Text
    <*> arbitrary -- rbacV1SubjectKind :: Text
    <*> arbitrary -- rbacV1SubjectName :: Text
    <*> arbitraryReducedMaybe n -- rbacV1SubjectNamespace :: Maybe Text
  
instance Arbitrary StorageV1TokenRequest where
  arbitrary = sized genStorageV1TokenRequest

genStorageV1TokenRequest :: Int -> Gen StorageV1TokenRequest
genStorageV1TokenRequest n =
  StorageV1TokenRequest
    <$> arbitrary -- storageV1TokenRequestAudience :: Text
    <*> arbitraryReducedMaybe n -- storageV1TokenRequestExpirationSeconds :: Maybe Integer
  
instance Arbitrary V1APIGroup where
  arbitrary = sized genV1APIGroup

genV1APIGroup :: Int -> Gen V1APIGroup
genV1APIGroup n =
  V1APIGroup
    <$> arbitraryReducedMaybe n -- v1APIGroupApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1APIGroupKind :: Maybe Text
    <*> arbitrary -- v1APIGroupName :: Text
    <*> arbitraryReducedMaybe n -- v1APIGroupPreferredVersion :: Maybe V1GroupVersionForDiscovery
    <*> arbitraryReducedMaybe n -- v1APIGroupServerAddressByClientCidrs :: Maybe [V1ServerAddressByClientCIDR]
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
    <*> arbitraryReducedMaybe n -- v1APIResourceStorageVersionHash :: Maybe Text
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
  
instance Arbitrary V1APIService where
  arbitrary = sized genV1APIService

genV1APIService :: Int -> Gen V1APIService
genV1APIService n =
  V1APIService
    <$> arbitraryReducedMaybe n -- v1APIServiceApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1APIServiceKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1APIServiceMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1APIServiceSpec :: Maybe V1APIServiceSpec
    <*> arbitraryReducedMaybe n -- v1APIServiceStatus :: Maybe V1APIServiceStatus
  
instance Arbitrary V1APIServiceCondition where
  arbitrary = sized genV1APIServiceCondition

genV1APIServiceCondition :: Int -> Gen V1APIServiceCondition
genV1APIServiceCondition n =
  V1APIServiceCondition
    <$> arbitraryReducedMaybe n -- v1APIServiceConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1APIServiceConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1APIServiceConditionReason :: Maybe Text
    <*> arbitrary -- v1APIServiceConditionStatus :: Text
    <*> arbitrary -- v1APIServiceConditionType :: Text
  
instance Arbitrary V1APIServiceList where
  arbitrary = sized genV1APIServiceList

genV1APIServiceList :: Int -> Gen V1APIServiceList
genV1APIServiceList n =
  V1APIServiceList
    <$> arbitraryReducedMaybe n -- v1APIServiceListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1APIServiceListItems :: [V1APIService]
    <*> arbitraryReducedMaybe n -- v1APIServiceListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1APIServiceListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1APIServiceSpec where
  arbitrary = sized genV1APIServiceSpec

genV1APIServiceSpec :: Int -> Gen V1APIServiceSpec
genV1APIServiceSpec n =
  V1APIServiceSpec
    <$> arbitraryReducedMaybe n -- v1APIServiceSpecCaBundle :: Maybe ByteArray
    <*> arbitraryReducedMaybe n -- v1APIServiceSpecGroup :: Maybe Text
    <*> arbitrary -- v1APIServiceSpecGroupPriorityMinimum :: Int
    <*> arbitraryReducedMaybe n -- v1APIServiceSpecInsecureSkipTlsVerify :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1APIServiceSpecService :: Maybe ApiregistrationV1ServiceReference
    <*> arbitraryReducedMaybe n -- v1APIServiceSpecVersion :: Maybe Text
    <*> arbitrary -- v1APIServiceSpecVersionPriority :: Int
  
instance Arbitrary V1APIServiceStatus where
  arbitrary = sized genV1APIServiceStatus

genV1APIServiceStatus :: Int -> Gen V1APIServiceStatus
genV1APIServiceStatus n =
  V1APIServiceStatus
    <$> arbitraryReducedMaybe n -- v1APIServiceStatusConditions :: Maybe [V1APIServiceCondition]
  
instance Arbitrary V1APIVersions where
  arbitrary = sized genV1APIVersions

genV1APIVersions :: Int -> Gen V1APIVersions
genV1APIVersions n =
  V1APIVersions
    <$> arbitraryReducedMaybe n -- v1APIVersionsApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1APIVersionsKind :: Maybe Text
    <*> arbitraryReduced n -- v1APIVersionsServerAddressByClientCidrs :: [V1ServerAddressByClientCIDR]
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
  
instance Arbitrary V1BoundObjectReference where
  arbitrary = sized genV1BoundObjectReference

genV1BoundObjectReference :: Int -> Gen V1BoundObjectReference
genV1BoundObjectReference n =
  V1BoundObjectReference
    <$> arbitraryReducedMaybe n -- v1BoundObjectReferenceApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1BoundObjectReferenceKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1BoundObjectReferenceName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1BoundObjectReferenceUid :: Maybe Text
  
instance Arbitrary V1CSIDriver where
  arbitrary = sized genV1CSIDriver

genV1CSIDriver :: Int -> Gen V1CSIDriver
genV1CSIDriver n =
  V1CSIDriver
    <$> arbitraryReducedMaybe n -- v1CSIDriverApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CSIDriverKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CSIDriverMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1CSIDriverSpec :: V1CSIDriverSpec
  
instance Arbitrary V1CSIDriverList where
  arbitrary = sized genV1CSIDriverList

genV1CSIDriverList :: Int -> Gen V1CSIDriverList
genV1CSIDriverList n =
  V1CSIDriverList
    <$> arbitraryReducedMaybe n -- v1CSIDriverListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1CSIDriverListItems :: [V1CSIDriver]
    <*> arbitraryReducedMaybe n -- v1CSIDriverListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CSIDriverListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1CSIDriverSpec where
  arbitrary = sized genV1CSIDriverSpec

genV1CSIDriverSpec :: Int -> Gen V1CSIDriverSpec
genV1CSIDriverSpec n =
  V1CSIDriverSpec
    <$> arbitraryReducedMaybe n -- v1CSIDriverSpecAttachRequired :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1CSIDriverSpecFsGroupPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CSIDriverSpecPodInfoOnMount :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1CSIDriverSpecRequiresRepublish :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1CSIDriverSpecSeLinuxMount :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1CSIDriverSpecStorageCapacity :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1CSIDriverSpecTokenRequests :: Maybe [StorageV1TokenRequest]
    <*> arbitraryReducedMaybe n -- v1CSIDriverSpecVolumeLifecycleModes :: Maybe [Text]
  
instance Arbitrary V1CSINode where
  arbitrary = sized genV1CSINode

genV1CSINode :: Int -> Gen V1CSINode
genV1CSINode n =
  V1CSINode
    <$> arbitraryReducedMaybe n -- v1CSINodeApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CSINodeKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CSINodeMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1CSINodeSpec :: V1CSINodeSpec
  
instance Arbitrary V1CSINodeDriver where
  arbitrary = sized genV1CSINodeDriver

genV1CSINodeDriver :: Int -> Gen V1CSINodeDriver
genV1CSINodeDriver n =
  V1CSINodeDriver
    <$> arbitraryReducedMaybe n -- v1CSINodeDriverAllocatable :: Maybe V1VolumeNodeResources
    <*> arbitrary -- v1CSINodeDriverName :: Text
    <*> arbitrary -- v1CSINodeDriverNodeId :: Text
    <*> arbitraryReducedMaybe n -- v1CSINodeDriverTopologyKeys :: Maybe [Text]
  
instance Arbitrary V1CSINodeList where
  arbitrary = sized genV1CSINodeList

genV1CSINodeList :: Int -> Gen V1CSINodeList
genV1CSINodeList n =
  V1CSINodeList
    <$> arbitraryReducedMaybe n -- v1CSINodeListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1CSINodeListItems :: [V1CSINode]
    <*> arbitraryReducedMaybe n -- v1CSINodeListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CSINodeListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1CSINodeSpec where
  arbitrary = sized genV1CSINodeSpec

genV1CSINodeSpec :: Int -> Gen V1CSINodeSpec
genV1CSINodeSpec n =
  V1CSINodeSpec
    <$> arbitraryReduced n -- v1CSINodeSpecDrivers :: [V1CSINodeDriver]
  
instance Arbitrary V1CSIPersistentVolumeSource where
  arbitrary = sized genV1CSIPersistentVolumeSource

genV1CSIPersistentVolumeSource :: Int -> Gen V1CSIPersistentVolumeSource
genV1CSIPersistentVolumeSource n =
  V1CSIPersistentVolumeSource
    <$> arbitraryReducedMaybe n -- v1CSIPersistentVolumeSourceControllerExpandSecretRef :: Maybe V1SecretReference
    <*> arbitraryReducedMaybe n -- v1CSIPersistentVolumeSourceControllerPublishSecretRef :: Maybe V1SecretReference
    <*> arbitrary -- v1CSIPersistentVolumeSourceDriver :: Text
    <*> arbitraryReducedMaybe n -- v1CSIPersistentVolumeSourceFsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CSIPersistentVolumeSourceNodeExpandSecretRef :: Maybe V1SecretReference
    <*> arbitraryReducedMaybe n -- v1CSIPersistentVolumeSourceNodePublishSecretRef :: Maybe V1SecretReference
    <*> arbitraryReducedMaybe n -- v1CSIPersistentVolumeSourceNodeStageSecretRef :: Maybe V1SecretReference
    <*> arbitraryReducedMaybe n -- v1CSIPersistentVolumeSourceReadOnly :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1CSIPersistentVolumeSourceVolumeAttributes :: Maybe (Map.Map String Text)
    <*> arbitrary -- v1CSIPersistentVolumeSourceVolumeHandle :: Text
  
instance Arbitrary V1CSIStorageCapacity where
  arbitrary = sized genV1CSIStorageCapacity

genV1CSIStorageCapacity :: Int -> Gen V1CSIStorageCapacity
genV1CSIStorageCapacity n =
  V1CSIStorageCapacity
    <$> arbitraryReducedMaybe n -- v1CSIStorageCapacityApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CSIStorageCapacityCapacity :: Maybe Quantity
    <*> arbitraryReducedMaybe n -- v1CSIStorageCapacityKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CSIStorageCapacityMaximumVolumeSize :: Maybe Quantity
    <*> arbitraryReducedMaybe n -- v1CSIStorageCapacityMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1CSIStorageCapacityNodeTopology :: Maybe V1LabelSelector
    <*> arbitrary -- v1CSIStorageCapacityStorageClassName :: Text
  
instance Arbitrary V1CSIStorageCapacityList where
  arbitrary = sized genV1CSIStorageCapacityList

genV1CSIStorageCapacityList :: Int -> Gen V1CSIStorageCapacityList
genV1CSIStorageCapacityList n =
  V1CSIStorageCapacityList
    <$> arbitraryReducedMaybe n -- v1CSIStorageCapacityListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1CSIStorageCapacityListItems :: [V1CSIStorageCapacity]
    <*> arbitraryReducedMaybe n -- v1CSIStorageCapacityListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CSIStorageCapacityListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1CSIVolumeSource where
  arbitrary = sized genV1CSIVolumeSource

genV1CSIVolumeSource :: Int -> Gen V1CSIVolumeSource
genV1CSIVolumeSource n =
  V1CSIVolumeSource
    <$> arbitrary -- v1CSIVolumeSourceDriver :: Text
    <*> arbitraryReducedMaybe n -- v1CSIVolumeSourceFsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CSIVolumeSourceNodePublishSecretRef :: Maybe V1LocalObjectReference
    <*> arbitraryReducedMaybe n -- v1CSIVolumeSourceReadOnly :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1CSIVolumeSourceVolumeAttributes :: Maybe (Map.Map String Text)
  
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
  
instance Arbitrary V1CertificateSigningRequest where
  arbitrary = sized genV1CertificateSigningRequest

genV1CertificateSigningRequest :: Int -> Gen V1CertificateSigningRequest
genV1CertificateSigningRequest n =
  V1CertificateSigningRequest
    <$> arbitraryReducedMaybe n -- v1CertificateSigningRequestApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CertificateSigningRequestKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CertificateSigningRequestMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1CertificateSigningRequestSpec :: V1CertificateSigningRequestSpec
    <*> arbitraryReducedMaybe n -- v1CertificateSigningRequestStatus :: Maybe V1CertificateSigningRequestStatus
  
instance Arbitrary V1CertificateSigningRequestCondition where
  arbitrary = sized genV1CertificateSigningRequestCondition

genV1CertificateSigningRequestCondition :: Int -> Gen V1CertificateSigningRequestCondition
genV1CertificateSigningRequestCondition n =
  V1CertificateSigningRequestCondition
    <$> arbitraryReducedMaybe n -- v1CertificateSigningRequestConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1CertificateSigningRequestConditionLastUpdateTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1CertificateSigningRequestConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CertificateSigningRequestConditionReason :: Maybe Text
    <*> arbitrary -- v1CertificateSigningRequestConditionStatus :: Text
    <*> arbitrary -- v1CertificateSigningRequestConditionType :: Text
  
instance Arbitrary V1CertificateSigningRequestList where
  arbitrary = sized genV1CertificateSigningRequestList

genV1CertificateSigningRequestList :: Int -> Gen V1CertificateSigningRequestList
genV1CertificateSigningRequestList n =
  V1CertificateSigningRequestList
    <$> arbitraryReducedMaybe n -- v1CertificateSigningRequestListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1CertificateSigningRequestListItems :: [V1CertificateSigningRequest]
    <*> arbitraryReducedMaybe n -- v1CertificateSigningRequestListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CertificateSigningRequestListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1CertificateSigningRequestSpec where
  arbitrary = sized genV1CertificateSigningRequestSpec

genV1CertificateSigningRequestSpec :: Int -> Gen V1CertificateSigningRequestSpec
genV1CertificateSigningRequestSpec n =
  V1CertificateSigningRequestSpec
    <$> arbitraryReducedMaybe n -- v1CertificateSigningRequestSpecExpirationSeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1CertificateSigningRequestSpecExtra :: Maybe (Map.Map String [Text])
    <*> arbitraryReducedMaybe n -- v1CertificateSigningRequestSpecGroups :: Maybe [Text]
    <*> arbitraryReduced n -- v1CertificateSigningRequestSpecRequest :: ByteArray
    <*> arbitrary -- v1CertificateSigningRequestSpecSignerName :: Text
    <*> arbitraryReducedMaybe n -- v1CertificateSigningRequestSpecUid :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CertificateSigningRequestSpecUsages :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1CertificateSigningRequestSpecUsername :: Maybe Text
  
instance Arbitrary V1CertificateSigningRequestStatus where
  arbitrary = sized genV1CertificateSigningRequestStatus

genV1CertificateSigningRequestStatus :: Int -> Gen V1CertificateSigningRequestStatus
genV1CertificateSigningRequestStatus n =
  V1CertificateSigningRequestStatus
    <$> arbitraryReducedMaybe n -- v1CertificateSigningRequestStatusCertificate :: Maybe ByteArray
    <*> arbitraryReducedMaybe n -- v1CertificateSigningRequestStatusConditions :: Maybe [V1CertificateSigningRequestCondition]
  
instance Arbitrary V1CinderPersistentVolumeSource where
  arbitrary = sized genV1CinderPersistentVolumeSource

genV1CinderPersistentVolumeSource :: Int -> Gen V1CinderPersistentVolumeSource
genV1CinderPersistentVolumeSource n =
  V1CinderPersistentVolumeSource
    <$> arbitraryReducedMaybe n -- v1CinderPersistentVolumeSourceFsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CinderPersistentVolumeSourceReadOnly :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1CinderPersistentVolumeSourceSecretRef :: Maybe V1SecretReference
    <*> arbitrary -- v1CinderPersistentVolumeSourceVolumeId :: Text
  
instance Arbitrary V1CinderVolumeSource where
  arbitrary = sized genV1CinderVolumeSource

genV1CinderVolumeSource :: Int -> Gen V1CinderVolumeSource
genV1CinderVolumeSource n =
  V1CinderVolumeSource
    <$> arbitraryReducedMaybe n -- v1CinderVolumeSourceFsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CinderVolumeSourceReadOnly :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1CinderVolumeSourceSecretRef :: Maybe V1LocalObjectReference
    <*> arbitrary -- v1CinderVolumeSourceVolumeId :: Text
  
instance Arbitrary V1ClaimSource where
  arbitrary = sized genV1ClaimSource

genV1ClaimSource :: Int -> Gen V1ClaimSource
genV1ClaimSource n =
  V1ClaimSource
    <$> arbitraryReducedMaybe n -- v1ClaimSourceResourceClaimName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ClaimSourceResourceClaimTemplateName :: Maybe Text
  
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
    <*> arbitraryReducedMaybe n -- v1ClusterRoleRules :: Maybe [V1PolicyRule]
  
instance Arbitrary V1ClusterRoleBinding where
  arbitrary = sized genV1ClusterRoleBinding

genV1ClusterRoleBinding :: Int -> Gen V1ClusterRoleBinding
genV1ClusterRoleBinding n =
  V1ClusterRoleBinding
    <$> arbitraryReducedMaybe n -- v1ClusterRoleBindingApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ClusterRoleBindingKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ClusterRoleBindingMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1ClusterRoleBindingRoleRef :: V1RoleRef
    <*> arbitraryReducedMaybe n -- v1ClusterRoleBindingSubjects :: Maybe [RbacV1Subject]
  
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
  
instance Arbitrary V1ClusterTrustBundleProjection where
  arbitrary = sized genV1ClusterTrustBundleProjection

genV1ClusterTrustBundleProjection :: Int -> Gen V1ClusterTrustBundleProjection
genV1ClusterTrustBundleProjection n =
  V1ClusterTrustBundleProjection
    <$> arbitraryReducedMaybe n -- v1ClusterTrustBundleProjectionLabelSelector :: Maybe V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1ClusterTrustBundleProjectionName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ClusterTrustBundleProjectionOptional :: Maybe Bool
    <*> arbitrary -- v1ClusterTrustBundleProjectionPath :: Text
    <*> arbitraryReducedMaybe n -- v1ClusterTrustBundleProjectionSignerName :: Maybe Text
  
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
  
instance Arbitrary V1Condition where
  arbitrary = sized genV1Condition

genV1Condition :: Int -> Gen V1Condition
genV1Condition n =
  V1Condition
    <$> arbitraryReduced n -- v1ConditionLastTransitionTime :: DateTime
    <*> arbitrary -- v1ConditionMessage :: Text
    <*> arbitraryReducedMaybe n -- v1ConditionObservedGeneration :: Maybe Integer
    <*> arbitrary -- v1ConditionReason :: Text
    <*> arbitrary -- v1ConditionStatus :: Text
    <*> arbitrary -- v1ConditionType :: Text
  
instance Arbitrary V1ConfigMap where
  arbitrary = sized genV1ConfigMap

genV1ConfigMap :: Int -> Gen V1ConfigMap
genV1ConfigMap n =
  V1ConfigMap
    <$> arbitraryReducedMaybe n -- v1ConfigMapApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ConfigMapBinaryData :: Maybe (Map.Map String ByteArray)
    <*> arbitraryReducedMaybe n -- v1ConfigMapData :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1ConfigMapImmutable :: Maybe Bool
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
  
instance Arbitrary V1ConfigMapNodeConfigSource where
  arbitrary = sized genV1ConfigMapNodeConfigSource

genV1ConfigMapNodeConfigSource :: Int -> Gen V1ConfigMapNodeConfigSource
genV1ConfigMapNodeConfigSource n =
  V1ConfigMapNodeConfigSource
    <$> arbitrary -- v1ConfigMapNodeConfigSourceKubeletConfigKey :: Text
    <*> arbitrary -- v1ConfigMapNodeConfigSourceName :: Text
    <*> arbitrary -- v1ConfigMapNodeConfigSourceNamespace :: Text
    <*> arbitraryReducedMaybe n -- v1ConfigMapNodeConfigSourceResourceVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ConfigMapNodeConfigSourceUid :: Maybe Text
  
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
    <*> arbitraryReducedMaybe n -- v1ContainerResizePolicy :: Maybe [V1ContainerResizePolicy]
    <*> arbitraryReducedMaybe n -- v1ContainerResources :: Maybe V1ResourceRequirements
    <*> arbitraryReducedMaybe n -- v1ContainerRestartPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ContainerSecurityContext :: Maybe V1SecurityContext
    <*> arbitraryReducedMaybe n -- v1ContainerStartupProbe :: Maybe V1Probe
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
    <$> arbitraryReducedMaybe n -- v1ContainerImageNames :: Maybe [Text]
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
  
instance Arbitrary V1ContainerResizePolicy where
  arbitrary = sized genV1ContainerResizePolicy

genV1ContainerResizePolicy :: Int -> Gen V1ContainerResizePolicy
genV1ContainerResizePolicy n =
  V1ContainerResizePolicy
    <$> arbitrary -- v1ContainerResizePolicyResourceName :: Text
    <*> arbitrary -- v1ContainerResizePolicyRestartPolicy :: Text
  
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
    <$> arbitraryReducedMaybe n -- v1ContainerStatusAllocatedResources :: Maybe (Map.Map String Quantity)
    <*> arbitraryReducedMaybe n -- v1ContainerStatusContainerId :: Maybe Text
    <*> arbitrary -- v1ContainerStatusImage :: Text
    <*> arbitrary -- v1ContainerStatusImageId :: Text
    <*> arbitraryReducedMaybe n -- v1ContainerStatusLastState :: Maybe V1ContainerState
    <*> arbitrary -- v1ContainerStatusName :: Text
    <*> arbitrary -- v1ContainerStatusReady :: Bool
    <*> arbitraryReducedMaybe n -- v1ContainerStatusResources :: Maybe V1ResourceRequirements
    <*> arbitrary -- v1ContainerStatusRestartCount :: Int
    <*> arbitraryReducedMaybe n -- v1ContainerStatusStarted :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1ContainerStatusState :: Maybe V1ContainerState
  
instance Arbitrary V1ControllerRevision where
  arbitrary = sized genV1ControllerRevision

genV1ControllerRevision :: Int -> Gen V1ControllerRevision
genV1ControllerRevision n =
  V1ControllerRevision
    <$> arbitraryReducedMaybe n -- v1ControllerRevisionApiVersion :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- v1ControllerRevisionData :: Maybe A.Value
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
  
instance Arbitrary V1CronJob where
  arbitrary = sized genV1CronJob

genV1CronJob :: Int -> Gen V1CronJob
genV1CronJob n =
  V1CronJob
    <$> arbitraryReducedMaybe n -- v1CronJobApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CronJobKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CronJobMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1CronJobSpec :: Maybe V1CronJobSpec
    <*> arbitraryReducedMaybe n -- v1CronJobStatus :: Maybe V1CronJobStatus
  
instance Arbitrary V1CronJobList where
  arbitrary = sized genV1CronJobList

genV1CronJobList :: Int -> Gen V1CronJobList
genV1CronJobList n =
  V1CronJobList
    <$> arbitraryReducedMaybe n -- v1CronJobListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1CronJobListItems :: [V1CronJob]
    <*> arbitraryReducedMaybe n -- v1CronJobListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CronJobListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1CronJobSpec where
  arbitrary = sized genV1CronJobSpec

genV1CronJobSpec :: Int -> Gen V1CronJobSpec
genV1CronJobSpec n =
  V1CronJobSpec
    <$> arbitraryReducedMaybe n -- v1CronJobSpecConcurrencyPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CronJobSpecFailedJobsHistoryLimit :: Maybe Int
    <*> arbitraryReduced n -- v1CronJobSpecJobTemplate :: V1JobTemplateSpec
    <*> arbitrary -- v1CronJobSpecSchedule :: Text
    <*> arbitraryReducedMaybe n -- v1CronJobSpecStartingDeadlineSeconds :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1CronJobSpecSuccessfulJobsHistoryLimit :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1CronJobSpecSuspend :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1CronJobSpecTimeZone :: Maybe Text
  
instance Arbitrary V1CronJobStatus where
  arbitrary = sized genV1CronJobStatus

genV1CronJobStatus :: Int -> Gen V1CronJobStatus
genV1CronJobStatus n =
  V1CronJobStatus
    <$> arbitraryReducedMaybe n -- v1CronJobStatusActive :: Maybe [V1ObjectReference]
    <*> arbitraryReducedMaybe n -- v1CronJobStatusLastScheduleTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1CronJobStatusLastSuccessfulTime :: Maybe DateTime
  
instance Arbitrary V1CrossVersionObjectReference where
  arbitrary = sized genV1CrossVersionObjectReference

genV1CrossVersionObjectReference :: Int -> Gen V1CrossVersionObjectReference
genV1CrossVersionObjectReference n =
  V1CrossVersionObjectReference
    <$> arbitraryReducedMaybe n -- v1CrossVersionObjectReferenceApiVersion :: Maybe Text
    <*> arbitrary -- v1CrossVersionObjectReferenceKind :: Text
    <*> arbitrary -- v1CrossVersionObjectReferenceName :: Text
  
instance Arbitrary V1CustomResourceColumnDefinition where
  arbitrary = sized genV1CustomResourceColumnDefinition

genV1CustomResourceColumnDefinition :: Int -> Gen V1CustomResourceColumnDefinition
genV1CustomResourceColumnDefinition n =
  V1CustomResourceColumnDefinition
    <$> arbitraryReducedMaybe n -- v1CustomResourceColumnDefinitionDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CustomResourceColumnDefinitionFormat :: Maybe Text
    <*> arbitrary -- v1CustomResourceColumnDefinitionJsonPath :: Text
    <*> arbitrary -- v1CustomResourceColumnDefinitionName :: Text
    <*> arbitraryReducedMaybe n -- v1CustomResourceColumnDefinitionPriority :: Maybe Int
    <*> arbitrary -- v1CustomResourceColumnDefinitionType :: Text
  
instance Arbitrary V1CustomResourceConversion where
  arbitrary = sized genV1CustomResourceConversion

genV1CustomResourceConversion :: Int -> Gen V1CustomResourceConversion
genV1CustomResourceConversion n =
  V1CustomResourceConversion
    <$> arbitrary -- v1CustomResourceConversionStrategy :: Text
    <*> arbitraryReducedMaybe n -- v1CustomResourceConversionWebhook :: Maybe V1WebhookConversion
  
instance Arbitrary V1CustomResourceDefinition where
  arbitrary = sized genV1CustomResourceDefinition

genV1CustomResourceDefinition :: Int -> Gen V1CustomResourceDefinition
genV1CustomResourceDefinition n =
  V1CustomResourceDefinition
    <$> arbitraryReducedMaybe n -- v1CustomResourceDefinitionApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CustomResourceDefinitionKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CustomResourceDefinitionMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1CustomResourceDefinitionSpec :: V1CustomResourceDefinitionSpec
    <*> arbitraryReducedMaybe n -- v1CustomResourceDefinitionStatus :: Maybe V1CustomResourceDefinitionStatus
  
instance Arbitrary V1CustomResourceDefinitionCondition where
  arbitrary = sized genV1CustomResourceDefinitionCondition

genV1CustomResourceDefinitionCondition :: Int -> Gen V1CustomResourceDefinitionCondition
genV1CustomResourceDefinitionCondition n =
  V1CustomResourceDefinitionCondition
    <$> arbitraryReducedMaybe n -- v1CustomResourceDefinitionConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1CustomResourceDefinitionConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CustomResourceDefinitionConditionReason :: Maybe Text
    <*> arbitrary -- v1CustomResourceDefinitionConditionStatus :: Text
    <*> arbitrary -- v1CustomResourceDefinitionConditionType :: Text
  
instance Arbitrary V1CustomResourceDefinitionList where
  arbitrary = sized genV1CustomResourceDefinitionList

genV1CustomResourceDefinitionList :: Int -> Gen V1CustomResourceDefinitionList
genV1CustomResourceDefinitionList n =
  V1CustomResourceDefinitionList
    <$> arbitraryReducedMaybe n -- v1CustomResourceDefinitionListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1CustomResourceDefinitionListItems :: [V1CustomResourceDefinition]
    <*> arbitraryReducedMaybe n -- v1CustomResourceDefinitionListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1CustomResourceDefinitionListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1CustomResourceDefinitionNames where
  arbitrary = sized genV1CustomResourceDefinitionNames

genV1CustomResourceDefinitionNames :: Int -> Gen V1CustomResourceDefinitionNames
genV1CustomResourceDefinitionNames n =
  V1CustomResourceDefinitionNames
    <$> arbitraryReducedMaybe n -- v1CustomResourceDefinitionNamesCategories :: Maybe [Text]
    <*> arbitrary -- v1CustomResourceDefinitionNamesKind :: Text
    <*> arbitraryReducedMaybe n -- v1CustomResourceDefinitionNamesListKind :: Maybe Text
    <*> arbitrary -- v1CustomResourceDefinitionNamesPlural :: Text
    <*> arbitraryReducedMaybe n -- v1CustomResourceDefinitionNamesShortNames :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1CustomResourceDefinitionNamesSingular :: Maybe Text
  
instance Arbitrary V1CustomResourceDefinitionSpec where
  arbitrary = sized genV1CustomResourceDefinitionSpec

genV1CustomResourceDefinitionSpec :: Int -> Gen V1CustomResourceDefinitionSpec
genV1CustomResourceDefinitionSpec n =
  V1CustomResourceDefinitionSpec
    <$> arbitraryReducedMaybe n -- v1CustomResourceDefinitionSpecConversion :: Maybe V1CustomResourceConversion
    <*> arbitrary -- v1CustomResourceDefinitionSpecGroup :: Text
    <*> arbitraryReduced n -- v1CustomResourceDefinitionSpecNames :: V1CustomResourceDefinitionNames
    <*> arbitraryReducedMaybe n -- v1CustomResourceDefinitionSpecPreserveUnknownFields :: Maybe Bool
    <*> arbitrary -- v1CustomResourceDefinitionSpecScope :: Text
    <*> arbitraryReduced n -- v1CustomResourceDefinitionSpecVersions :: [V1CustomResourceDefinitionVersion]
  
instance Arbitrary V1CustomResourceDefinitionStatus where
  arbitrary = sized genV1CustomResourceDefinitionStatus

genV1CustomResourceDefinitionStatus :: Int -> Gen V1CustomResourceDefinitionStatus
genV1CustomResourceDefinitionStatus n =
  V1CustomResourceDefinitionStatus
    <$> arbitraryReducedMaybe n -- v1CustomResourceDefinitionStatusAcceptedNames :: Maybe V1CustomResourceDefinitionNames
    <*> arbitraryReducedMaybe n -- v1CustomResourceDefinitionStatusConditions :: Maybe [V1CustomResourceDefinitionCondition]
    <*> arbitraryReducedMaybe n -- v1CustomResourceDefinitionStatusStoredVersions :: Maybe [Text]
  
instance Arbitrary V1CustomResourceDefinitionVersion where
  arbitrary = sized genV1CustomResourceDefinitionVersion

genV1CustomResourceDefinitionVersion :: Int -> Gen V1CustomResourceDefinitionVersion
genV1CustomResourceDefinitionVersion n =
  V1CustomResourceDefinitionVersion
    <$> arbitraryReducedMaybe n -- v1CustomResourceDefinitionVersionAdditionalPrinterColumns :: Maybe [V1CustomResourceColumnDefinition]
    <*> arbitraryReducedMaybe n -- v1CustomResourceDefinitionVersionDeprecated :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1CustomResourceDefinitionVersionDeprecationWarning :: Maybe Text
    <*> arbitrary -- v1CustomResourceDefinitionVersionName :: Text
    <*> arbitraryReducedMaybe n -- v1CustomResourceDefinitionVersionSchema :: Maybe V1CustomResourceValidation
    <*> arbitrary -- v1CustomResourceDefinitionVersionServed :: Bool
    <*> arbitrary -- v1CustomResourceDefinitionVersionStorage :: Bool
    <*> arbitraryReducedMaybe n -- v1CustomResourceDefinitionVersionSubresources :: Maybe V1CustomResourceSubresources
  
instance Arbitrary V1CustomResourceSubresourceScale where
  arbitrary = sized genV1CustomResourceSubresourceScale

genV1CustomResourceSubresourceScale :: Int -> Gen V1CustomResourceSubresourceScale
genV1CustomResourceSubresourceScale n =
  V1CustomResourceSubresourceScale
    <$> arbitraryReducedMaybe n -- v1CustomResourceSubresourceScaleLabelSelectorPath :: Maybe Text
    <*> arbitrary -- v1CustomResourceSubresourceScaleSpecReplicasPath :: Text
    <*> arbitrary -- v1CustomResourceSubresourceScaleStatusReplicasPath :: Text
  
instance Arbitrary V1CustomResourceSubresources where
  arbitrary = sized genV1CustomResourceSubresources

genV1CustomResourceSubresources :: Int -> Gen V1CustomResourceSubresources
genV1CustomResourceSubresources n =
  V1CustomResourceSubresources
    <$> arbitraryReducedMaybe n -- v1CustomResourceSubresourcesScale :: Maybe V1CustomResourceSubresourceScale
    <*> arbitraryReducedMaybeValue n -- v1CustomResourceSubresourcesStatus :: Maybe A.Value
  
instance Arbitrary V1CustomResourceValidation where
  arbitrary = sized genV1CustomResourceValidation

genV1CustomResourceValidation :: Int -> Gen V1CustomResourceValidation
genV1CustomResourceValidation n =
  V1CustomResourceValidation
    <$> arbitraryReducedMaybe n -- v1CustomResourceValidationOpenApiv3Schema :: Maybe V1JSONSchemaProps
  
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
    <*> arbitraryReducedMaybe n -- v1DeleteOptionsDryRun :: Maybe [Text]
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
    <*> arbitraryReducedMaybe n -- v1EmptyDirVolumeSourceSizeLimit :: Maybe Quantity
  
instance Arbitrary V1Endpoint where
  arbitrary = sized genV1Endpoint

genV1Endpoint :: Int -> Gen V1Endpoint
genV1Endpoint n =
  V1Endpoint
    <$> arbitrary -- v1EndpointAddresses :: [Text]
    <*> arbitraryReducedMaybe n -- v1EndpointConditions :: Maybe V1EndpointConditions
    <*> arbitraryReducedMaybe n -- v1EndpointDeprecatedTopology :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1EndpointHints :: Maybe V1EndpointHints
    <*> arbitraryReducedMaybe n -- v1EndpointHostname :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EndpointNodeName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EndpointTargetRef :: Maybe V1ObjectReference
    <*> arbitraryReducedMaybe n -- v1EndpointZone :: Maybe Text
  
instance Arbitrary V1EndpointAddress where
  arbitrary = sized genV1EndpointAddress

genV1EndpointAddress :: Int -> Gen V1EndpointAddress
genV1EndpointAddress n =
  V1EndpointAddress
    <$> arbitraryReducedMaybe n -- v1EndpointAddressHostname :: Maybe Text
    <*> arbitrary -- v1EndpointAddressIp :: Text
    <*> arbitraryReducedMaybe n -- v1EndpointAddressNodeName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EndpointAddressTargetRef :: Maybe V1ObjectReference
  
instance Arbitrary V1EndpointConditions where
  arbitrary = sized genV1EndpointConditions

genV1EndpointConditions :: Int -> Gen V1EndpointConditions
genV1EndpointConditions n =
  V1EndpointConditions
    <$> arbitraryReducedMaybe n -- v1EndpointConditionsReady :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1EndpointConditionsServing :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1EndpointConditionsTerminating :: Maybe Bool
  
instance Arbitrary V1EndpointHints where
  arbitrary = sized genV1EndpointHints

genV1EndpointHints :: Int -> Gen V1EndpointHints
genV1EndpointHints n =
  V1EndpointHints
    <$> arbitraryReducedMaybe n -- v1EndpointHintsForZones :: Maybe [V1ForZone]
  
instance Arbitrary V1EndpointSlice where
  arbitrary = sized genV1EndpointSlice

genV1EndpointSlice :: Int -> Gen V1EndpointSlice
genV1EndpointSlice n =
  V1EndpointSlice
    <$> arbitrary -- v1EndpointSliceAddressType :: Text
    <*> arbitraryReducedMaybe n -- v1EndpointSliceApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1EndpointSliceEndpoints :: [V1Endpoint]
    <*> arbitraryReducedMaybe n -- v1EndpointSliceKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EndpointSliceMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1EndpointSlicePorts :: Maybe [DiscoveryV1EndpointPort]
  
instance Arbitrary V1EndpointSliceList where
  arbitrary = sized genV1EndpointSliceList

genV1EndpointSliceList :: Int -> Gen V1EndpointSliceList
genV1EndpointSliceList n =
  V1EndpointSliceList
    <$> arbitraryReducedMaybe n -- v1EndpointSliceListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1EndpointSliceListItems :: [V1EndpointSlice]
    <*> arbitraryReducedMaybe n -- v1EndpointSliceListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EndpointSliceListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1EndpointSubset where
  arbitrary = sized genV1EndpointSubset

genV1EndpointSubset :: Int -> Gen V1EndpointSubset
genV1EndpointSubset n =
  V1EndpointSubset
    <$> arbitraryReducedMaybe n -- v1EndpointSubsetAddresses :: Maybe [V1EndpointAddress]
    <*> arbitraryReducedMaybe n -- v1EndpointSubsetNotReadyAddresses :: Maybe [V1EndpointAddress]
    <*> arbitraryReducedMaybe n -- v1EndpointSubsetPorts :: Maybe [CoreV1EndpointPort]
  
instance Arbitrary V1Endpoints where
  arbitrary = sized genV1Endpoints

genV1Endpoints :: Int -> Gen V1Endpoints
genV1Endpoints n =
  V1Endpoints
    <$> arbitraryReducedMaybe n -- v1EndpointsApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EndpointsKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EndpointsMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1EndpointsSubsets :: Maybe [V1EndpointSubset]
  
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
  
instance Arbitrary V1EphemeralContainer where
  arbitrary = sized genV1EphemeralContainer

genV1EphemeralContainer :: Int -> Gen V1EphemeralContainer
genV1EphemeralContainer n =
  V1EphemeralContainer
    <$> arbitraryReducedMaybe n -- v1EphemeralContainerArgs :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerCommand :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerEnv :: Maybe [V1EnvVar]
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerEnvFrom :: Maybe [V1EnvFromSource]
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerImage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerImagePullPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerLifecycle :: Maybe V1Lifecycle
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerLivenessProbe :: Maybe V1Probe
    <*> arbitrary -- v1EphemeralContainerName :: Text
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerPorts :: Maybe [V1ContainerPort]
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerReadinessProbe :: Maybe V1Probe
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerResizePolicy :: Maybe [V1ContainerResizePolicy]
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerResources :: Maybe V1ResourceRequirements
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerRestartPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerSecurityContext :: Maybe V1SecurityContext
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerStartupProbe :: Maybe V1Probe
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerStdin :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerStdinOnce :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerTargetContainerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerTerminationMessagePath :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerTerminationMessagePolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerTty :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerVolumeDevices :: Maybe [V1VolumeDevice]
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerVolumeMounts :: Maybe [V1VolumeMount]
    <*> arbitraryReducedMaybe n -- v1EphemeralContainerWorkingDir :: Maybe Text
  
instance Arbitrary V1EphemeralVolumeSource where
  arbitrary = sized genV1EphemeralVolumeSource

genV1EphemeralVolumeSource :: Int -> Gen V1EphemeralVolumeSource
genV1EphemeralVolumeSource n =
  V1EphemeralVolumeSource
    <$> arbitraryReducedMaybe n -- v1EphemeralVolumeSourceVolumeClaimTemplate :: Maybe V1PersistentVolumeClaimTemplate
  
instance Arbitrary V1EventSource where
  arbitrary = sized genV1EventSource

genV1EventSource :: Int -> Gen V1EventSource
genV1EventSource n =
  V1EventSource
    <$> arbitraryReducedMaybe n -- v1EventSourceComponent :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EventSourceHost :: Maybe Text
  
instance Arbitrary V1Eviction where
  arbitrary = sized genV1Eviction

genV1Eviction :: Int -> Gen V1Eviction
genV1Eviction n =
  V1Eviction
    <$> arbitraryReducedMaybe n -- v1EvictionApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EvictionDeleteOptions :: Maybe V1DeleteOptions
    <*> arbitraryReducedMaybe n -- v1EvictionKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1EvictionMetadata :: Maybe V1ObjectMeta
  
instance Arbitrary V1ExecAction where
  arbitrary = sized genV1ExecAction

genV1ExecAction :: Int -> Gen V1ExecAction
genV1ExecAction n =
  V1ExecAction
    <$> arbitraryReducedMaybe n -- v1ExecActionCommand :: Maybe [Text]
  
instance Arbitrary V1ExemptPriorityLevelConfiguration where
  arbitrary = sized genV1ExemptPriorityLevelConfiguration

genV1ExemptPriorityLevelConfiguration :: Int -> Gen V1ExemptPriorityLevelConfiguration
genV1ExemptPriorityLevelConfiguration n =
  V1ExemptPriorityLevelConfiguration
    <$> arbitraryReducedMaybe n -- v1ExemptPriorityLevelConfigurationLendablePercent :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1ExemptPriorityLevelConfigurationNominalConcurrencyShares :: Maybe Int
  
instance Arbitrary V1ExternalDocumentation where
  arbitrary = sized genV1ExternalDocumentation

genV1ExternalDocumentation :: Int -> Gen V1ExternalDocumentation
genV1ExternalDocumentation n =
  V1ExternalDocumentation
    <$> arbitraryReducedMaybe n -- v1ExternalDocumentationDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ExternalDocumentationUrl :: Maybe Text
  
instance Arbitrary V1FCVolumeSource where
  arbitrary = sized genV1FCVolumeSource

genV1FCVolumeSource :: Int -> Gen V1FCVolumeSource
genV1FCVolumeSource n =
  V1FCVolumeSource
    <$> arbitraryReducedMaybe n -- v1FCVolumeSourceFsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1FCVolumeSourceLun :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1FCVolumeSourceReadOnly :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1FCVolumeSourceTargetWwns :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1FCVolumeSourceWwids :: Maybe [Text]
  
instance Arbitrary V1FlexPersistentVolumeSource where
  arbitrary = sized genV1FlexPersistentVolumeSource

genV1FlexPersistentVolumeSource :: Int -> Gen V1FlexPersistentVolumeSource
genV1FlexPersistentVolumeSource n =
  V1FlexPersistentVolumeSource
    <$> arbitrary -- v1FlexPersistentVolumeSourceDriver :: Text
    <*> arbitraryReducedMaybe n -- v1FlexPersistentVolumeSourceFsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1FlexPersistentVolumeSourceOptions :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1FlexPersistentVolumeSourceReadOnly :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1FlexPersistentVolumeSourceSecretRef :: Maybe V1SecretReference
  
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
  
instance Arbitrary V1FlowDistinguisherMethod where
  arbitrary = sized genV1FlowDistinguisherMethod

genV1FlowDistinguisherMethod :: Int -> Gen V1FlowDistinguisherMethod
genV1FlowDistinguisherMethod n =
  V1FlowDistinguisherMethod
    <$> arbitrary -- v1FlowDistinguisherMethodType :: Text
  
instance Arbitrary V1FlowSchema where
  arbitrary = sized genV1FlowSchema

genV1FlowSchema :: Int -> Gen V1FlowSchema
genV1FlowSchema n =
  V1FlowSchema
    <$> arbitraryReducedMaybe n -- v1FlowSchemaApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1FlowSchemaKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1FlowSchemaMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1FlowSchemaSpec :: Maybe V1FlowSchemaSpec
    <*> arbitraryReducedMaybe n -- v1FlowSchemaStatus :: Maybe V1FlowSchemaStatus
  
instance Arbitrary V1FlowSchemaCondition where
  arbitrary = sized genV1FlowSchemaCondition

genV1FlowSchemaCondition :: Int -> Gen V1FlowSchemaCondition
genV1FlowSchemaCondition n =
  V1FlowSchemaCondition
    <$> arbitraryReducedMaybe n -- v1FlowSchemaConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1FlowSchemaConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1FlowSchemaConditionReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1FlowSchemaConditionStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1FlowSchemaConditionType :: Maybe Text
  
instance Arbitrary V1FlowSchemaList where
  arbitrary = sized genV1FlowSchemaList

genV1FlowSchemaList :: Int -> Gen V1FlowSchemaList
genV1FlowSchemaList n =
  V1FlowSchemaList
    <$> arbitraryReducedMaybe n -- v1FlowSchemaListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1FlowSchemaListItems :: [V1FlowSchema]
    <*> arbitraryReducedMaybe n -- v1FlowSchemaListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1FlowSchemaListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1FlowSchemaSpec where
  arbitrary = sized genV1FlowSchemaSpec

genV1FlowSchemaSpec :: Int -> Gen V1FlowSchemaSpec
genV1FlowSchemaSpec n =
  V1FlowSchemaSpec
    <$> arbitraryReducedMaybe n -- v1FlowSchemaSpecDistinguisherMethod :: Maybe V1FlowDistinguisherMethod
    <*> arbitraryReducedMaybe n -- v1FlowSchemaSpecMatchingPrecedence :: Maybe Int
    <*> arbitraryReduced n -- v1FlowSchemaSpecPriorityLevelConfiguration :: V1PriorityLevelConfigurationReference
    <*> arbitraryReducedMaybe n -- v1FlowSchemaSpecRules :: Maybe [V1PolicyRulesWithSubjects]
  
instance Arbitrary V1FlowSchemaStatus where
  arbitrary = sized genV1FlowSchemaStatus

genV1FlowSchemaStatus :: Int -> Gen V1FlowSchemaStatus
genV1FlowSchemaStatus n =
  V1FlowSchemaStatus
    <$> arbitraryReducedMaybe n -- v1FlowSchemaStatusConditions :: Maybe [V1FlowSchemaCondition]
  
instance Arbitrary V1ForZone where
  arbitrary = sized genV1ForZone

genV1ForZone :: Int -> Gen V1ForZone
genV1ForZone n =
  V1ForZone
    <$> arbitrary -- v1ForZoneName :: Text
  
instance Arbitrary V1GCEPersistentDiskVolumeSource where
  arbitrary = sized genV1GCEPersistentDiskVolumeSource

genV1GCEPersistentDiskVolumeSource :: Int -> Gen V1GCEPersistentDiskVolumeSource
genV1GCEPersistentDiskVolumeSource n =
  V1GCEPersistentDiskVolumeSource
    <$> arbitraryReducedMaybe n -- v1GCEPersistentDiskVolumeSourceFsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1GCEPersistentDiskVolumeSourcePartition :: Maybe Int
    <*> arbitrary -- v1GCEPersistentDiskVolumeSourcePdName :: Text
    <*> arbitraryReducedMaybe n -- v1GCEPersistentDiskVolumeSourceReadOnly :: Maybe Bool
  
instance Arbitrary V1GRPCAction where
  arbitrary = sized genV1GRPCAction

genV1GRPCAction :: Int -> Gen V1GRPCAction
genV1GRPCAction n =
  V1GRPCAction
    <$> arbitrary -- v1GRPCActionPort :: Int
    <*> arbitraryReducedMaybe n -- v1GRPCActionService :: Maybe Text
  
instance Arbitrary V1GitRepoVolumeSource where
  arbitrary = sized genV1GitRepoVolumeSource

genV1GitRepoVolumeSource :: Int -> Gen V1GitRepoVolumeSource
genV1GitRepoVolumeSource n =
  V1GitRepoVolumeSource
    <$> arbitraryReducedMaybe n -- v1GitRepoVolumeSourceDirectory :: Maybe Text
    <*> arbitrary -- v1GitRepoVolumeSourceRepository :: Text
    <*> arbitraryReducedMaybe n -- v1GitRepoVolumeSourceRevision :: Maybe Text
  
instance Arbitrary V1GlusterfsPersistentVolumeSource where
  arbitrary = sized genV1GlusterfsPersistentVolumeSource

genV1GlusterfsPersistentVolumeSource :: Int -> Gen V1GlusterfsPersistentVolumeSource
genV1GlusterfsPersistentVolumeSource n =
  V1GlusterfsPersistentVolumeSource
    <$> arbitrary -- v1GlusterfsPersistentVolumeSourceEndpoints :: Text
    <*> arbitraryReducedMaybe n -- v1GlusterfsPersistentVolumeSourceEndpointsNamespace :: Maybe Text
    <*> arbitrary -- v1GlusterfsPersistentVolumeSourcePath :: Text
    <*> arbitraryReducedMaybe n -- v1GlusterfsPersistentVolumeSourceReadOnly :: Maybe Bool
  
instance Arbitrary V1GlusterfsVolumeSource where
  arbitrary = sized genV1GlusterfsVolumeSource

genV1GlusterfsVolumeSource :: Int -> Gen V1GlusterfsVolumeSource
genV1GlusterfsVolumeSource n =
  V1GlusterfsVolumeSource
    <$> arbitrary -- v1GlusterfsVolumeSourceEndpoints :: Text
    <*> arbitrary -- v1GlusterfsVolumeSourcePath :: Text
    <*> arbitraryReducedMaybe n -- v1GlusterfsVolumeSourceReadOnly :: Maybe Bool
  
instance Arbitrary V1GroupSubject where
  arbitrary = sized genV1GroupSubject

genV1GroupSubject :: Int -> Gen V1GroupSubject
genV1GroupSubject n =
  V1GroupSubject
    <$> arbitrary -- v1GroupSubjectName :: Text
  
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
    <*> arbitraryReduced n -- v1HTTPGetActionPort :: IntOrString
    <*> arbitraryReducedMaybe n -- v1HTTPGetActionScheme :: Maybe Text
  
instance Arbitrary V1HTTPHeader where
  arbitrary = sized genV1HTTPHeader

genV1HTTPHeader :: Int -> Gen V1HTTPHeader
genV1HTTPHeader n =
  V1HTTPHeader
    <$> arbitrary -- v1HTTPHeaderName :: Text
    <*> arbitrary -- v1HTTPHeaderValue :: Text
  
instance Arbitrary V1HTTPIngressPath where
  arbitrary = sized genV1HTTPIngressPath

genV1HTTPIngressPath :: Int -> Gen V1HTTPIngressPath
genV1HTTPIngressPath n =
  V1HTTPIngressPath
    <$> arbitraryReduced n -- v1HTTPIngressPathBackend :: V1IngressBackend
    <*> arbitraryReducedMaybe n -- v1HTTPIngressPathPath :: Maybe Text
    <*> arbitrary -- v1HTTPIngressPathPathType :: Text
  
instance Arbitrary V1HTTPIngressRuleValue where
  arbitrary = sized genV1HTTPIngressRuleValue

genV1HTTPIngressRuleValue :: Int -> Gen V1HTTPIngressRuleValue
genV1HTTPIngressRuleValue n =
  V1HTTPIngressRuleValue
    <$> arbitraryReduced n -- v1HTTPIngressRuleValuePaths :: [V1HTTPIngressPath]
  
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
  
instance Arbitrary V1HostIP where
  arbitrary = sized genV1HostIP

genV1HostIP :: Int -> Gen V1HostIP
genV1HostIP n =
  V1HostIP
    <$> arbitraryReducedMaybe n -- v1HostIPIp :: Maybe Text
  
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
  
instance Arbitrary V1Ingress where
  arbitrary = sized genV1Ingress

genV1Ingress :: Int -> Gen V1Ingress
genV1Ingress n =
  V1Ingress
    <$> arbitraryReducedMaybe n -- v1IngressApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1IngressKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1IngressMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1IngressSpec :: Maybe V1IngressSpec
    <*> arbitraryReducedMaybe n -- v1IngressStatus :: Maybe V1IngressStatus
  
instance Arbitrary V1IngressBackend where
  arbitrary = sized genV1IngressBackend

genV1IngressBackend :: Int -> Gen V1IngressBackend
genV1IngressBackend n =
  V1IngressBackend
    <$> arbitraryReducedMaybe n -- v1IngressBackendResource :: Maybe V1TypedLocalObjectReference
    <*> arbitraryReducedMaybe n -- v1IngressBackendService :: Maybe V1IngressServiceBackend
  
instance Arbitrary V1IngressClass where
  arbitrary = sized genV1IngressClass

genV1IngressClass :: Int -> Gen V1IngressClass
genV1IngressClass n =
  V1IngressClass
    <$> arbitraryReducedMaybe n -- v1IngressClassApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1IngressClassKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1IngressClassMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1IngressClassSpec :: Maybe V1IngressClassSpec
  
instance Arbitrary V1IngressClassList where
  arbitrary = sized genV1IngressClassList

genV1IngressClassList :: Int -> Gen V1IngressClassList
genV1IngressClassList n =
  V1IngressClassList
    <$> arbitraryReducedMaybe n -- v1IngressClassListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1IngressClassListItems :: [V1IngressClass]
    <*> arbitraryReducedMaybe n -- v1IngressClassListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1IngressClassListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1IngressClassParametersReference where
  arbitrary = sized genV1IngressClassParametersReference

genV1IngressClassParametersReference :: Int -> Gen V1IngressClassParametersReference
genV1IngressClassParametersReference n =
  V1IngressClassParametersReference
    <$> arbitraryReducedMaybe n -- v1IngressClassParametersReferenceApiGroup :: Maybe Text
    <*> arbitrary -- v1IngressClassParametersReferenceKind :: Text
    <*> arbitrary -- v1IngressClassParametersReferenceName :: Text
    <*> arbitraryReducedMaybe n -- v1IngressClassParametersReferenceNamespace :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1IngressClassParametersReferenceScope :: Maybe Text
  
instance Arbitrary V1IngressClassSpec where
  arbitrary = sized genV1IngressClassSpec

genV1IngressClassSpec :: Int -> Gen V1IngressClassSpec
genV1IngressClassSpec n =
  V1IngressClassSpec
    <$> arbitraryReducedMaybe n -- v1IngressClassSpecController :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1IngressClassSpecParameters :: Maybe V1IngressClassParametersReference
  
instance Arbitrary V1IngressList where
  arbitrary = sized genV1IngressList

genV1IngressList :: Int -> Gen V1IngressList
genV1IngressList n =
  V1IngressList
    <$> arbitraryReducedMaybe n -- v1IngressListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1IngressListItems :: [V1Ingress]
    <*> arbitraryReducedMaybe n -- v1IngressListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1IngressListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1IngressLoadBalancerIngress where
  arbitrary = sized genV1IngressLoadBalancerIngress

genV1IngressLoadBalancerIngress :: Int -> Gen V1IngressLoadBalancerIngress
genV1IngressLoadBalancerIngress n =
  V1IngressLoadBalancerIngress
    <$> arbitraryReducedMaybe n -- v1IngressLoadBalancerIngressHostname :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1IngressLoadBalancerIngressIp :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1IngressLoadBalancerIngressPorts :: Maybe [V1IngressPortStatus]
  
instance Arbitrary V1IngressLoadBalancerStatus where
  arbitrary = sized genV1IngressLoadBalancerStatus

genV1IngressLoadBalancerStatus :: Int -> Gen V1IngressLoadBalancerStatus
genV1IngressLoadBalancerStatus n =
  V1IngressLoadBalancerStatus
    <$> arbitraryReducedMaybe n -- v1IngressLoadBalancerStatusIngress :: Maybe [V1IngressLoadBalancerIngress]
  
instance Arbitrary V1IngressPortStatus where
  arbitrary = sized genV1IngressPortStatus

genV1IngressPortStatus :: Int -> Gen V1IngressPortStatus
genV1IngressPortStatus n =
  V1IngressPortStatus
    <$> arbitraryReducedMaybe n -- v1IngressPortStatusError :: Maybe Text
    <*> arbitrary -- v1IngressPortStatusPort :: Int
    <*> arbitrary -- v1IngressPortStatusProtocol :: Text
  
instance Arbitrary V1IngressRule where
  arbitrary = sized genV1IngressRule

genV1IngressRule :: Int -> Gen V1IngressRule
genV1IngressRule n =
  V1IngressRule
    <$> arbitraryReducedMaybe n -- v1IngressRuleHost :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1IngressRuleHttp :: Maybe V1HTTPIngressRuleValue
  
instance Arbitrary V1IngressServiceBackend where
  arbitrary = sized genV1IngressServiceBackend

genV1IngressServiceBackend :: Int -> Gen V1IngressServiceBackend
genV1IngressServiceBackend n =
  V1IngressServiceBackend
    <$> arbitrary -- v1IngressServiceBackendName :: Text
    <*> arbitraryReducedMaybe n -- v1IngressServiceBackendPort :: Maybe V1ServiceBackendPort
  
instance Arbitrary V1IngressSpec where
  arbitrary = sized genV1IngressSpec

genV1IngressSpec :: Int -> Gen V1IngressSpec
genV1IngressSpec n =
  V1IngressSpec
    <$> arbitraryReducedMaybe n -- v1IngressSpecDefaultBackend :: Maybe V1IngressBackend
    <*> arbitraryReducedMaybe n -- v1IngressSpecIngressClassName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1IngressSpecRules :: Maybe [V1IngressRule]
    <*> arbitraryReducedMaybe n -- v1IngressSpecTls :: Maybe [V1IngressTLS]
  
instance Arbitrary V1IngressStatus where
  arbitrary = sized genV1IngressStatus

genV1IngressStatus :: Int -> Gen V1IngressStatus
genV1IngressStatus n =
  V1IngressStatus
    <$> arbitraryReducedMaybe n -- v1IngressStatusLoadBalancer :: Maybe V1IngressLoadBalancerStatus
  
instance Arbitrary V1IngressTLS where
  arbitrary = sized genV1IngressTLS

genV1IngressTLS :: Int -> Gen V1IngressTLS
genV1IngressTLS n =
  V1IngressTLS
    <$> arbitraryReducedMaybe n -- v1IngressTLSHosts :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1IngressTLSSecretName :: Maybe Text
  
instance Arbitrary V1JSONSchemaProps where
  arbitrary = sized genV1JSONSchemaProps

genV1JSONSchemaProps :: Int -> Gen V1JSONSchemaProps
genV1JSONSchemaProps n =
  V1JSONSchemaProps
    <$> arbitraryReducedMaybe n -- v1JSONSchemaPropsRef :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsSchema :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- v1JSONSchemaPropsAdditionalItems :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- v1JSONSchemaPropsAdditionalProperties :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsAllOf :: Maybe [V1JSONSchemaProps]
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsAnyOf :: Maybe [V1JSONSchemaProps]
    <*> arbitraryReducedMaybeValue n -- v1JSONSchemaPropsDefault :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsDefinitions :: Maybe (Map.Map String V1JSONSchemaProps)
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsDependencies :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsEnum :: Maybe [A.Value]
    <*> arbitraryReducedMaybeValue n -- v1JSONSchemaPropsExample :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsExclusiveMaximum :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsExclusiveMinimum :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsExternalDocs :: Maybe V1ExternalDocumentation
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsFormat :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsId :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- v1JSONSchemaPropsItems :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsMaxItems :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsMaxLength :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsMaxProperties :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsMaximum :: Maybe Double
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsMinItems :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsMinLength :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsMinProperties :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsMinimum :: Maybe Double
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsMultipleOf :: Maybe Double
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsNot :: Maybe V1JSONSchemaProps
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsNullable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsOneOf :: Maybe [V1JSONSchemaProps]
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsPattern :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsPatternProperties :: Maybe (Map.Map String V1JSONSchemaProps)
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsProperties :: Maybe (Map.Map String V1JSONSchemaProps)
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsRequired :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsUniqueItems :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsXKubernetesEmbeddedResource :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsXKubernetesIntOrString :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsXKubernetesListMapKeys :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsXKubernetesListType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsXKubernetesMapType :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsXKubernetesPreserveUnknownFields :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1JSONSchemaPropsXKubernetesValidations :: Maybe [V1ValidationRule]
  
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
    <*> arbitraryReducedMaybe n -- v1JobSpecBackoffLimitPerIndex :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1JobSpecCompletionMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1JobSpecCompletions :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1JobSpecManualSelector :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1JobSpecMaxFailedIndexes :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1JobSpecParallelism :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1JobSpecPodFailurePolicy :: Maybe V1PodFailurePolicy
    <*> arbitraryReducedMaybe n -- v1JobSpecPodReplacementPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1JobSpecSelector :: Maybe V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1JobSpecSuspend :: Maybe Bool
    <*> arbitraryReduced n -- v1JobSpecTemplate :: V1PodTemplateSpec
    <*> arbitraryReducedMaybe n -- v1JobSpecTtlSecondsAfterFinished :: Maybe Int
  
instance Arbitrary V1JobStatus where
  arbitrary = sized genV1JobStatus

genV1JobStatus :: Int -> Gen V1JobStatus
genV1JobStatus n =
  V1JobStatus
    <$> arbitraryReducedMaybe n -- v1JobStatusActive :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1JobStatusCompletedIndexes :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1JobStatusCompletionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1JobStatusConditions :: Maybe [V1JobCondition]
    <*> arbitraryReducedMaybe n -- v1JobStatusFailed :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1JobStatusFailedIndexes :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1JobStatusReady :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1JobStatusStartTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1JobStatusSucceeded :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1JobStatusTerminating :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1JobStatusUncountedTerminatedPods :: Maybe V1UncountedTerminatedPods
  
instance Arbitrary V1JobTemplateSpec where
  arbitrary = sized genV1JobTemplateSpec

genV1JobTemplateSpec :: Int -> Gen V1JobTemplateSpec
genV1JobTemplateSpec n =
  V1JobTemplateSpec
    <$> arbitraryReducedMaybe n -- v1JobTemplateSpecMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1JobTemplateSpecSpec :: Maybe V1JobSpec
  
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
  
instance Arbitrary V1Lease where
  arbitrary = sized genV1Lease

genV1Lease :: Int -> Gen V1Lease
genV1Lease n =
  V1Lease
    <$> arbitraryReducedMaybe n -- v1LeaseApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1LeaseKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1LeaseMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1LeaseSpec :: Maybe V1LeaseSpec
  
instance Arbitrary V1LeaseList where
  arbitrary = sized genV1LeaseList

genV1LeaseList :: Int -> Gen V1LeaseList
genV1LeaseList n =
  V1LeaseList
    <$> arbitraryReducedMaybe n -- v1LeaseListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1LeaseListItems :: [V1Lease]
    <*> arbitraryReducedMaybe n -- v1LeaseListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1LeaseListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1LeaseSpec where
  arbitrary = sized genV1LeaseSpec

genV1LeaseSpec :: Int -> Gen V1LeaseSpec
genV1LeaseSpec n =
  V1LeaseSpec
    <$> arbitraryReducedMaybe n -- v1LeaseSpecAcquireTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1LeaseSpecHolderIdentity :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1LeaseSpecLeaseDurationSeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1LeaseSpecLeaseTransitions :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1LeaseSpecRenewTime :: Maybe DateTime
  
instance Arbitrary V1Lifecycle where
  arbitrary = sized genV1Lifecycle

genV1Lifecycle :: Int -> Gen V1Lifecycle
genV1Lifecycle n =
  V1Lifecycle
    <$> arbitraryReducedMaybe n -- v1LifecyclePostStart :: Maybe V1LifecycleHandler
    <*> arbitraryReducedMaybe n -- v1LifecyclePreStop :: Maybe V1LifecycleHandler
  
instance Arbitrary V1LifecycleHandler where
  arbitrary = sized genV1LifecycleHandler

genV1LifecycleHandler :: Int -> Gen V1LifecycleHandler
genV1LifecycleHandler n =
  V1LifecycleHandler
    <$> arbitraryReducedMaybe n -- v1LifecycleHandlerExec :: Maybe V1ExecAction
    <*> arbitraryReducedMaybe n -- v1LifecycleHandlerHttpGet :: Maybe V1HTTPGetAction
    <*> arbitraryReducedMaybe n -- v1LifecycleHandlerSleep :: Maybe V1SleepAction
    <*> arbitraryReducedMaybe n -- v1LifecycleHandlerTcpSocket :: Maybe V1TCPSocketAction
  
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
    <$> arbitraryReducedMaybe n -- v1LimitRangeItemDefault :: Maybe (Map.Map String Quantity)
    <*> arbitraryReducedMaybe n -- v1LimitRangeItemDefaultRequest :: Maybe (Map.Map String Quantity)
    <*> arbitraryReducedMaybe n -- v1LimitRangeItemMax :: Maybe (Map.Map String Quantity)
    <*> arbitraryReducedMaybe n -- v1LimitRangeItemMaxLimitRequestRatio :: Maybe (Map.Map String Quantity)
    <*> arbitraryReducedMaybe n -- v1LimitRangeItemMin :: Maybe (Map.Map String Quantity)
    <*> arbitrary -- v1LimitRangeItemType :: Text
  
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
  
instance Arbitrary V1LimitResponse where
  arbitrary = sized genV1LimitResponse

genV1LimitResponse :: Int -> Gen V1LimitResponse
genV1LimitResponse n =
  V1LimitResponse
    <$> arbitraryReducedMaybe n -- v1LimitResponseQueuing :: Maybe V1QueuingConfiguration
    <*> arbitrary -- v1LimitResponseType :: Text
  
instance Arbitrary V1LimitedPriorityLevelConfiguration where
  arbitrary = sized genV1LimitedPriorityLevelConfiguration

genV1LimitedPriorityLevelConfiguration :: Int -> Gen V1LimitedPriorityLevelConfiguration
genV1LimitedPriorityLevelConfiguration n =
  V1LimitedPriorityLevelConfiguration
    <$> arbitraryReducedMaybe n -- v1LimitedPriorityLevelConfigurationBorrowingLimitPercent :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1LimitedPriorityLevelConfigurationLendablePercent :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1LimitedPriorityLevelConfigurationLimitResponse :: Maybe V1LimitResponse
    <*> arbitraryReducedMaybe n -- v1LimitedPriorityLevelConfigurationNominalConcurrencyShares :: Maybe Int
  
instance Arbitrary V1ListMeta where
  arbitrary = sized genV1ListMeta

genV1ListMeta :: Int -> Gen V1ListMeta
genV1ListMeta n =
  V1ListMeta
    <$> arbitraryReducedMaybe n -- v1ListMetaContinue :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ListMetaRemainingItemCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1ListMetaResourceVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ListMetaSelfLink :: Maybe Text
  
instance Arbitrary V1LoadBalancerIngress where
  arbitrary = sized genV1LoadBalancerIngress

genV1LoadBalancerIngress :: Int -> Gen V1LoadBalancerIngress
genV1LoadBalancerIngress n =
  V1LoadBalancerIngress
    <$> arbitraryReducedMaybe n -- v1LoadBalancerIngressHostname :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1LoadBalancerIngressIp :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1LoadBalancerIngressIpMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1LoadBalancerIngressPorts :: Maybe [V1PortStatus]
  
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
    <$> arbitraryReducedMaybe n -- v1LocalVolumeSourceFsType :: Maybe Text
    <*> arbitrary -- v1LocalVolumeSourcePath :: Text
  
instance Arbitrary V1ManagedFieldsEntry where
  arbitrary = sized genV1ManagedFieldsEntry

genV1ManagedFieldsEntry :: Int -> Gen V1ManagedFieldsEntry
genV1ManagedFieldsEntry n =
  V1ManagedFieldsEntry
    <$> arbitraryReducedMaybe n -- v1ManagedFieldsEntryApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ManagedFieldsEntryFieldsType :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- v1ManagedFieldsEntryFieldsV1 :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- v1ManagedFieldsEntryManager :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ManagedFieldsEntryOperation :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ManagedFieldsEntrySubresource :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ManagedFieldsEntryTime :: Maybe DateTime
  
instance Arbitrary V1MatchCondition where
  arbitrary = sized genV1MatchCondition

genV1MatchCondition :: Int -> Gen V1MatchCondition
genV1MatchCondition n =
  V1MatchCondition
    <$> arbitrary -- v1MatchConditionExpression :: Text
    <*> arbitrary -- v1MatchConditionName :: Text
  
instance Arbitrary V1ModifyVolumeStatus where
  arbitrary = sized genV1ModifyVolumeStatus

genV1ModifyVolumeStatus :: Int -> Gen V1ModifyVolumeStatus
genV1ModifyVolumeStatus n =
  V1ModifyVolumeStatus
    <$> arbitrary -- v1ModifyVolumeStatusStatus :: Text
    <*> arbitraryReducedMaybe n -- v1ModifyVolumeStatusTargetVolumeAttributesClassName :: Maybe Text
  
instance Arbitrary V1MutatingWebhook where
  arbitrary = sized genV1MutatingWebhook

genV1MutatingWebhook :: Int -> Gen V1MutatingWebhook
genV1MutatingWebhook n =
  V1MutatingWebhook
    <$> arbitrary -- v1MutatingWebhookAdmissionReviewVersions :: [Text]
    <*> arbitraryReduced n -- v1MutatingWebhookClientConfig :: AdmissionregistrationV1WebhookClientConfig
    <*> arbitraryReducedMaybe n -- v1MutatingWebhookFailurePolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1MutatingWebhookMatchConditions :: Maybe [V1MatchCondition]
    <*> arbitraryReducedMaybe n -- v1MutatingWebhookMatchPolicy :: Maybe Text
    <*> arbitrary -- v1MutatingWebhookName :: Text
    <*> arbitraryReducedMaybe n -- v1MutatingWebhookNamespaceSelector :: Maybe V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1MutatingWebhookObjectSelector :: Maybe V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1MutatingWebhookReinvocationPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1MutatingWebhookRules :: Maybe [V1RuleWithOperations]
    <*> arbitrary -- v1MutatingWebhookSideEffects :: Text
    <*> arbitraryReducedMaybe n -- v1MutatingWebhookTimeoutSeconds :: Maybe Int
  
instance Arbitrary V1MutatingWebhookConfiguration where
  arbitrary = sized genV1MutatingWebhookConfiguration

genV1MutatingWebhookConfiguration :: Int -> Gen V1MutatingWebhookConfiguration
genV1MutatingWebhookConfiguration n =
  V1MutatingWebhookConfiguration
    <$> arbitraryReducedMaybe n -- v1MutatingWebhookConfigurationApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1MutatingWebhookConfigurationKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1MutatingWebhookConfigurationMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1MutatingWebhookConfigurationWebhooks :: Maybe [V1MutatingWebhook]
  
instance Arbitrary V1MutatingWebhookConfigurationList where
  arbitrary = sized genV1MutatingWebhookConfigurationList

genV1MutatingWebhookConfigurationList :: Int -> Gen V1MutatingWebhookConfigurationList
genV1MutatingWebhookConfigurationList n =
  V1MutatingWebhookConfigurationList
    <$> arbitraryReducedMaybe n -- v1MutatingWebhookConfigurationListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1MutatingWebhookConfigurationListItems :: [V1MutatingWebhookConfiguration]
    <*> arbitraryReducedMaybe n -- v1MutatingWebhookConfigurationListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1MutatingWebhookConfigurationListMetadata :: Maybe V1ListMeta
  
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
  
instance Arbitrary V1NamespaceCondition where
  arbitrary = sized genV1NamespaceCondition

genV1NamespaceCondition :: Int -> Gen V1NamespaceCondition
genV1NamespaceCondition n =
  V1NamespaceCondition
    <$> arbitraryReducedMaybe n -- v1NamespaceConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1NamespaceConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1NamespaceConditionReason :: Maybe Text
    <*> arbitrary -- v1NamespaceConditionStatus :: Text
    <*> arbitrary -- v1NamespaceConditionType :: Text
  
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
    <$> arbitraryReducedMaybe n -- v1NamespaceStatusConditions :: Maybe [V1NamespaceCondition]
    <*> arbitraryReducedMaybe n -- v1NamespaceStatusPhase :: Maybe Text
  
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
    <$> arbitraryReducedMaybe n -- v1NetworkPolicyPortEndPort :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1NetworkPolicyPortPort :: Maybe IntOrString
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
    <$> arbitraryReducedMaybe n -- v1NodeConfigSourceConfigMap :: Maybe V1ConfigMapNodeConfigSource
  
instance Arbitrary V1NodeConfigStatus where
  arbitrary = sized genV1NodeConfigStatus

genV1NodeConfigStatus :: Int -> Gen V1NodeConfigStatus
genV1NodeConfigStatus n =
  V1NodeConfigStatus
    <$> arbitraryReducedMaybe n -- v1NodeConfigStatusActive :: Maybe V1NodeConfigSource
    <*> arbitraryReducedMaybe n -- v1NodeConfigStatusAssigned :: Maybe V1NodeConfigSource
    <*> arbitraryReducedMaybe n -- v1NodeConfigStatusError :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1NodeConfigStatusLastKnownGood :: Maybe V1NodeConfigSource
  
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
    <$> arbitraryReducedMaybe n -- v1NodeSelectorTermMatchExpressions :: Maybe [V1NodeSelectorRequirement]
    <*> arbitraryReducedMaybe n -- v1NodeSelectorTermMatchFields :: Maybe [V1NodeSelectorRequirement]
  
instance Arbitrary V1NodeSpec where
  arbitrary = sized genV1NodeSpec

genV1NodeSpec :: Int -> Gen V1NodeSpec
genV1NodeSpec n =
  V1NodeSpec
    <$> arbitraryReducedMaybe n -- v1NodeSpecConfigSource :: Maybe V1NodeConfigSource
    <*> arbitraryReducedMaybe n -- v1NodeSpecExternalId :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1NodeSpecPodCidr :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1NodeSpecPodCidrs :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1NodeSpecProviderId :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1NodeSpecTaints :: Maybe [V1Taint]
    <*> arbitraryReducedMaybe n -- v1NodeSpecUnschedulable :: Maybe Bool
  
instance Arbitrary V1NodeStatus where
  arbitrary = sized genV1NodeStatus

genV1NodeStatus :: Int -> Gen V1NodeStatus
genV1NodeStatus n =
  V1NodeStatus
    <$> arbitraryReducedMaybe n -- v1NodeStatusAddresses :: Maybe [V1NodeAddress]
    <*> arbitraryReducedMaybe n -- v1NodeStatusAllocatable :: Maybe (Map.Map String Quantity)
    <*> arbitraryReducedMaybe n -- v1NodeStatusCapacity :: Maybe (Map.Map String Quantity)
    <*> arbitraryReducedMaybe n -- v1NodeStatusConditions :: Maybe [V1NodeCondition]
    <*> arbitraryReducedMaybe n -- v1NodeStatusConfig :: Maybe V1NodeConfigStatus
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
  
instance Arbitrary V1NonResourcePolicyRule where
  arbitrary = sized genV1NonResourcePolicyRule

genV1NonResourcePolicyRule :: Int -> Gen V1NonResourcePolicyRule
genV1NonResourcePolicyRule n =
  V1NonResourcePolicyRule
    <$> arbitrary -- v1NonResourcePolicyRuleNonResourceUrls :: [Text]
    <*> arbitrary -- v1NonResourcePolicyRuleVerbs :: [Text]
  
instance Arbitrary V1NonResourceRule where
  arbitrary = sized genV1NonResourceRule

genV1NonResourceRule :: Int -> Gen V1NonResourceRule
genV1NonResourceRule n =
  V1NonResourceRule
    <$> arbitraryReducedMaybe n -- v1NonResourceRuleNonResourceUrls :: Maybe [Text]
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
    <*> arbitraryReducedMaybe n -- v1ObjectMetaCreationTimestamp :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1ObjectMetaDeletionGracePeriodSeconds :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1ObjectMetaDeletionTimestamp :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1ObjectMetaFinalizers :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1ObjectMetaGenerateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ObjectMetaGeneration :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1ObjectMetaLabels :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1ObjectMetaManagedFields :: Maybe [V1ManagedFieldsEntry]
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
  
instance Arbitrary V1Overhead where
  arbitrary = sized genV1Overhead

genV1Overhead :: Int -> Gen V1Overhead
genV1Overhead n =
  V1Overhead
    <$> arbitraryReducedMaybe n -- v1OverheadPodFixed :: Maybe (Map.Map String Quantity)
  
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
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimSpecDataSource :: Maybe V1TypedLocalObjectReference
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimSpecDataSourceRef :: Maybe V1TypedObjectReference
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimSpecResources :: Maybe V1VolumeResourceRequirements
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimSpecSelector :: Maybe V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimSpecStorageClassName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimSpecVolumeAttributesClassName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimSpecVolumeMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimSpecVolumeName :: Maybe Text
  
instance Arbitrary V1PersistentVolumeClaimStatus where
  arbitrary = sized genV1PersistentVolumeClaimStatus

genV1PersistentVolumeClaimStatus :: Int -> Gen V1PersistentVolumeClaimStatus
genV1PersistentVolumeClaimStatus n =
  V1PersistentVolumeClaimStatus
    <$> arbitraryReducedMaybe n -- v1PersistentVolumeClaimStatusAccessModes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimStatusAllocatedResourceStatuses :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimStatusAllocatedResources :: Maybe (Map.Map String Quantity)
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimStatusCapacity :: Maybe (Map.Map String Quantity)
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimStatusConditions :: Maybe [V1PersistentVolumeClaimCondition]
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimStatusCurrentVolumeAttributesClassName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimStatusModifyVolumeStatus :: Maybe V1ModifyVolumeStatus
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeClaimStatusPhase :: Maybe Text
  
instance Arbitrary V1PersistentVolumeClaimTemplate where
  arbitrary = sized genV1PersistentVolumeClaimTemplate

genV1PersistentVolumeClaimTemplate :: Int -> Gen V1PersistentVolumeClaimTemplate
genV1PersistentVolumeClaimTemplate n =
  V1PersistentVolumeClaimTemplate
    <$> arbitraryReducedMaybe n -- v1PersistentVolumeClaimTemplateMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1PersistentVolumeClaimTemplateSpec :: V1PersistentVolumeClaimSpec
  
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
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecCapacity :: Maybe (Map.Map String Quantity)
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecCephfs :: Maybe V1CephFSPersistentVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecCinder :: Maybe V1CinderPersistentVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecClaimRef :: Maybe V1ObjectReference
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecCsi :: Maybe V1CSIPersistentVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecFc :: Maybe V1FCVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecFlexVolume :: Maybe V1FlexPersistentVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecFlocker :: Maybe V1FlockerVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecGcePersistentDisk :: Maybe V1GCEPersistentDiskVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecGlusterfs :: Maybe V1GlusterfsPersistentVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecHostPath :: Maybe V1HostPathVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecIscsi :: Maybe V1ISCSIPersistentVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecLocal :: Maybe V1LocalVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecMountOptions :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecNfs :: Maybe V1NFSVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecNodeAffinity :: Maybe V1VolumeNodeAffinity
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecPersistentVolumeReclaimPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecPhotonPersistentDisk :: Maybe V1PhotonPersistentDiskVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecPortworxVolume :: Maybe V1PortworxVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecQuobyte :: Maybe V1QuobyteVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecRbd :: Maybe V1RBDPersistentVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecScaleIo :: Maybe V1ScaleIOPersistentVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecStorageClassName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecStorageos :: Maybe V1StorageOSPersistentVolumeSource
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecVolumeAttributesClassName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecVolumeMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeSpecVsphereVolume :: Maybe V1VsphereVirtualDiskVolumeSource
  
instance Arbitrary V1PersistentVolumeStatus where
  arbitrary = sized genV1PersistentVolumeStatus

genV1PersistentVolumeStatus :: Int -> Gen V1PersistentVolumeStatus
genV1PersistentVolumeStatus n =
  V1PersistentVolumeStatus
    <$> arbitraryReducedMaybe n -- v1PersistentVolumeStatusLastPhaseTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1PersistentVolumeStatusMessage :: Maybe Text
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
    <*> arbitraryReducedMaybe n -- v1PodAffinityTermMatchLabelKeys :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1PodAffinityTermMismatchLabelKeys :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1PodAffinityTermNamespaceSelector :: Maybe V1LabelSelector
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
  
instance Arbitrary V1PodDisruptionBudget where
  arbitrary = sized genV1PodDisruptionBudget

genV1PodDisruptionBudget :: Int -> Gen V1PodDisruptionBudget
genV1PodDisruptionBudget n =
  V1PodDisruptionBudget
    <$> arbitraryReducedMaybe n -- v1PodDisruptionBudgetApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodDisruptionBudgetKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodDisruptionBudgetMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1PodDisruptionBudgetSpec :: Maybe V1PodDisruptionBudgetSpec
    <*> arbitraryReducedMaybe n -- v1PodDisruptionBudgetStatus :: Maybe V1PodDisruptionBudgetStatus
  
instance Arbitrary V1PodDisruptionBudgetList where
  arbitrary = sized genV1PodDisruptionBudgetList

genV1PodDisruptionBudgetList :: Int -> Gen V1PodDisruptionBudgetList
genV1PodDisruptionBudgetList n =
  V1PodDisruptionBudgetList
    <$> arbitraryReducedMaybe n -- v1PodDisruptionBudgetListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1PodDisruptionBudgetListItems :: [V1PodDisruptionBudget]
    <*> arbitraryReducedMaybe n -- v1PodDisruptionBudgetListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodDisruptionBudgetListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1PodDisruptionBudgetSpec where
  arbitrary = sized genV1PodDisruptionBudgetSpec

genV1PodDisruptionBudgetSpec :: Int -> Gen V1PodDisruptionBudgetSpec
genV1PodDisruptionBudgetSpec n =
  V1PodDisruptionBudgetSpec
    <$> arbitraryReducedMaybe n -- v1PodDisruptionBudgetSpecMaxUnavailable :: Maybe IntOrString
    <*> arbitraryReducedMaybe n -- v1PodDisruptionBudgetSpecMinAvailable :: Maybe IntOrString
    <*> arbitraryReducedMaybe n -- v1PodDisruptionBudgetSpecSelector :: Maybe V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1PodDisruptionBudgetSpecUnhealthyPodEvictionPolicy :: Maybe Text
  
instance Arbitrary V1PodDisruptionBudgetStatus where
  arbitrary = sized genV1PodDisruptionBudgetStatus

genV1PodDisruptionBudgetStatus :: Int -> Gen V1PodDisruptionBudgetStatus
genV1PodDisruptionBudgetStatus n =
  V1PodDisruptionBudgetStatus
    <$> arbitraryReducedMaybe n -- v1PodDisruptionBudgetStatusConditions :: Maybe [V1Condition]
    <*> arbitrary -- v1PodDisruptionBudgetStatusCurrentHealthy :: Int
    <*> arbitrary -- v1PodDisruptionBudgetStatusDesiredHealthy :: Int
    <*> arbitraryReducedMaybe n -- v1PodDisruptionBudgetStatusDisruptedPods :: Maybe (Map.Map String DateTime)
    <*> arbitrary -- v1PodDisruptionBudgetStatusDisruptionsAllowed :: Int
    <*> arbitrary -- v1PodDisruptionBudgetStatusExpectedPods :: Int
    <*> arbitraryReducedMaybe n -- v1PodDisruptionBudgetStatusObservedGeneration :: Maybe Integer
  
instance Arbitrary V1PodFailurePolicy where
  arbitrary = sized genV1PodFailurePolicy

genV1PodFailurePolicy :: Int -> Gen V1PodFailurePolicy
genV1PodFailurePolicy n =
  V1PodFailurePolicy
    <$> arbitraryReduced n -- v1PodFailurePolicyRules :: [V1PodFailurePolicyRule]
  
instance Arbitrary V1PodFailurePolicyOnExitCodesRequirement where
  arbitrary = sized genV1PodFailurePolicyOnExitCodesRequirement

genV1PodFailurePolicyOnExitCodesRequirement :: Int -> Gen V1PodFailurePolicyOnExitCodesRequirement
genV1PodFailurePolicyOnExitCodesRequirement n =
  V1PodFailurePolicyOnExitCodesRequirement
    <$> arbitraryReducedMaybe n -- v1PodFailurePolicyOnExitCodesRequirementContainerName :: Maybe Text
    <*> arbitrary -- v1PodFailurePolicyOnExitCodesRequirementOperator :: Text
    <*> arbitrary -- v1PodFailurePolicyOnExitCodesRequirementValues :: [Int]
  
instance Arbitrary V1PodFailurePolicyOnPodConditionsPattern where
  arbitrary = sized genV1PodFailurePolicyOnPodConditionsPattern

genV1PodFailurePolicyOnPodConditionsPattern :: Int -> Gen V1PodFailurePolicyOnPodConditionsPattern
genV1PodFailurePolicyOnPodConditionsPattern n =
  V1PodFailurePolicyOnPodConditionsPattern
    <$> arbitrary -- v1PodFailurePolicyOnPodConditionsPatternStatus :: Text
    <*> arbitrary -- v1PodFailurePolicyOnPodConditionsPatternType :: Text
  
instance Arbitrary V1PodFailurePolicyRule where
  arbitrary = sized genV1PodFailurePolicyRule

genV1PodFailurePolicyRule :: Int -> Gen V1PodFailurePolicyRule
genV1PodFailurePolicyRule n =
  V1PodFailurePolicyRule
    <$> arbitrary -- v1PodFailurePolicyRuleAction :: Text
    <*> arbitraryReducedMaybe n -- v1PodFailurePolicyRuleOnExitCodes :: Maybe V1PodFailurePolicyOnExitCodesRequirement
    <*> arbitraryReducedMaybe n -- v1PodFailurePolicyRuleOnPodConditions :: Maybe [V1PodFailurePolicyOnPodConditionsPattern]
  
instance Arbitrary V1PodIP where
  arbitrary = sized genV1PodIP

genV1PodIP :: Int -> Gen V1PodIP
genV1PodIP n =
  V1PodIP
    <$> arbitraryReducedMaybe n -- v1PodIPIp :: Maybe Text
  
instance Arbitrary V1PodList where
  arbitrary = sized genV1PodList

genV1PodList :: Int -> Gen V1PodList
genV1PodList n =
  V1PodList
    <$> arbitraryReducedMaybe n -- v1PodListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1PodListItems :: [V1Pod]
    <*> arbitraryReducedMaybe n -- v1PodListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1PodOS where
  arbitrary = sized genV1PodOS

genV1PodOS :: Int -> Gen V1PodOS
genV1PodOS n =
  V1PodOS
    <$> arbitrary -- v1PodOSName :: Text
  
instance Arbitrary V1PodReadinessGate where
  arbitrary = sized genV1PodReadinessGate

genV1PodReadinessGate :: Int -> Gen V1PodReadinessGate
genV1PodReadinessGate n =
  V1PodReadinessGate
    <$> arbitrary -- v1PodReadinessGateConditionType :: Text
  
instance Arbitrary V1PodResourceClaim where
  arbitrary = sized genV1PodResourceClaim

genV1PodResourceClaim :: Int -> Gen V1PodResourceClaim
genV1PodResourceClaim n =
  V1PodResourceClaim
    <$> arbitrary -- v1PodResourceClaimName :: Text
    <*> arbitraryReducedMaybe n -- v1PodResourceClaimSource :: Maybe V1ClaimSource
  
instance Arbitrary V1PodResourceClaimStatus where
  arbitrary = sized genV1PodResourceClaimStatus

genV1PodResourceClaimStatus :: Int -> Gen V1PodResourceClaimStatus
genV1PodResourceClaimStatus n =
  V1PodResourceClaimStatus
    <$> arbitrary -- v1PodResourceClaimStatusName :: Text
    <*> arbitraryReducedMaybe n -- v1PodResourceClaimStatusResourceClaimName :: Maybe Text
  
instance Arbitrary V1PodSchedulingGate where
  arbitrary = sized genV1PodSchedulingGate

genV1PodSchedulingGate :: Int -> Gen V1PodSchedulingGate
genV1PodSchedulingGate n =
  V1PodSchedulingGate
    <$> arbitrary -- v1PodSchedulingGateName :: Text
  
instance Arbitrary V1PodSecurityContext where
  arbitrary = sized genV1PodSecurityContext

genV1PodSecurityContext :: Int -> Gen V1PodSecurityContext
genV1PodSecurityContext n =
  V1PodSecurityContext
    <$> arbitraryReducedMaybe n -- v1PodSecurityContextFsGroup :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1PodSecurityContextFsGroupChangePolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodSecurityContextRunAsGroup :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1PodSecurityContextRunAsNonRoot :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1PodSecurityContextRunAsUser :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1PodSecurityContextSeLinuxOptions :: Maybe V1SELinuxOptions
    <*> arbitraryReducedMaybe n -- v1PodSecurityContextSeccompProfile :: Maybe V1SeccompProfile
    <*> arbitraryReducedMaybe n -- v1PodSecurityContextSupplementalGroups :: Maybe [Integer]
    <*> arbitraryReducedMaybe n -- v1PodSecurityContextSysctls :: Maybe [V1Sysctl]
    <*> arbitraryReducedMaybe n -- v1PodSecurityContextWindowsOptions :: Maybe V1WindowsSecurityContextOptions
  
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
    <*> arbitraryReducedMaybe n -- v1PodSpecEnableServiceLinks :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1PodSpecEphemeralContainers :: Maybe [V1EphemeralContainer]
    <*> arbitraryReducedMaybe n -- v1PodSpecHostAliases :: Maybe [V1HostAlias]
    <*> arbitraryReducedMaybe n -- v1PodSpecHostIpc :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1PodSpecHostNetwork :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1PodSpecHostPid :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1PodSpecHostUsers :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1PodSpecHostname :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodSpecImagePullSecrets :: Maybe [V1LocalObjectReference]
    <*> arbitraryReducedMaybe n -- v1PodSpecInitContainers :: Maybe [V1Container]
    <*> arbitraryReducedMaybe n -- v1PodSpecNodeName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodSpecNodeSelector :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1PodSpecOs :: Maybe V1PodOS
    <*> arbitraryReducedMaybe n -- v1PodSpecOverhead :: Maybe (Map.Map String Quantity)
    <*> arbitraryReducedMaybe n -- v1PodSpecPreemptionPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodSpecPriority :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1PodSpecPriorityClassName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodSpecReadinessGates :: Maybe [V1PodReadinessGate]
    <*> arbitraryReducedMaybe n -- v1PodSpecResourceClaims :: Maybe [V1PodResourceClaim]
    <*> arbitraryReducedMaybe n -- v1PodSpecRestartPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodSpecRuntimeClassName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodSpecSchedulerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodSpecSchedulingGates :: Maybe [V1PodSchedulingGate]
    <*> arbitraryReducedMaybe n -- v1PodSpecSecurityContext :: Maybe V1PodSecurityContext
    <*> arbitraryReducedMaybe n -- v1PodSpecServiceAccount :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodSpecServiceAccountName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodSpecSetHostnameAsFqdn :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1PodSpecShareProcessNamespace :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1PodSpecSubdomain :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodSpecTerminationGracePeriodSeconds :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1PodSpecTolerations :: Maybe [V1Toleration]
    <*> arbitraryReducedMaybe n -- v1PodSpecTopologySpreadConstraints :: Maybe [V1TopologySpreadConstraint]
    <*> arbitraryReducedMaybe n -- v1PodSpecVolumes :: Maybe [V1Volume]
  
instance Arbitrary V1PodStatus where
  arbitrary = sized genV1PodStatus

genV1PodStatus :: Int -> Gen V1PodStatus
genV1PodStatus n =
  V1PodStatus
    <$> arbitraryReducedMaybe n -- v1PodStatusConditions :: Maybe [V1PodCondition]
    <*> arbitraryReducedMaybe n -- v1PodStatusContainerStatuses :: Maybe [V1ContainerStatus]
    <*> arbitraryReducedMaybe n -- v1PodStatusEphemeralContainerStatuses :: Maybe [V1ContainerStatus]
    <*> arbitraryReducedMaybe n -- v1PodStatusHostIp :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodStatusHostIps :: Maybe [V1HostIP]
    <*> arbitraryReducedMaybe n -- v1PodStatusInitContainerStatuses :: Maybe [V1ContainerStatus]
    <*> arbitraryReducedMaybe n -- v1PodStatusMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodStatusNominatedNodeName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodStatusPhase :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodStatusPodIp :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodStatusPodIps :: Maybe [V1PodIP]
    <*> arbitraryReducedMaybe n -- v1PodStatusQosClass :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodStatusReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodStatusResize :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PodStatusResourceClaimStatuses :: Maybe [V1PodResourceClaimStatus]
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
    <*> arbitraryReducedMaybe n -- v1PolicyRuleNonResourceUrls :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1PolicyRuleResourceNames :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1PolicyRuleResources :: Maybe [Text]
    <*> arbitrary -- v1PolicyRuleVerbs :: [Text]
  
instance Arbitrary V1PolicyRulesWithSubjects where
  arbitrary = sized genV1PolicyRulesWithSubjects

genV1PolicyRulesWithSubjects :: Int -> Gen V1PolicyRulesWithSubjects
genV1PolicyRulesWithSubjects n =
  V1PolicyRulesWithSubjects
    <$> arbitraryReducedMaybe n -- v1PolicyRulesWithSubjectsNonResourceRules :: Maybe [V1NonResourcePolicyRule]
    <*> arbitraryReducedMaybe n -- v1PolicyRulesWithSubjectsResourceRules :: Maybe [V1ResourcePolicyRule]
    <*> arbitraryReduced n -- v1PolicyRulesWithSubjectsSubjects :: [FlowcontrolV1Subject]
  
instance Arbitrary V1PortStatus where
  arbitrary = sized genV1PortStatus

genV1PortStatus :: Int -> Gen V1PortStatus
genV1PortStatus n =
  V1PortStatus
    <$> arbitraryReducedMaybe n -- v1PortStatusError :: Maybe Text
    <*> arbitrary -- v1PortStatusPort :: Int
    <*> arbitrary -- v1PortStatusProtocol :: Text
  
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
    <$> arbitraryReducedMaybe n -- v1PreconditionsResourceVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PreconditionsUid :: Maybe Text
  
instance Arbitrary V1PreferredSchedulingTerm where
  arbitrary = sized genV1PreferredSchedulingTerm

genV1PreferredSchedulingTerm :: Int -> Gen V1PreferredSchedulingTerm
genV1PreferredSchedulingTerm n =
  V1PreferredSchedulingTerm
    <$> arbitraryReduced n -- v1PreferredSchedulingTermPreference :: V1NodeSelectorTerm
    <*> arbitrary -- v1PreferredSchedulingTermWeight :: Int
  
instance Arbitrary V1PriorityClass where
  arbitrary = sized genV1PriorityClass

genV1PriorityClass :: Int -> Gen V1PriorityClass
genV1PriorityClass n =
  V1PriorityClass
    <$> arbitraryReducedMaybe n -- v1PriorityClassApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PriorityClassDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PriorityClassGlobalDefault :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1PriorityClassKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PriorityClassMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1PriorityClassPreemptionPolicy :: Maybe Text
    <*> arbitrary -- v1PriorityClassValue :: Int
  
instance Arbitrary V1PriorityClassList where
  arbitrary = sized genV1PriorityClassList

genV1PriorityClassList :: Int -> Gen V1PriorityClassList
genV1PriorityClassList n =
  V1PriorityClassList
    <$> arbitraryReducedMaybe n -- v1PriorityClassListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1PriorityClassListItems :: [V1PriorityClass]
    <*> arbitraryReducedMaybe n -- v1PriorityClassListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PriorityClassListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1PriorityLevelConfiguration where
  arbitrary = sized genV1PriorityLevelConfiguration

genV1PriorityLevelConfiguration :: Int -> Gen V1PriorityLevelConfiguration
genV1PriorityLevelConfiguration n =
  V1PriorityLevelConfiguration
    <$> arbitraryReducedMaybe n -- v1PriorityLevelConfigurationApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PriorityLevelConfigurationKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PriorityLevelConfigurationMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1PriorityLevelConfigurationSpec :: Maybe V1PriorityLevelConfigurationSpec
    <*> arbitraryReducedMaybe n -- v1PriorityLevelConfigurationStatus :: Maybe V1PriorityLevelConfigurationStatus
  
instance Arbitrary V1PriorityLevelConfigurationCondition where
  arbitrary = sized genV1PriorityLevelConfigurationCondition

genV1PriorityLevelConfigurationCondition :: Int -> Gen V1PriorityLevelConfigurationCondition
genV1PriorityLevelConfigurationCondition n =
  V1PriorityLevelConfigurationCondition
    <$> arbitraryReducedMaybe n -- v1PriorityLevelConfigurationConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1PriorityLevelConfigurationConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PriorityLevelConfigurationConditionReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PriorityLevelConfigurationConditionStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PriorityLevelConfigurationConditionType :: Maybe Text
  
instance Arbitrary V1PriorityLevelConfigurationList where
  arbitrary = sized genV1PriorityLevelConfigurationList

genV1PriorityLevelConfigurationList :: Int -> Gen V1PriorityLevelConfigurationList
genV1PriorityLevelConfigurationList n =
  V1PriorityLevelConfigurationList
    <$> arbitraryReducedMaybe n -- v1PriorityLevelConfigurationListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1PriorityLevelConfigurationListItems :: [V1PriorityLevelConfiguration]
    <*> arbitraryReducedMaybe n -- v1PriorityLevelConfigurationListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1PriorityLevelConfigurationListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1PriorityLevelConfigurationReference where
  arbitrary = sized genV1PriorityLevelConfigurationReference

genV1PriorityLevelConfigurationReference :: Int -> Gen V1PriorityLevelConfigurationReference
genV1PriorityLevelConfigurationReference n =
  V1PriorityLevelConfigurationReference
    <$> arbitrary -- v1PriorityLevelConfigurationReferenceName :: Text
  
instance Arbitrary V1PriorityLevelConfigurationSpec where
  arbitrary = sized genV1PriorityLevelConfigurationSpec

genV1PriorityLevelConfigurationSpec :: Int -> Gen V1PriorityLevelConfigurationSpec
genV1PriorityLevelConfigurationSpec n =
  V1PriorityLevelConfigurationSpec
    <$> arbitraryReducedMaybe n -- v1PriorityLevelConfigurationSpecExempt :: Maybe V1ExemptPriorityLevelConfiguration
    <*> arbitraryReducedMaybe n -- v1PriorityLevelConfigurationSpecLimited :: Maybe V1LimitedPriorityLevelConfiguration
    <*> arbitrary -- v1PriorityLevelConfigurationSpecType :: Text
  
instance Arbitrary V1PriorityLevelConfigurationStatus where
  arbitrary = sized genV1PriorityLevelConfigurationStatus

genV1PriorityLevelConfigurationStatus :: Int -> Gen V1PriorityLevelConfigurationStatus
genV1PriorityLevelConfigurationStatus n =
  V1PriorityLevelConfigurationStatus
    <$> arbitraryReducedMaybe n -- v1PriorityLevelConfigurationStatusConditions :: Maybe [V1PriorityLevelConfigurationCondition]
  
instance Arbitrary V1Probe where
  arbitrary = sized genV1Probe

genV1Probe :: Int -> Gen V1Probe
genV1Probe n =
  V1Probe
    <$> arbitraryReducedMaybe n -- v1ProbeExec :: Maybe V1ExecAction
    <*> arbitraryReducedMaybe n -- v1ProbeFailureThreshold :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1ProbeGrpc :: Maybe V1GRPCAction
    <*> arbitraryReducedMaybe n -- v1ProbeHttpGet :: Maybe V1HTTPGetAction
    <*> arbitraryReducedMaybe n -- v1ProbeInitialDelaySeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1ProbePeriodSeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1ProbeSuccessThreshold :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1ProbeTcpSocket :: Maybe V1TCPSocketAction
    <*> arbitraryReducedMaybe n -- v1ProbeTerminationGracePeriodSeconds :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1ProbeTimeoutSeconds :: Maybe Int
  
instance Arbitrary V1ProjectedVolumeSource where
  arbitrary = sized genV1ProjectedVolumeSource

genV1ProjectedVolumeSource :: Int -> Gen V1ProjectedVolumeSource
genV1ProjectedVolumeSource n =
  V1ProjectedVolumeSource
    <$> arbitraryReducedMaybe n -- v1ProjectedVolumeSourceDefaultMode :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1ProjectedVolumeSourceSources :: Maybe [V1VolumeProjection]
  
instance Arbitrary V1QueuingConfiguration where
  arbitrary = sized genV1QueuingConfiguration

genV1QueuingConfiguration :: Int -> Gen V1QueuingConfiguration
genV1QueuingConfiguration n =
  V1QueuingConfiguration
    <$> arbitraryReducedMaybe n -- v1QueuingConfigurationHandSize :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1QueuingConfigurationQueueLengthLimit :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1QueuingConfigurationQueues :: Maybe Int
  
instance Arbitrary V1QuobyteVolumeSource where
  arbitrary = sized genV1QuobyteVolumeSource

genV1QuobyteVolumeSource :: Int -> Gen V1QuobyteVolumeSource
genV1QuobyteVolumeSource n =
  V1QuobyteVolumeSource
    <$> arbitraryReducedMaybe n -- v1QuobyteVolumeSourceGroup :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1QuobyteVolumeSourceReadOnly :: Maybe Bool
    <*> arbitrary -- v1QuobyteVolumeSourceRegistry :: Text
    <*> arbitraryReducedMaybe n -- v1QuobyteVolumeSourceTenant :: Maybe Text
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
  
instance Arbitrary V1ResourceClaim where
  arbitrary = sized genV1ResourceClaim

genV1ResourceClaim :: Int -> Gen V1ResourceClaim
genV1ResourceClaim n =
  V1ResourceClaim
    <$> arbitrary -- v1ResourceClaimName :: Text
  
instance Arbitrary V1ResourceFieldSelector where
  arbitrary = sized genV1ResourceFieldSelector

genV1ResourceFieldSelector :: Int -> Gen V1ResourceFieldSelector
genV1ResourceFieldSelector n =
  V1ResourceFieldSelector
    <$> arbitraryReducedMaybe n -- v1ResourceFieldSelectorContainerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ResourceFieldSelectorDivisor :: Maybe Quantity
    <*> arbitrary -- v1ResourceFieldSelectorResource :: Text
  
instance Arbitrary V1ResourcePolicyRule where
  arbitrary = sized genV1ResourcePolicyRule

genV1ResourcePolicyRule :: Int -> Gen V1ResourcePolicyRule
genV1ResourcePolicyRule n =
  V1ResourcePolicyRule
    <$> arbitrary -- v1ResourcePolicyRuleApiGroups :: [Text]
    <*> arbitraryReducedMaybe n -- v1ResourcePolicyRuleClusterScope :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1ResourcePolicyRuleNamespaces :: Maybe [Text]
    <*> arbitrary -- v1ResourcePolicyRuleResources :: [Text]
    <*> arbitrary -- v1ResourcePolicyRuleVerbs :: [Text]
  
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
    <$> arbitraryReducedMaybe n -- v1ResourceQuotaSpecHard :: Maybe (Map.Map String Quantity)
    <*> arbitraryReducedMaybe n -- v1ResourceQuotaSpecScopeSelector :: Maybe V1ScopeSelector
    <*> arbitraryReducedMaybe n -- v1ResourceQuotaSpecScopes :: Maybe [Text]
  
instance Arbitrary V1ResourceQuotaStatus where
  arbitrary = sized genV1ResourceQuotaStatus

genV1ResourceQuotaStatus :: Int -> Gen V1ResourceQuotaStatus
genV1ResourceQuotaStatus n =
  V1ResourceQuotaStatus
    <$> arbitraryReducedMaybe n -- v1ResourceQuotaStatusHard :: Maybe (Map.Map String Quantity)
    <*> arbitraryReducedMaybe n -- v1ResourceQuotaStatusUsed :: Maybe (Map.Map String Quantity)
  
instance Arbitrary V1ResourceRequirements where
  arbitrary = sized genV1ResourceRequirements

genV1ResourceRequirements :: Int -> Gen V1ResourceRequirements
genV1ResourceRequirements n =
  V1ResourceRequirements
    <$> arbitraryReducedMaybe n -- v1ResourceRequirementsClaims :: Maybe [V1ResourceClaim]
    <*> arbitraryReducedMaybe n -- v1ResourceRequirementsLimits :: Maybe (Map.Map String Quantity)
    <*> arbitraryReducedMaybe n -- v1ResourceRequirementsRequests :: Maybe (Map.Map String Quantity)
  
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
    <*> arbitraryReducedMaybe n -- v1RoleRules :: Maybe [V1PolicyRule]
  
instance Arbitrary V1RoleBinding where
  arbitrary = sized genV1RoleBinding

genV1RoleBinding :: Int -> Gen V1RoleBinding
genV1RoleBinding n =
  V1RoleBinding
    <$> arbitraryReducedMaybe n -- v1RoleBindingApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1RoleBindingKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1RoleBindingMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1RoleBindingRoleRef :: V1RoleRef
    <*> arbitraryReducedMaybe n -- v1RoleBindingSubjects :: Maybe [RbacV1Subject]
  
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
    <$> arbitraryReducedMaybe n -- v1RollingUpdateDaemonSetMaxSurge :: Maybe IntOrString
    <*> arbitraryReducedMaybe n -- v1RollingUpdateDaemonSetMaxUnavailable :: Maybe IntOrString
  
instance Arbitrary V1RollingUpdateDeployment where
  arbitrary = sized genV1RollingUpdateDeployment

genV1RollingUpdateDeployment :: Int -> Gen V1RollingUpdateDeployment
genV1RollingUpdateDeployment n =
  V1RollingUpdateDeployment
    <$> arbitraryReducedMaybe n -- v1RollingUpdateDeploymentMaxSurge :: Maybe IntOrString
    <*> arbitraryReducedMaybe n -- v1RollingUpdateDeploymentMaxUnavailable :: Maybe IntOrString
  
instance Arbitrary V1RollingUpdateStatefulSetStrategy where
  arbitrary = sized genV1RollingUpdateStatefulSetStrategy

genV1RollingUpdateStatefulSetStrategy :: Int -> Gen V1RollingUpdateStatefulSetStrategy
genV1RollingUpdateStatefulSetStrategy n =
  V1RollingUpdateStatefulSetStrategy
    <$> arbitraryReducedMaybe n -- v1RollingUpdateStatefulSetStrategyMaxUnavailable :: Maybe IntOrString
    <*> arbitraryReducedMaybe n -- v1RollingUpdateStatefulSetStrategyPartition :: Maybe Int
  
instance Arbitrary V1RuleWithOperations where
  arbitrary = sized genV1RuleWithOperations

genV1RuleWithOperations :: Int -> Gen V1RuleWithOperations
genV1RuleWithOperations n =
  V1RuleWithOperations
    <$> arbitraryReducedMaybe n -- v1RuleWithOperationsApiGroups :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1RuleWithOperationsApiVersions :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1RuleWithOperationsOperations :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1RuleWithOperationsResources :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1RuleWithOperationsScope :: Maybe Text
  
instance Arbitrary V1RuntimeClass where
  arbitrary = sized genV1RuntimeClass

genV1RuntimeClass :: Int -> Gen V1RuntimeClass
genV1RuntimeClass n =
  V1RuntimeClass
    <$> arbitraryReducedMaybe n -- v1RuntimeClassApiVersion :: Maybe Text
    <*> arbitrary -- v1RuntimeClassHandler :: Text
    <*> arbitraryReducedMaybe n -- v1RuntimeClassKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1RuntimeClassMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1RuntimeClassOverhead :: Maybe V1Overhead
    <*> arbitraryReducedMaybe n -- v1RuntimeClassScheduling :: Maybe V1Scheduling
  
instance Arbitrary V1RuntimeClassList where
  arbitrary = sized genV1RuntimeClassList

genV1RuntimeClassList :: Int -> Gen V1RuntimeClassList
genV1RuntimeClassList n =
  V1RuntimeClassList
    <$> arbitraryReducedMaybe n -- v1RuntimeClassListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1RuntimeClassListItems :: [V1RuntimeClass]
    <*> arbitraryReducedMaybe n -- v1RuntimeClassListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1RuntimeClassListMetadata :: Maybe V1ListMeta
  
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
  
instance Arbitrary V1Scheduling where
  arbitrary = sized genV1Scheduling

genV1Scheduling :: Int -> Gen V1Scheduling
genV1Scheduling n =
  V1Scheduling
    <$> arbitraryReducedMaybe n -- v1SchedulingNodeSelector :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1SchedulingTolerations :: Maybe [V1Toleration]
  
instance Arbitrary V1ScopeSelector where
  arbitrary = sized genV1ScopeSelector

genV1ScopeSelector :: Int -> Gen V1ScopeSelector
genV1ScopeSelector n =
  V1ScopeSelector
    <$> arbitraryReducedMaybe n -- v1ScopeSelectorMatchExpressions :: Maybe [V1ScopedResourceSelectorRequirement]
  
instance Arbitrary V1ScopedResourceSelectorRequirement where
  arbitrary = sized genV1ScopedResourceSelectorRequirement

genV1ScopedResourceSelectorRequirement :: Int -> Gen V1ScopedResourceSelectorRequirement
genV1ScopedResourceSelectorRequirement n =
  V1ScopedResourceSelectorRequirement
    <$> arbitrary -- v1ScopedResourceSelectorRequirementOperator :: Text
    <*> arbitrary -- v1ScopedResourceSelectorRequirementScopeName :: Text
    <*> arbitraryReducedMaybe n -- v1ScopedResourceSelectorRequirementValues :: Maybe [Text]
  
instance Arbitrary V1SeccompProfile where
  arbitrary = sized genV1SeccompProfile

genV1SeccompProfile :: Int -> Gen V1SeccompProfile
genV1SeccompProfile n =
  V1SeccompProfile
    <$> arbitraryReducedMaybe n -- v1SeccompProfileLocalhostProfile :: Maybe Text
    <*> arbitrary -- v1SeccompProfileType :: Text
  
instance Arbitrary V1Secret where
  arbitrary = sized genV1Secret

genV1Secret :: Int -> Gen V1Secret
genV1Secret n =
  V1Secret
    <$> arbitraryReducedMaybe n -- v1SecretApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1SecretData :: Maybe (Map.Map String ByteArray)
    <*> arbitraryReducedMaybe n -- v1SecretImmutable :: Maybe Bool
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
    <*> arbitraryReducedMaybe n -- v1SecurityContextProcMount :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1SecurityContextReadOnlyRootFilesystem :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1SecurityContextRunAsGroup :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1SecurityContextRunAsNonRoot :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1SecurityContextRunAsUser :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1SecurityContextSeLinuxOptions :: Maybe V1SELinuxOptions
    <*> arbitraryReducedMaybe n -- v1SecurityContextSeccompProfile :: Maybe V1SeccompProfile
    <*> arbitraryReducedMaybe n -- v1SecurityContextWindowsOptions :: Maybe V1WindowsSecurityContextOptions
  
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
  
instance Arbitrary V1SelfSubjectReview where
  arbitrary = sized genV1SelfSubjectReview

genV1SelfSubjectReview :: Int -> Gen V1SelfSubjectReview
genV1SelfSubjectReview n =
  V1SelfSubjectReview
    <$> arbitraryReducedMaybe n -- v1SelfSubjectReviewApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1SelfSubjectReviewKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1SelfSubjectReviewMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1SelfSubjectReviewStatus :: Maybe V1SelfSubjectReviewStatus
  
instance Arbitrary V1SelfSubjectReviewStatus where
  arbitrary = sized genV1SelfSubjectReviewStatus

genV1SelfSubjectReviewStatus :: Int -> Gen V1SelfSubjectReviewStatus
genV1SelfSubjectReviewStatus n =
  V1SelfSubjectReviewStatus
    <$> arbitraryReducedMaybe n -- v1SelfSubjectReviewStatusUserInfo :: Maybe V1UserInfo
  
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
  
instance Arbitrary V1ServiceAccountSubject where
  arbitrary = sized genV1ServiceAccountSubject

genV1ServiceAccountSubject :: Int -> Gen V1ServiceAccountSubject
genV1ServiceAccountSubject n =
  V1ServiceAccountSubject
    <$> arbitrary -- v1ServiceAccountSubjectName :: Text
    <*> arbitrary -- v1ServiceAccountSubjectNamespace :: Text
  
instance Arbitrary V1ServiceAccountTokenProjection where
  arbitrary = sized genV1ServiceAccountTokenProjection

genV1ServiceAccountTokenProjection :: Int -> Gen V1ServiceAccountTokenProjection
genV1ServiceAccountTokenProjection n =
  V1ServiceAccountTokenProjection
    <$> arbitraryReducedMaybe n -- v1ServiceAccountTokenProjectionAudience :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ServiceAccountTokenProjectionExpirationSeconds :: Maybe Integer
    <*> arbitrary -- v1ServiceAccountTokenProjectionPath :: Text
  
instance Arbitrary V1ServiceBackendPort where
  arbitrary = sized genV1ServiceBackendPort

genV1ServiceBackendPort :: Int -> Gen V1ServiceBackendPort
genV1ServiceBackendPort n =
  V1ServiceBackendPort
    <$> arbitraryReducedMaybe n -- v1ServiceBackendPortName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ServiceBackendPortNumber :: Maybe Int
  
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
    <$> arbitraryReducedMaybe n -- v1ServicePortAppProtocol :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ServicePortName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ServicePortNodePort :: Maybe Int
    <*> arbitrary -- v1ServicePortPort :: Int
    <*> arbitraryReducedMaybe n -- v1ServicePortProtocol :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ServicePortTargetPort :: Maybe IntOrString
  
instance Arbitrary V1ServiceSpec where
  arbitrary = sized genV1ServiceSpec

genV1ServiceSpec :: Int -> Gen V1ServiceSpec
genV1ServiceSpec n =
  V1ServiceSpec
    <$> arbitraryReducedMaybe n -- v1ServiceSpecAllocateLoadBalancerNodePorts :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1ServiceSpecClusterIp :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ServiceSpecClusterIps :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1ServiceSpecExternalIps :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1ServiceSpecExternalName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ServiceSpecExternalTrafficPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ServiceSpecHealthCheckNodePort :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1ServiceSpecInternalTrafficPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ServiceSpecIpFamilies :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1ServiceSpecIpFamilyPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ServiceSpecLoadBalancerClass :: Maybe Text
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
    <$> arbitraryReducedMaybe n -- v1ServiceStatusConditions :: Maybe [V1Condition]
    <*> arbitraryReducedMaybe n -- v1ServiceStatusLoadBalancer :: Maybe V1LoadBalancerStatus
  
instance Arbitrary V1SessionAffinityConfig where
  arbitrary = sized genV1SessionAffinityConfig

genV1SessionAffinityConfig :: Int -> Gen V1SessionAffinityConfig
genV1SessionAffinityConfig n =
  V1SessionAffinityConfig
    <$> arbitraryReducedMaybe n -- v1SessionAffinityConfigClientIp :: Maybe V1ClientIPConfig
  
instance Arbitrary V1SleepAction where
  arbitrary = sized genV1SleepAction

genV1SleepAction :: Int -> Gen V1SleepAction
genV1SleepAction n =
  V1SleepAction
    <$> arbitrary -- v1SleepActionSeconds :: Integer
  
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
  
instance Arbitrary V1StatefulSetOrdinals where
  arbitrary = sized genV1StatefulSetOrdinals

genV1StatefulSetOrdinals :: Int -> Gen V1StatefulSetOrdinals
genV1StatefulSetOrdinals n =
  V1StatefulSetOrdinals
    <$> arbitraryReducedMaybe n -- v1StatefulSetOrdinalsStart :: Maybe Int
  
instance Arbitrary V1StatefulSetPersistentVolumeClaimRetentionPolicy where
  arbitrary = sized genV1StatefulSetPersistentVolumeClaimRetentionPolicy

genV1StatefulSetPersistentVolumeClaimRetentionPolicy :: Int -> Gen V1StatefulSetPersistentVolumeClaimRetentionPolicy
genV1StatefulSetPersistentVolumeClaimRetentionPolicy n =
  V1StatefulSetPersistentVolumeClaimRetentionPolicy
    <$> arbitraryReducedMaybe n -- v1StatefulSetPersistentVolumeClaimRetentionPolicyWhenDeleted :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1StatefulSetPersistentVolumeClaimRetentionPolicyWhenScaled :: Maybe Text
  
instance Arbitrary V1StatefulSetSpec where
  arbitrary = sized genV1StatefulSetSpec

genV1StatefulSetSpec :: Int -> Gen V1StatefulSetSpec
genV1StatefulSetSpec n =
  V1StatefulSetSpec
    <$> arbitraryReducedMaybe n -- v1StatefulSetSpecMinReadySeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1StatefulSetSpecOrdinals :: Maybe V1StatefulSetOrdinals
    <*> arbitraryReducedMaybe n -- v1StatefulSetSpecPersistentVolumeClaimRetentionPolicy :: Maybe V1StatefulSetPersistentVolumeClaimRetentionPolicy
    <*> arbitraryReducedMaybe n -- v1StatefulSetSpecPodManagementPolicy :: Maybe Text
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
    <$> arbitraryReducedMaybe n -- v1StatefulSetStatusAvailableReplicas :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1StatefulSetStatusCollisionCount :: Maybe Int
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
    <*> arbitraryReducedMaybe n -- v1StorageClassAllowedTopologies :: Maybe [V1TopologySelectorTerm]
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
  
instance Arbitrary V1Sysctl where
  arbitrary = sized genV1Sysctl

genV1Sysctl :: Int -> Gen V1Sysctl
genV1Sysctl n =
  V1Sysctl
    <$> arbitrary -- v1SysctlName :: Text
    <*> arbitrary -- v1SysctlValue :: Text
  
instance Arbitrary V1TCPSocketAction where
  arbitrary = sized genV1TCPSocketAction

genV1TCPSocketAction :: Int -> Gen V1TCPSocketAction
genV1TCPSocketAction n =
  V1TCPSocketAction
    <$> arbitraryReducedMaybe n -- v1TCPSocketActionHost :: Maybe Text
    <*> arbitraryReduced n -- v1TCPSocketActionPort :: IntOrString
  
instance Arbitrary V1Taint where
  arbitrary = sized genV1Taint

genV1Taint :: Int -> Gen V1Taint
genV1Taint n =
  V1Taint
    <$> arbitrary -- v1TaintEffect :: Text
    <*> arbitrary -- v1TaintKey :: Text
    <*> arbitraryReducedMaybe n -- v1TaintTimeAdded :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1TaintValue :: Maybe Text
  
instance Arbitrary V1TokenRequestSpec where
  arbitrary = sized genV1TokenRequestSpec

genV1TokenRequestSpec :: Int -> Gen V1TokenRequestSpec
genV1TokenRequestSpec n =
  V1TokenRequestSpec
    <$> arbitrary -- v1TokenRequestSpecAudiences :: [Text]
    <*> arbitraryReducedMaybe n -- v1TokenRequestSpecBoundObjectRef :: Maybe V1BoundObjectReference
    <*> arbitraryReducedMaybe n -- v1TokenRequestSpecExpirationSeconds :: Maybe Integer
  
instance Arbitrary V1TokenRequestStatus where
  arbitrary = sized genV1TokenRequestStatus

genV1TokenRequestStatus :: Int -> Gen V1TokenRequestStatus
genV1TokenRequestStatus n =
  V1TokenRequestStatus
    <$> arbitraryReduced n -- v1TokenRequestStatusExpirationTimestamp :: DateTime
    <*> arbitrary -- v1TokenRequestStatusToken :: Text
  
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
    <$> arbitraryReducedMaybe n -- v1TokenReviewSpecAudiences :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1TokenReviewSpecToken :: Maybe Text
  
instance Arbitrary V1TokenReviewStatus where
  arbitrary = sized genV1TokenReviewStatus

genV1TokenReviewStatus :: Int -> Gen V1TokenReviewStatus
genV1TokenReviewStatus n =
  V1TokenReviewStatus
    <$> arbitraryReducedMaybe n -- v1TokenReviewStatusAudiences :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1TokenReviewStatusAuthenticated :: Maybe Bool
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
  
instance Arbitrary V1TopologySelectorLabelRequirement where
  arbitrary = sized genV1TopologySelectorLabelRequirement

genV1TopologySelectorLabelRequirement :: Int -> Gen V1TopologySelectorLabelRequirement
genV1TopologySelectorLabelRequirement n =
  V1TopologySelectorLabelRequirement
    <$> arbitrary -- v1TopologySelectorLabelRequirementKey :: Text
    <*> arbitrary -- v1TopologySelectorLabelRequirementValues :: [Text]
  
instance Arbitrary V1TopologySelectorTerm where
  arbitrary = sized genV1TopologySelectorTerm

genV1TopologySelectorTerm :: Int -> Gen V1TopologySelectorTerm
genV1TopologySelectorTerm n =
  V1TopologySelectorTerm
    <$> arbitraryReducedMaybe n -- v1TopologySelectorTermMatchLabelExpressions :: Maybe [V1TopologySelectorLabelRequirement]
  
instance Arbitrary V1TopologySpreadConstraint where
  arbitrary = sized genV1TopologySpreadConstraint

genV1TopologySpreadConstraint :: Int -> Gen V1TopologySpreadConstraint
genV1TopologySpreadConstraint n =
  V1TopologySpreadConstraint
    <$> arbitraryReducedMaybe n -- v1TopologySpreadConstraintLabelSelector :: Maybe V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1TopologySpreadConstraintMatchLabelKeys :: Maybe [Text]
    <*> arbitrary -- v1TopologySpreadConstraintMaxSkew :: Int
    <*> arbitraryReducedMaybe n -- v1TopologySpreadConstraintMinDomains :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1TopologySpreadConstraintNodeAffinityPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1TopologySpreadConstraintNodeTaintsPolicy :: Maybe Text
    <*> arbitrary -- v1TopologySpreadConstraintTopologyKey :: Text
    <*> arbitrary -- v1TopologySpreadConstraintWhenUnsatisfiable :: Text
  
instance Arbitrary V1TypedLocalObjectReference where
  arbitrary = sized genV1TypedLocalObjectReference

genV1TypedLocalObjectReference :: Int -> Gen V1TypedLocalObjectReference
genV1TypedLocalObjectReference n =
  V1TypedLocalObjectReference
    <$> arbitraryReducedMaybe n -- v1TypedLocalObjectReferenceApiGroup :: Maybe Text
    <*> arbitrary -- v1TypedLocalObjectReferenceKind :: Text
    <*> arbitrary -- v1TypedLocalObjectReferenceName :: Text
  
instance Arbitrary V1TypedObjectReference where
  arbitrary = sized genV1TypedObjectReference

genV1TypedObjectReference :: Int -> Gen V1TypedObjectReference
genV1TypedObjectReference n =
  V1TypedObjectReference
    <$> arbitraryReducedMaybe n -- v1TypedObjectReferenceApiGroup :: Maybe Text
    <*> arbitrary -- v1TypedObjectReferenceKind :: Text
    <*> arbitrary -- v1TypedObjectReferenceName :: Text
    <*> arbitraryReducedMaybe n -- v1TypedObjectReferenceNamespace :: Maybe Text
  
instance Arbitrary V1UncountedTerminatedPods where
  arbitrary = sized genV1UncountedTerminatedPods

genV1UncountedTerminatedPods :: Int -> Gen V1UncountedTerminatedPods
genV1UncountedTerminatedPods n =
  V1UncountedTerminatedPods
    <$> arbitraryReducedMaybe n -- v1UncountedTerminatedPodsFailed :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1UncountedTerminatedPodsSucceeded :: Maybe [Text]
  
instance Arbitrary V1UserInfo where
  arbitrary = sized genV1UserInfo

genV1UserInfo :: Int -> Gen V1UserInfo
genV1UserInfo n =
  V1UserInfo
    <$> arbitraryReducedMaybe n -- v1UserInfoExtra :: Maybe (Map.Map String [Text])
    <*> arbitraryReducedMaybe n -- v1UserInfoGroups :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1UserInfoUid :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1UserInfoUsername :: Maybe Text
  
instance Arbitrary V1UserSubject where
  arbitrary = sized genV1UserSubject

genV1UserSubject :: Int -> Gen V1UserSubject
genV1UserSubject n =
  V1UserSubject
    <$> arbitrary -- v1UserSubjectName :: Text
  
instance Arbitrary V1ValidatingWebhook where
  arbitrary = sized genV1ValidatingWebhook

genV1ValidatingWebhook :: Int -> Gen V1ValidatingWebhook
genV1ValidatingWebhook n =
  V1ValidatingWebhook
    <$> arbitrary -- v1ValidatingWebhookAdmissionReviewVersions :: [Text]
    <*> arbitraryReduced n -- v1ValidatingWebhookClientConfig :: AdmissionregistrationV1WebhookClientConfig
    <*> arbitraryReducedMaybe n -- v1ValidatingWebhookFailurePolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ValidatingWebhookMatchConditions :: Maybe [V1MatchCondition]
    <*> arbitraryReducedMaybe n -- v1ValidatingWebhookMatchPolicy :: Maybe Text
    <*> arbitrary -- v1ValidatingWebhookName :: Text
    <*> arbitraryReducedMaybe n -- v1ValidatingWebhookNamespaceSelector :: Maybe V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1ValidatingWebhookObjectSelector :: Maybe V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1ValidatingWebhookRules :: Maybe [V1RuleWithOperations]
    <*> arbitrary -- v1ValidatingWebhookSideEffects :: Text
    <*> arbitraryReducedMaybe n -- v1ValidatingWebhookTimeoutSeconds :: Maybe Int
  
instance Arbitrary V1ValidatingWebhookConfiguration where
  arbitrary = sized genV1ValidatingWebhookConfiguration

genV1ValidatingWebhookConfiguration :: Int -> Gen V1ValidatingWebhookConfiguration
genV1ValidatingWebhookConfiguration n =
  V1ValidatingWebhookConfiguration
    <$> arbitraryReducedMaybe n -- v1ValidatingWebhookConfigurationApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ValidatingWebhookConfigurationKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ValidatingWebhookConfigurationMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1ValidatingWebhookConfigurationWebhooks :: Maybe [V1ValidatingWebhook]
  
instance Arbitrary V1ValidatingWebhookConfigurationList where
  arbitrary = sized genV1ValidatingWebhookConfigurationList

genV1ValidatingWebhookConfigurationList :: Int -> Gen V1ValidatingWebhookConfigurationList
genV1ValidatingWebhookConfigurationList n =
  V1ValidatingWebhookConfigurationList
    <$> arbitraryReducedMaybe n -- v1ValidatingWebhookConfigurationListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1ValidatingWebhookConfigurationListItems :: [V1ValidatingWebhookConfiguration]
    <*> arbitraryReducedMaybe n -- v1ValidatingWebhookConfigurationListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ValidatingWebhookConfigurationListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1ValidationRule where
  arbitrary = sized genV1ValidationRule

genV1ValidationRule :: Int -> Gen V1ValidationRule
genV1ValidationRule n =
  V1ValidationRule
    <$> arbitraryReducedMaybe n -- v1ValidationRuleFieldPath :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ValidationRuleMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ValidationRuleMessageExpression :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1ValidationRuleOptionalOldSelf :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1ValidationRuleReason :: Maybe Text
    <*> arbitrary -- v1ValidationRuleRule :: Text
  
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
    <*> arbitraryReducedMaybe n -- v1VolumeCsi :: Maybe V1CSIVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeDownwardApi :: Maybe V1DownwardAPIVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeEmptyDir :: Maybe V1EmptyDirVolumeSource
    <*> arbitraryReducedMaybe n -- v1VolumeEphemeral :: Maybe V1EphemeralVolumeSource
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
  
instance Arbitrary V1VolumeAttachment where
  arbitrary = sized genV1VolumeAttachment

genV1VolumeAttachment :: Int -> Gen V1VolumeAttachment
genV1VolumeAttachment n =
  V1VolumeAttachment
    <$> arbitraryReducedMaybe n -- v1VolumeAttachmentApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1VolumeAttachmentKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1VolumeAttachmentMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1VolumeAttachmentSpec :: V1VolumeAttachmentSpec
    <*> arbitraryReducedMaybe n -- v1VolumeAttachmentStatus :: Maybe V1VolumeAttachmentStatus
  
instance Arbitrary V1VolumeAttachmentList where
  arbitrary = sized genV1VolumeAttachmentList

genV1VolumeAttachmentList :: Int -> Gen V1VolumeAttachmentList
genV1VolumeAttachmentList n =
  V1VolumeAttachmentList
    <$> arbitraryReducedMaybe n -- v1VolumeAttachmentListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1VolumeAttachmentListItems :: [V1VolumeAttachment]
    <*> arbitraryReducedMaybe n -- v1VolumeAttachmentListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1VolumeAttachmentListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1VolumeAttachmentSource where
  arbitrary = sized genV1VolumeAttachmentSource

genV1VolumeAttachmentSource :: Int -> Gen V1VolumeAttachmentSource
genV1VolumeAttachmentSource n =
  V1VolumeAttachmentSource
    <$> arbitraryReducedMaybe n -- v1VolumeAttachmentSourceInlineVolumeSpec :: Maybe V1PersistentVolumeSpec
    <*> arbitraryReducedMaybe n -- v1VolumeAttachmentSourcePersistentVolumeName :: Maybe Text
  
instance Arbitrary V1VolumeAttachmentSpec where
  arbitrary = sized genV1VolumeAttachmentSpec

genV1VolumeAttachmentSpec :: Int -> Gen V1VolumeAttachmentSpec
genV1VolumeAttachmentSpec n =
  V1VolumeAttachmentSpec
    <$> arbitrary -- v1VolumeAttachmentSpecAttacher :: Text
    <*> arbitrary -- v1VolumeAttachmentSpecNodeName :: Text
    <*> arbitraryReduced n -- v1VolumeAttachmentSpecSource :: V1VolumeAttachmentSource
  
instance Arbitrary V1VolumeAttachmentStatus where
  arbitrary = sized genV1VolumeAttachmentStatus

genV1VolumeAttachmentStatus :: Int -> Gen V1VolumeAttachmentStatus
genV1VolumeAttachmentStatus n =
  V1VolumeAttachmentStatus
    <$> arbitraryReducedMaybe n -- v1VolumeAttachmentStatusAttachError :: Maybe V1VolumeError
    <*> arbitrary -- v1VolumeAttachmentStatusAttached :: Bool
    <*> arbitraryReducedMaybe n -- v1VolumeAttachmentStatusAttachmentMetadata :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- v1VolumeAttachmentStatusDetachError :: Maybe V1VolumeError
  
instance Arbitrary V1VolumeDevice where
  arbitrary = sized genV1VolumeDevice

genV1VolumeDevice :: Int -> Gen V1VolumeDevice
genV1VolumeDevice n =
  V1VolumeDevice
    <$> arbitrary -- v1VolumeDeviceDevicePath :: Text
    <*> arbitrary -- v1VolumeDeviceName :: Text
  
instance Arbitrary V1VolumeError where
  arbitrary = sized genV1VolumeError

genV1VolumeError :: Int -> Gen V1VolumeError
genV1VolumeError n =
  V1VolumeError
    <$> arbitraryReducedMaybe n -- v1VolumeErrorMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1VolumeErrorTime :: Maybe DateTime
  
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
    <*> arbitraryReducedMaybe n -- v1VolumeMountSubPathExpr :: Maybe Text
  
instance Arbitrary V1VolumeNodeAffinity where
  arbitrary = sized genV1VolumeNodeAffinity

genV1VolumeNodeAffinity :: Int -> Gen V1VolumeNodeAffinity
genV1VolumeNodeAffinity n =
  V1VolumeNodeAffinity
    <$> arbitraryReducedMaybe n -- v1VolumeNodeAffinityRequired :: Maybe V1NodeSelector
  
instance Arbitrary V1VolumeNodeResources where
  arbitrary = sized genV1VolumeNodeResources

genV1VolumeNodeResources :: Int -> Gen V1VolumeNodeResources
genV1VolumeNodeResources n =
  V1VolumeNodeResources
    <$> arbitraryReducedMaybe n -- v1VolumeNodeResourcesCount :: Maybe Int
  
instance Arbitrary V1VolumeProjection where
  arbitrary = sized genV1VolumeProjection

genV1VolumeProjection :: Int -> Gen V1VolumeProjection
genV1VolumeProjection n =
  V1VolumeProjection
    <$> arbitraryReducedMaybe n -- v1VolumeProjectionClusterTrustBundle :: Maybe V1ClusterTrustBundleProjection
    <*> arbitraryReducedMaybe n -- v1VolumeProjectionConfigMap :: Maybe V1ConfigMapProjection
    <*> arbitraryReducedMaybe n -- v1VolumeProjectionDownwardApi :: Maybe V1DownwardAPIProjection
    <*> arbitraryReducedMaybe n -- v1VolumeProjectionSecret :: Maybe V1SecretProjection
    <*> arbitraryReducedMaybe n -- v1VolumeProjectionServiceAccountToken :: Maybe V1ServiceAccountTokenProjection
  
instance Arbitrary V1VolumeResourceRequirements where
  arbitrary = sized genV1VolumeResourceRequirements

genV1VolumeResourceRequirements :: Int -> Gen V1VolumeResourceRequirements
genV1VolumeResourceRequirements n =
  V1VolumeResourceRequirements
    <$> arbitraryReducedMaybe n -- v1VolumeResourceRequirementsLimits :: Maybe (Map.Map String Quantity)
    <*> arbitraryReducedMaybe n -- v1VolumeResourceRequirementsRequests :: Maybe (Map.Map String Quantity)
  
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
    <$> arbitraryReduced n -- v1WatchEventObject :: A.Value
    <*> arbitrary -- v1WatchEventType :: Text
  
instance Arbitrary V1WebhookConversion where
  arbitrary = sized genV1WebhookConversion

genV1WebhookConversion :: Int -> Gen V1WebhookConversion
genV1WebhookConversion n =
  V1WebhookConversion
    <$> arbitraryReducedMaybe n -- v1WebhookConversionClientConfig :: Maybe ApiextensionsV1WebhookClientConfig
    <*> arbitrary -- v1WebhookConversionConversionReviewVersions :: [Text]
  
instance Arbitrary V1WeightedPodAffinityTerm where
  arbitrary = sized genV1WeightedPodAffinityTerm

genV1WeightedPodAffinityTerm :: Int -> Gen V1WeightedPodAffinityTerm
genV1WeightedPodAffinityTerm n =
  V1WeightedPodAffinityTerm
    <$> arbitraryReduced n -- v1WeightedPodAffinityTermPodAffinityTerm :: V1PodAffinityTerm
    <*> arbitrary -- v1WeightedPodAffinityTermWeight :: Int
  
instance Arbitrary V1WindowsSecurityContextOptions where
  arbitrary = sized genV1WindowsSecurityContextOptions

genV1WindowsSecurityContextOptions :: Int -> Gen V1WindowsSecurityContextOptions
genV1WindowsSecurityContextOptions n =
  V1WindowsSecurityContextOptions
    <$> arbitraryReducedMaybe n -- v1WindowsSecurityContextOptionsGmsaCredentialSpec :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1WindowsSecurityContextOptionsGmsaCredentialSpecName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1WindowsSecurityContextOptionsHostProcess :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1WindowsSecurityContextOptionsRunAsUserName :: Maybe Text
  
instance Arbitrary V1alpha1AuditAnnotation where
  arbitrary = sized genV1alpha1AuditAnnotation

genV1alpha1AuditAnnotation :: Int -> Gen V1alpha1AuditAnnotation
genV1alpha1AuditAnnotation n =
  V1alpha1AuditAnnotation
    <$> arbitrary -- v1alpha1AuditAnnotationKey :: Text
    <*> arbitrary -- v1alpha1AuditAnnotationValueExpression :: Text
  
instance Arbitrary V1alpha1ClusterTrustBundle where
  arbitrary = sized genV1alpha1ClusterTrustBundle

genV1alpha1ClusterTrustBundle :: Int -> Gen V1alpha1ClusterTrustBundle
genV1alpha1ClusterTrustBundle n =
  V1alpha1ClusterTrustBundle
    <$> arbitraryReducedMaybe n -- v1alpha1ClusterTrustBundleApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ClusterTrustBundleKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ClusterTrustBundleMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1alpha1ClusterTrustBundleSpec :: V1alpha1ClusterTrustBundleSpec
  
instance Arbitrary V1alpha1ClusterTrustBundleList where
  arbitrary = sized genV1alpha1ClusterTrustBundleList

genV1alpha1ClusterTrustBundleList :: Int -> Gen V1alpha1ClusterTrustBundleList
genV1alpha1ClusterTrustBundleList n =
  V1alpha1ClusterTrustBundleList
    <$> arbitraryReducedMaybe n -- v1alpha1ClusterTrustBundleListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1alpha1ClusterTrustBundleListItems :: [V1alpha1ClusterTrustBundle]
    <*> arbitraryReducedMaybe n -- v1alpha1ClusterTrustBundleListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ClusterTrustBundleListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1alpha1ClusterTrustBundleSpec where
  arbitrary = sized genV1alpha1ClusterTrustBundleSpec

genV1alpha1ClusterTrustBundleSpec :: Int -> Gen V1alpha1ClusterTrustBundleSpec
genV1alpha1ClusterTrustBundleSpec n =
  V1alpha1ClusterTrustBundleSpec
    <$> arbitraryReducedMaybe n -- v1alpha1ClusterTrustBundleSpecSignerName :: Maybe Text
    <*> arbitrary -- v1alpha1ClusterTrustBundleSpecTrustBundle :: Text
  
instance Arbitrary V1alpha1ExpressionWarning where
  arbitrary = sized genV1alpha1ExpressionWarning

genV1alpha1ExpressionWarning :: Int -> Gen V1alpha1ExpressionWarning
genV1alpha1ExpressionWarning n =
  V1alpha1ExpressionWarning
    <$> arbitrary -- v1alpha1ExpressionWarningFieldRef :: Text
    <*> arbitrary -- v1alpha1ExpressionWarningWarning :: Text
  
instance Arbitrary V1alpha1IPAddress where
  arbitrary = sized genV1alpha1IPAddress

genV1alpha1IPAddress :: Int -> Gen V1alpha1IPAddress
genV1alpha1IPAddress n =
  V1alpha1IPAddress
    <$> arbitraryReducedMaybe n -- v1alpha1IPAddressApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1IPAddressKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1IPAddressMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1alpha1IPAddressSpec :: Maybe V1alpha1IPAddressSpec
  
instance Arbitrary V1alpha1IPAddressList where
  arbitrary = sized genV1alpha1IPAddressList

genV1alpha1IPAddressList :: Int -> Gen V1alpha1IPAddressList
genV1alpha1IPAddressList n =
  V1alpha1IPAddressList
    <$> arbitraryReducedMaybe n -- v1alpha1IPAddressListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1alpha1IPAddressListItems :: [V1alpha1IPAddress]
    <*> arbitraryReducedMaybe n -- v1alpha1IPAddressListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1IPAddressListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1alpha1IPAddressSpec where
  arbitrary = sized genV1alpha1IPAddressSpec

genV1alpha1IPAddressSpec :: Int -> Gen V1alpha1IPAddressSpec
genV1alpha1IPAddressSpec n =
  V1alpha1IPAddressSpec
    <$> arbitraryReducedMaybe n -- v1alpha1IPAddressSpecParentRef :: Maybe V1alpha1ParentReference
  
instance Arbitrary V1alpha1MatchCondition where
  arbitrary = sized genV1alpha1MatchCondition

genV1alpha1MatchCondition :: Int -> Gen V1alpha1MatchCondition
genV1alpha1MatchCondition n =
  V1alpha1MatchCondition
    <$> arbitrary -- v1alpha1MatchConditionExpression :: Text
    <*> arbitrary -- v1alpha1MatchConditionName :: Text
  
instance Arbitrary V1alpha1MatchResources where
  arbitrary = sized genV1alpha1MatchResources

genV1alpha1MatchResources :: Int -> Gen V1alpha1MatchResources
genV1alpha1MatchResources n =
  V1alpha1MatchResources
    <$> arbitraryReducedMaybe n -- v1alpha1MatchResourcesExcludeResourceRules :: Maybe [V1alpha1NamedRuleWithOperations]
    <*> arbitraryReducedMaybe n -- v1alpha1MatchResourcesMatchPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1MatchResourcesNamespaceSelector :: Maybe V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1alpha1MatchResourcesObjectSelector :: Maybe V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1alpha1MatchResourcesResourceRules :: Maybe [V1alpha1NamedRuleWithOperations]
  
instance Arbitrary V1alpha1NamedRuleWithOperations where
  arbitrary = sized genV1alpha1NamedRuleWithOperations

genV1alpha1NamedRuleWithOperations :: Int -> Gen V1alpha1NamedRuleWithOperations
genV1alpha1NamedRuleWithOperations n =
  V1alpha1NamedRuleWithOperations
    <$> arbitraryReducedMaybe n -- v1alpha1NamedRuleWithOperationsApiGroups :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1alpha1NamedRuleWithOperationsApiVersions :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1alpha1NamedRuleWithOperationsOperations :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1alpha1NamedRuleWithOperationsResourceNames :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1alpha1NamedRuleWithOperationsResources :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1alpha1NamedRuleWithOperationsScope :: Maybe Text
  
instance Arbitrary V1alpha1ParamKind where
  arbitrary = sized genV1alpha1ParamKind

genV1alpha1ParamKind :: Int -> Gen V1alpha1ParamKind
genV1alpha1ParamKind n =
  V1alpha1ParamKind
    <$> arbitraryReducedMaybe n -- v1alpha1ParamKindApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ParamKindKind :: Maybe Text
  
instance Arbitrary V1alpha1ParamRef where
  arbitrary = sized genV1alpha1ParamRef

genV1alpha1ParamRef :: Int -> Gen V1alpha1ParamRef
genV1alpha1ParamRef n =
  V1alpha1ParamRef
    <$> arbitraryReducedMaybe n -- v1alpha1ParamRefName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ParamRefNamespace :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ParamRefParameterNotFoundAction :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ParamRefSelector :: Maybe V1LabelSelector
  
instance Arbitrary V1alpha1ParentReference where
  arbitrary = sized genV1alpha1ParentReference

genV1alpha1ParentReference :: Int -> Gen V1alpha1ParentReference
genV1alpha1ParentReference n =
  V1alpha1ParentReference
    <$> arbitraryReducedMaybe n -- v1alpha1ParentReferenceGroup :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ParentReferenceName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ParentReferenceNamespace :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ParentReferenceResource :: Maybe Text
  
instance Arbitrary V1alpha1SelfSubjectReview where
  arbitrary = sized genV1alpha1SelfSubjectReview

genV1alpha1SelfSubjectReview :: Int -> Gen V1alpha1SelfSubjectReview
genV1alpha1SelfSubjectReview n =
  V1alpha1SelfSubjectReview
    <$> arbitraryReducedMaybe n -- v1alpha1SelfSubjectReviewApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1SelfSubjectReviewKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1SelfSubjectReviewMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1alpha1SelfSubjectReviewStatus :: Maybe V1alpha1SelfSubjectReviewStatus
  
instance Arbitrary V1alpha1SelfSubjectReviewStatus where
  arbitrary = sized genV1alpha1SelfSubjectReviewStatus

genV1alpha1SelfSubjectReviewStatus :: Int -> Gen V1alpha1SelfSubjectReviewStatus
genV1alpha1SelfSubjectReviewStatus n =
  V1alpha1SelfSubjectReviewStatus
    <$> arbitraryReducedMaybe n -- v1alpha1SelfSubjectReviewStatusUserInfo :: Maybe V1UserInfo
  
instance Arbitrary V1alpha1ServerStorageVersion where
  arbitrary = sized genV1alpha1ServerStorageVersion

genV1alpha1ServerStorageVersion :: Int -> Gen V1alpha1ServerStorageVersion
genV1alpha1ServerStorageVersion n =
  V1alpha1ServerStorageVersion
    <$> arbitraryReducedMaybe n -- v1alpha1ServerStorageVersionApiServerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ServerStorageVersionDecodableVersions :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1alpha1ServerStorageVersionEncodingVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ServerStorageVersionServedVersions :: Maybe [Text]
  
instance Arbitrary V1alpha1ServiceCIDR where
  arbitrary = sized genV1alpha1ServiceCIDR

genV1alpha1ServiceCIDR :: Int -> Gen V1alpha1ServiceCIDR
genV1alpha1ServiceCIDR n =
  V1alpha1ServiceCIDR
    <$> arbitraryReducedMaybe n -- v1alpha1ServiceCIDRApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ServiceCIDRKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ServiceCIDRMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1alpha1ServiceCIDRSpec :: Maybe V1alpha1ServiceCIDRSpec
    <*> arbitraryReducedMaybe n -- v1alpha1ServiceCIDRStatus :: Maybe V1alpha1ServiceCIDRStatus
  
instance Arbitrary V1alpha1ServiceCIDRList where
  arbitrary = sized genV1alpha1ServiceCIDRList

genV1alpha1ServiceCIDRList :: Int -> Gen V1alpha1ServiceCIDRList
genV1alpha1ServiceCIDRList n =
  V1alpha1ServiceCIDRList
    <$> arbitraryReducedMaybe n -- v1alpha1ServiceCIDRListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1alpha1ServiceCIDRListItems :: [V1alpha1ServiceCIDR]
    <*> arbitraryReducedMaybe n -- v1alpha1ServiceCIDRListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ServiceCIDRListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1alpha1ServiceCIDRSpec where
  arbitrary = sized genV1alpha1ServiceCIDRSpec

genV1alpha1ServiceCIDRSpec :: Int -> Gen V1alpha1ServiceCIDRSpec
genV1alpha1ServiceCIDRSpec n =
  V1alpha1ServiceCIDRSpec
    <$> arbitraryReducedMaybe n -- v1alpha1ServiceCIDRSpecCidrs :: Maybe [Text]
  
instance Arbitrary V1alpha1ServiceCIDRStatus where
  arbitrary = sized genV1alpha1ServiceCIDRStatus

genV1alpha1ServiceCIDRStatus :: Int -> Gen V1alpha1ServiceCIDRStatus
genV1alpha1ServiceCIDRStatus n =
  V1alpha1ServiceCIDRStatus
    <$> arbitraryReducedMaybe n -- v1alpha1ServiceCIDRStatusConditions :: Maybe [V1Condition]
  
instance Arbitrary V1alpha1StorageVersion where
  arbitrary = sized genV1alpha1StorageVersion

genV1alpha1StorageVersion :: Int -> Gen V1alpha1StorageVersion
genV1alpha1StorageVersion n =
  V1alpha1StorageVersion
    <$> arbitraryReducedMaybe n -- v1alpha1StorageVersionApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1StorageVersionKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1StorageVersionMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1alpha1StorageVersionSpec :: A.Value
    <*> arbitraryReduced n -- v1alpha1StorageVersionStatus :: V1alpha1StorageVersionStatus
  
instance Arbitrary V1alpha1StorageVersionCondition where
  arbitrary = sized genV1alpha1StorageVersionCondition

genV1alpha1StorageVersionCondition :: Int -> Gen V1alpha1StorageVersionCondition
genV1alpha1StorageVersionCondition n =
  V1alpha1StorageVersionCondition
    <$> arbitraryReducedMaybe n -- v1alpha1StorageVersionConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1alpha1StorageVersionConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1StorageVersionConditionObservedGeneration :: Maybe Integer
    <*> arbitrary -- v1alpha1StorageVersionConditionReason :: Text
    <*> arbitrary -- v1alpha1StorageVersionConditionStatus :: Text
    <*> arbitrary -- v1alpha1StorageVersionConditionType :: Text
  
instance Arbitrary V1alpha1StorageVersionList where
  arbitrary = sized genV1alpha1StorageVersionList

genV1alpha1StorageVersionList :: Int -> Gen V1alpha1StorageVersionList
genV1alpha1StorageVersionList n =
  V1alpha1StorageVersionList
    <$> arbitraryReducedMaybe n -- v1alpha1StorageVersionListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1alpha1StorageVersionListItems :: [V1alpha1StorageVersion]
    <*> arbitraryReducedMaybe n -- v1alpha1StorageVersionListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1StorageVersionListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1alpha1StorageVersionStatus where
  arbitrary = sized genV1alpha1StorageVersionStatus

genV1alpha1StorageVersionStatus :: Int -> Gen V1alpha1StorageVersionStatus
genV1alpha1StorageVersionStatus n =
  V1alpha1StorageVersionStatus
    <$> arbitraryReducedMaybe n -- v1alpha1StorageVersionStatusCommonEncodingVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1StorageVersionStatusConditions :: Maybe [V1alpha1StorageVersionCondition]
    <*> arbitraryReducedMaybe n -- v1alpha1StorageVersionStatusStorageVersions :: Maybe [V1alpha1ServerStorageVersion]
  
instance Arbitrary V1alpha1TypeChecking where
  arbitrary = sized genV1alpha1TypeChecking

genV1alpha1TypeChecking :: Int -> Gen V1alpha1TypeChecking
genV1alpha1TypeChecking n =
  V1alpha1TypeChecking
    <$> arbitraryReducedMaybe n -- v1alpha1TypeCheckingExpressionWarnings :: Maybe [V1alpha1ExpressionWarning]
  
instance Arbitrary V1alpha1ValidatingAdmissionPolicy where
  arbitrary = sized genV1alpha1ValidatingAdmissionPolicy

genV1alpha1ValidatingAdmissionPolicy :: Int -> Gen V1alpha1ValidatingAdmissionPolicy
genV1alpha1ValidatingAdmissionPolicy n =
  V1alpha1ValidatingAdmissionPolicy
    <$> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicySpec :: Maybe V1alpha1ValidatingAdmissionPolicySpec
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyStatus :: Maybe V1alpha1ValidatingAdmissionPolicyStatus
  
instance Arbitrary V1alpha1ValidatingAdmissionPolicyBinding where
  arbitrary = sized genV1alpha1ValidatingAdmissionPolicyBinding

genV1alpha1ValidatingAdmissionPolicyBinding :: Int -> Gen V1alpha1ValidatingAdmissionPolicyBinding
genV1alpha1ValidatingAdmissionPolicyBinding n =
  V1alpha1ValidatingAdmissionPolicyBinding
    <$> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyBindingApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyBindingKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyBindingMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyBindingSpec :: Maybe V1alpha1ValidatingAdmissionPolicyBindingSpec
  
instance Arbitrary V1alpha1ValidatingAdmissionPolicyBindingList where
  arbitrary = sized genV1alpha1ValidatingAdmissionPolicyBindingList

genV1alpha1ValidatingAdmissionPolicyBindingList :: Int -> Gen V1alpha1ValidatingAdmissionPolicyBindingList
genV1alpha1ValidatingAdmissionPolicyBindingList n =
  V1alpha1ValidatingAdmissionPolicyBindingList
    <$> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyBindingListApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyBindingListItems :: Maybe [V1alpha1ValidatingAdmissionPolicyBinding]
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyBindingListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyBindingListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1alpha1ValidatingAdmissionPolicyBindingSpec where
  arbitrary = sized genV1alpha1ValidatingAdmissionPolicyBindingSpec

genV1alpha1ValidatingAdmissionPolicyBindingSpec :: Int -> Gen V1alpha1ValidatingAdmissionPolicyBindingSpec
genV1alpha1ValidatingAdmissionPolicyBindingSpec n =
  V1alpha1ValidatingAdmissionPolicyBindingSpec
    <$> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyBindingSpecMatchResources :: Maybe V1alpha1MatchResources
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyBindingSpecParamRef :: Maybe V1alpha1ParamRef
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyBindingSpecPolicyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyBindingSpecValidationActions :: Maybe [Text]
  
instance Arbitrary V1alpha1ValidatingAdmissionPolicyList where
  arbitrary = sized genV1alpha1ValidatingAdmissionPolicyList

genV1alpha1ValidatingAdmissionPolicyList :: Int -> Gen V1alpha1ValidatingAdmissionPolicyList
genV1alpha1ValidatingAdmissionPolicyList n =
  V1alpha1ValidatingAdmissionPolicyList
    <$> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyListApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyListItems :: Maybe [V1alpha1ValidatingAdmissionPolicy]
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1alpha1ValidatingAdmissionPolicySpec where
  arbitrary = sized genV1alpha1ValidatingAdmissionPolicySpec

genV1alpha1ValidatingAdmissionPolicySpec :: Int -> Gen V1alpha1ValidatingAdmissionPolicySpec
genV1alpha1ValidatingAdmissionPolicySpec n =
  V1alpha1ValidatingAdmissionPolicySpec
    <$> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicySpecAuditAnnotations :: Maybe [V1alpha1AuditAnnotation]
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicySpecFailurePolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicySpecMatchConditions :: Maybe [V1alpha1MatchCondition]
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicySpecMatchConstraints :: Maybe V1alpha1MatchResources
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicySpecParamKind :: Maybe V1alpha1ParamKind
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicySpecValidations :: Maybe [V1alpha1Validation]
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicySpecVariables :: Maybe [V1alpha1Variable]
  
instance Arbitrary V1alpha1ValidatingAdmissionPolicyStatus where
  arbitrary = sized genV1alpha1ValidatingAdmissionPolicyStatus

genV1alpha1ValidatingAdmissionPolicyStatus :: Int -> Gen V1alpha1ValidatingAdmissionPolicyStatus
genV1alpha1ValidatingAdmissionPolicyStatus n =
  V1alpha1ValidatingAdmissionPolicyStatus
    <$> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyStatusConditions :: Maybe [V1Condition]
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyStatusObservedGeneration :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1alpha1ValidatingAdmissionPolicyStatusTypeChecking :: Maybe V1alpha1TypeChecking
  
instance Arbitrary V1alpha1Validation where
  arbitrary = sized genV1alpha1Validation

genV1alpha1Validation :: Int -> Gen V1alpha1Validation
genV1alpha1Validation n =
  V1alpha1Validation
    <$> arbitrary -- v1alpha1ValidationExpression :: Text
    <*> arbitraryReducedMaybe n -- v1alpha1ValidationMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ValidationMessageExpression :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1ValidationReason :: Maybe Text
  
instance Arbitrary V1alpha1Variable where
  arbitrary = sized genV1alpha1Variable

genV1alpha1Variable :: Int -> Gen V1alpha1Variable
genV1alpha1Variable n =
  V1alpha1Variable
    <$> arbitrary -- v1alpha1VariableExpression :: Text
    <*> arbitrary -- v1alpha1VariableName :: Text
  
instance Arbitrary V1alpha1VolumeAttributesClass where
  arbitrary = sized genV1alpha1VolumeAttributesClass

genV1alpha1VolumeAttributesClass :: Int -> Gen V1alpha1VolumeAttributesClass
genV1alpha1VolumeAttributesClass n =
  V1alpha1VolumeAttributesClass
    <$> arbitraryReducedMaybe n -- v1alpha1VolumeAttributesClassApiVersion :: Maybe Text
    <*> arbitrary -- v1alpha1VolumeAttributesClassDriverName :: Text
    <*> arbitraryReducedMaybe n -- v1alpha1VolumeAttributesClassKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1VolumeAttributesClassMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1alpha1VolumeAttributesClassParameters :: Maybe (Map.Map String Text)
  
instance Arbitrary V1alpha1VolumeAttributesClassList where
  arbitrary = sized genV1alpha1VolumeAttributesClassList

genV1alpha1VolumeAttributesClassList :: Int -> Gen V1alpha1VolumeAttributesClassList
genV1alpha1VolumeAttributesClassList n =
  V1alpha1VolumeAttributesClassList
    <$> arbitraryReducedMaybe n -- v1alpha1VolumeAttributesClassListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1alpha1VolumeAttributesClassListItems :: [V1alpha1VolumeAttributesClass]
    <*> arbitraryReducedMaybe n -- v1alpha1VolumeAttributesClassListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha1VolumeAttributesClassListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1alpha2AllocationResult where
  arbitrary = sized genV1alpha2AllocationResult

genV1alpha2AllocationResult :: Int -> Gen V1alpha2AllocationResult
genV1alpha2AllocationResult n =
  V1alpha2AllocationResult
    <$> arbitraryReducedMaybe n -- v1alpha2AllocationResultAvailableOnNodes :: Maybe V1NodeSelector
    <*> arbitraryReducedMaybe n -- v1alpha2AllocationResultResourceHandles :: Maybe [V1alpha2ResourceHandle]
    <*> arbitraryReducedMaybe n -- v1alpha2AllocationResultShareable :: Maybe Bool
  
instance Arbitrary V1alpha2PodSchedulingContext where
  arbitrary = sized genV1alpha2PodSchedulingContext

genV1alpha2PodSchedulingContext :: Int -> Gen V1alpha2PodSchedulingContext
genV1alpha2PodSchedulingContext n =
  V1alpha2PodSchedulingContext
    <$> arbitraryReducedMaybe n -- v1alpha2PodSchedulingContextApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha2PodSchedulingContextKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha2PodSchedulingContextMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1alpha2PodSchedulingContextSpec :: V1alpha2PodSchedulingContextSpec
    <*> arbitraryReducedMaybe n -- v1alpha2PodSchedulingContextStatus :: Maybe V1alpha2PodSchedulingContextStatus
  
instance Arbitrary V1alpha2PodSchedulingContextList where
  arbitrary = sized genV1alpha2PodSchedulingContextList

genV1alpha2PodSchedulingContextList :: Int -> Gen V1alpha2PodSchedulingContextList
genV1alpha2PodSchedulingContextList n =
  V1alpha2PodSchedulingContextList
    <$> arbitraryReducedMaybe n -- v1alpha2PodSchedulingContextListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1alpha2PodSchedulingContextListItems :: [V1alpha2PodSchedulingContext]
    <*> arbitraryReducedMaybe n -- v1alpha2PodSchedulingContextListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha2PodSchedulingContextListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1alpha2PodSchedulingContextSpec where
  arbitrary = sized genV1alpha2PodSchedulingContextSpec

genV1alpha2PodSchedulingContextSpec :: Int -> Gen V1alpha2PodSchedulingContextSpec
genV1alpha2PodSchedulingContextSpec n =
  V1alpha2PodSchedulingContextSpec
    <$> arbitraryReducedMaybe n -- v1alpha2PodSchedulingContextSpecPotentialNodes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1alpha2PodSchedulingContextSpecSelectedNode :: Maybe Text
  
instance Arbitrary V1alpha2PodSchedulingContextStatus where
  arbitrary = sized genV1alpha2PodSchedulingContextStatus

genV1alpha2PodSchedulingContextStatus :: Int -> Gen V1alpha2PodSchedulingContextStatus
genV1alpha2PodSchedulingContextStatus n =
  V1alpha2PodSchedulingContextStatus
    <$> arbitraryReducedMaybe n -- v1alpha2PodSchedulingContextStatusResourceClaims :: Maybe [V1alpha2ResourceClaimSchedulingStatus]
  
instance Arbitrary V1alpha2ResourceClaim where
  arbitrary = sized genV1alpha2ResourceClaim

genV1alpha2ResourceClaim :: Int -> Gen V1alpha2ResourceClaim
genV1alpha2ResourceClaim n =
  V1alpha2ResourceClaim
    <$> arbitraryReducedMaybe n -- v1alpha2ResourceClaimApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha2ResourceClaimKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha2ResourceClaimMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1alpha2ResourceClaimSpec :: V1alpha2ResourceClaimSpec
    <*> arbitraryReducedMaybe n -- v1alpha2ResourceClaimStatus :: Maybe V1alpha2ResourceClaimStatus
  
instance Arbitrary V1alpha2ResourceClaimConsumerReference where
  arbitrary = sized genV1alpha2ResourceClaimConsumerReference

genV1alpha2ResourceClaimConsumerReference :: Int -> Gen V1alpha2ResourceClaimConsumerReference
genV1alpha2ResourceClaimConsumerReference n =
  V1alpha2ResourceClaimConsumerReference
    <$> arbitraryReducedMaybe n -- v1alpha2ResourceClaimConsumerReferenceApiGroup :: Maybe Text
    <*> arbitrary -- v1alpha2ResourceClaimConsumerReferenceName :: Text
    <*> arbitrary -- v1alpha2ResourceClaimConsumerReferenceResource :: Text
    <*> arbitrary -- v1alpha2ResourceClaimConsumerReferenceUid :: Text
  
instance Arbitrary V1alpha2ResourceClaimList where
  arbitrary = sized genV1alpha2ResourceClaimList

genV1alpha2ResourceClaimList :: Int -> Gen V1alpha2ResourceClaimList
genV1alpha2ResourceClaimList n =
  V1alpha2ResourceClaimList
    <$> arbitraryReducedMaybe n -- v1alpha2ResourceClaimListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1alpha2ResourceClaimListItems :: [V1alpha2ResourceClaim]
    <*> arbitraryReducedMaybe n -- v1alpha2ResourceClaimListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha2ResourceClaimListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1alpha2ResourceClaimParametersReference where
  arbitrary = sized genV1alpha2ResourceClaimParametersReference

genV1alpha2ResourceClaimParametersReference :: Int -> Gen V1alpha2ResourceClaimParametersReference
genV1alpha2ResourceClaimParametersReference n =
  V1alpha2ResourceClaimParametersReference
    <$> arbitraryReducedMaybe n -- v1alpha2ResourceClaimParametersReferenceApiGroup :: Maybe Text
    <*> arbitrary -- v1alpha2ResourceClaimParametersReferenceKind :: Text
    <*> arbitrary -- v1alpha2ResourceClaimParametersReferenceName :: Text
  
instance Arbitrary V1alpha2ResourceClaimSchedulingStatus where
  arbitrary = sized genV1alpha2ResourceClaimSchedulingStatus

genV1alpha2ResourceClaimSchedulingStatus :: Int -> Gen V1alpha2ResourceClaimSchedulingStatus
genV1alpha2ResourceClaimSchedulingStatus n =
  V1alpha2ResourceClaimSchedulingStatus
    <$> arbitraryReducedMaybe n -- v1alpha2ResourceClaimSchedulingStatusName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha2ResourceClaimSchedulingStatusUnsuitableNodes :: Maybe [Text]
  
instance Arbitrary V1alpha2ResourceClaimSpec where
  arbitrary = sized genV1alpha2ResourceClaimSpec

genV1alpha2ResourceClaimSpec :: Int -> Gen V1alpha2ResourceClaimSpec
genV1alpha2ResourceClaimSpec n =
  V1alpha2ResourceClaimSpec
    <$> arbitraryReducedMaybe n -- v1alpha2ResourceClaimSpecAllocationMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha2ResourceClaimSpecParametersRef :: Maybe V1alpha2ResourceClaimParametersReference
    <*> arbitrary -- v1alpha2ResourceClaimSpecResourceClassName :: Text
  
instance Arbitrary V1alpha2ResourceClaimStatus where
  arbitrary = sized genV1alpha2ResourceClaimStatus

genV1alpha2ResourceClaimStatus :: Int -> Gen V1alpha2ResourceClaimStatus
genV1alpha2ResourceClaimStatus n =
  V1alpha2ResourceClaimStatus
    <$> arbitraryReducedMaybe n -- v1alpha2ResourceClaimStatusAllocation :: Maybe V1alpha2AllocationResult
    <*> arbitraryReducedMaybe n -- v1alpha2ResourceClaimStatusDeallocationRequested :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1alpha2ResourceClaimStatusDriverName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha2ResourceClaimStatusReservedFor :: Maybe [V1alpha2ResourceClaimConsumerReference]
  
instance Arbitrary V1alpha2ResourceClaimTemplate where
  arbitrary = sized genV1alpha2ResourceClaimTemplate

genV1alpha2ResourceClaimTemplate :: Int -> Gen V1alpha2ResourceClaimTemplate
genV1alpha2ResourceClaimTemplate n =
  V1alpha2ResourceClaimTemplate
    <$> arbitraryReducedMaybe n -- v1alpha2ResourceClaimTemplateApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha2ResourceClaimTemplateKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha2ResourceClaimTemplateMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1alpha2ResourceClaimTemplateSpec :: V1alpha2ResourceClaimTemplateSpec
  
instance Arbitrary V1alpha2ResourceClaimTemplateList where
  arbitrary = sized genV1alpha2ResourceClaimTemplateList

genV1alpha2ResourceClaimTemplateList :: Int -> Gen V1alpha2ResourceClaimTemplateList
genV1alpha2ResourceClaimTemplateList n =
  V1alpha2ResourceClaimTemplateList
    <$> arbitraryReducedMaybe n -- v1alpha2ResourceClaimTemplateListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1alpha2ResourceClaimTemplateListItems :: [V1alpha2ResourceClaimTemplate]
    <*> arbitraryReducedMaybe n -- v1alpha2ResourceClaimTemplateListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha2ResourceClaimTemplateListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1alpha2ResourceClaimTemplateSpec where
  arbitrary = sized genV1alpha2ResourceClaimTemplateSpec

genV1alpha2ResourceClaimTemplateSpec :: Int -> Gen V1alpha2ResourceClaimTemplateSpec
genV1alpha2ResourceClaimTemplateSpec n =
  V1alpha2ResourceClaimTemplateSpec
    <$> arbitraryReducedMaybe n -- v1alpha2ResourceClaimTemplateSpecMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReduced n -- v1alpha2ResourceClaimTemplateSpecSpec :: V1alpha2ResourceClaimSpec
  
instance Arbitrary V1alpha2ResourceClass where
  arbitrary = sized genV1alpha2ResourceClass

genV1alpha2ResourceClass :: Int -> Gen V1alpha2ResourceClass
genV1alpha2ResourceClass n =
  V1alpha2ResourceClass
    <$> arbitraryReducedMaybe n -- v1alpha2ResourceClassApiVersion :: Maybe Text
    <*> arbitrary -- v1alpha2ResourceClassDriverName :: Text
    <*> arbitraryReducedMaybe n -- v1alpha2ResourceClassKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha2ResourceClassMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1alpha2ResourceClassParametersRef :: Maybe V1alpha2ResourceClassParametersReference
    <*> arbitraryReducedMaybe n -- v1alpha2ResourceClassSuitableNodes :: Maybe V1NodeSelector
  
instance Arbitrary V1alpha2ResourceClassList where
  arbitrary = sized genV1alpha2ResourceClassList

genV1alpha2ResourceClassList :: Int -> Gen V1alpha2ResourceClassList
genV1alpha2ResourceClassList n =
  V1alpha2ResourceClassList
    <$> arbitraryReducedMaybe n -- v1alpha2ResourceClassListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1alpha2ResourceClassListItems :: [V1alpha2ResourceClass]
    <*> arbitraryReducedMaybe n -- v1alpha2ResourceClassListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha2ResourceClassListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1alpha2ResourceClassParametersReference where
  arbitrary = sized genV1alpha2ResourceClassParametersReference

genV1alpha2ResourceClassParametersReference :: Int -> Gen V1alpha2ResourceClassParametersReference
genV1alpha2ResourceClassParametersReference n =
  V1alpha2ResourceClassParametersReference
    <$> arbitraryReducedMaybe n -- v1alpha2ResourceClassParametersReferenceApiGroup :: Maybe Text
    <*> arbitrary -- v1alpha2ResourceClassParametersReferenceKind :: Text
    <*> arbitrary -- v1alpha2ResourceClassParametersReferenceName :: Text
    <*> arbitraryReducedMaybe n -- v1alpha2ResourceClassParametersReferenceNamespace :: Maybe Text
  
instance Arbitrary V1alpha2ResourceHandle where
  arbitrary = sized genV1alpha2ResourceHandle

genV1alpha2ResourceHandle :: Int -> Gen V1alpha2ResourceHandle
genV1alpha2ResourceHandle n =
  V1alpha2ResourceHandle
    <$> arbitraryReducedMaybe n -- v1alpha2ResourceHandleData :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1alpha2ResourceHandleDriverName :: Maybe Text
  
instance Arbitrary V1beta1AuditAnnotation where
  arbitrary = sized genV1beta1AuditAnnotation

genV1beta1AuditAnnotation :: Int -> Gen V1beta1AuditAnnotation
genV1beta1AuditAnnotation n =
  V1beta1AuditAnnotation
    <$> arbitrary -- v1beta1AuditAnnotationKey :: Text
    <*> arbitrary -- v1beta1AuditAnnotationValueExpression :: Text
  
instance Arbitrary V1beta1ExpressionWarning where
  arbitrary = sized genV1beta1ExpressionWarning

genV1beta1ExpressionWarning :: Int -> Gen V1beta1ExpressionWarning
genV1beta1ExpressionWarning n =
  V1beta1ExpressionWarning
    <$> arbitrary -- v1beta1ExpressionWarningFieldRef :: Text
    <*> arbitrary -- v1beta1ExpressionWarningWarning :: Text
  
instance Arbitrary V1beta1MatchCondition where
  arbitrary = sized genV1beta1MatchCondition

genV1beta1MatchCondition :: Int -> Gen V1beta1MatchCondition
genV1beta1MatchCondition n =
  V1beta1MatchCondition
    <$> arbitrary -- v1beta1MatchConditionExpression :: Text
    <*> arbitrary -- v1beta1MatchConditionName :: Text
  
instance Arbitrary V1beta1MatchResources where
  arbitrary = sized genV1beta1MatchResources

genV1beta1MatchResources :: Int -> Gen V1beta1MatchResources
genV1beta1MatchResources n =
  V1beta1MatchResources
    <$> arbitraryReducedMaybe n -- v1beta1MatchResourcesExcludeResourceRules :: Maybe [V1beta1NamedRuleWithOperations]
    <*> arbitraryReducedMaybe n -- v1beta1MatchResourcesMatchPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1MatchResourcesNamespaceSelector :: Maybe V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1beta1MatchResourcesObjectSelector :: Maybe V1LabelSelector
    <*> arbitraryReducedMaybe n -- v1beta1MatchResourcesResourceRules :: Maybe [V1beta1NamedRuleWithOperations]
  
instance Arbitrary V1beta1NamedRuleWithOperations where
  arbitrary = sized genV1beta1NamedRuleWithOperations

genV1beta1NamedRuleWithOperations :: Int -> Gen V1beta1NamedRuleWithOperations
genV1beta1NamedRuleWithOperations n =
  V1beta1NamedRuleWithOperations
    <$> arbitraryReducedMaybe n -- v1beta1NamedRuleWithOperationsApiGroups :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1beta1NamedRuleWithOperationsApiVersions :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1beta1NamedRuleWithOperationsOperations :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1beta1NamedRuleWithOperationsResourceNames :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1beta1NamedRuleWithOperationsResources :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1beta1NamedRuleWithOperationsScope :: Maybe Text
  
instance Arbitrary V1beta1ParamKind where
  arbitrary = sized genV1beta1ParamKind

genV1beta1ParamKind :: Int -> Gen V1beta1ParamKind
genV1beta1ParamKind n =
  V1beta1ParamKind
    <$> arbitraryReducedMaybe n -- v1beta1ParamKindApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ParamKindKind :: Maybe Text
  
instance Arbitrary V1beta1ParamRef where
  arbitrary = sized genV1beta1ParamRef

genV1beta1ParamRef :: Int -> Gen V1beta1ParamRef
genV1beta1ParamRef n =
  V1beta1ParamRef
    <$> arbitraryReducedMaybe n -- v1beta1ParamRefName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ParamRefNamespace :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ParamRefParameterNotFoundAction :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ParamRefSelector :: Maybe V1LabelSelector
  
instance Arbitrary V1beta1SelfSubjectReview where
  arbitrary = sized genV1beta1SelfSubjectReview

genV1beta1SelfSubjectReview :: Int -> Gen V1beta1SelfSubjectReview
genV1beta1SelfSubjectReview n =
  V1beta1SelfSubjectReview
    <$> arbitraryReducedMaybe n -- v1beta1SelfSubjectReviewApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1SelfSubjectReviewKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1SelfSubjectReviewMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta1SelfSubjectReviewStatus :: Maybe V1beta1SelfSubjectReviewStatus
  
instance Arbitrary V1beta1SelfSubjectReviewStatus where
  arbitrary = sized genV1beta1SelfSubjectReviewStatus

genV1beta1SelfSubjectReviewStatus :: Int -> Gen V1beta1SelfSubjectReviewStatus
genV1beta1SelfSubjectReviewStatus n =
  V1beta1SelfSubjectReviewStatus
    <$> arbitraryReducedMaybe n -- v1beta1SelfSubjectReviewStatusUserInfo :: Maybe V1UserInfo
  
instance Arbitrary V1beta1TypeChecking where
  arbitrary = sized genV1beta1TypeChecking

genV1beta1TypeChecking :: Int -> Gen V1beta1TypeChecking
genV1beta1TypeChecking n =
  V1beta1TypeChecking
    <$> arbitraryReducedMaybe n -- v1beta1TypeCheckingExpressionWarnings :: Maybe [V1beta1ExpressionWarning]
  
instance Arbitrary V1beta1ValidatingAdmissionPolicy where
  arbitrary = sized genV1beta1ValidatingAdmissionPolicy

genV1beta1ValidatingAdmissionPolicy :: Int -> Gen V1beta1ValidatingAdmissionPolicy
genV1beta1ValidatingAdmissionPolicy n =
  V1beta1ValidatingAdmissionPolicy
    <$> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicySpec :: Maybe V1beta1ValidatingAdmissionPolicySpec
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyStatus :: Maybe V1beta1ValidatingAdmissionPolicyStatus
  
instance Arbitrary V1beta1ValidatingAdmissionPolicyBinding where
  arbitrary = sized genV1beta1ValidatingAdmissionPolicyBinding

genV1beta1ValidatingAdmissionPolicyBinding :: Int -> Gen V1beta1ValidatingAdmissionPolicyBinding
genV1beta1ValidatingAdmissionPolicyBinding n =
  V1beta1ValidatingAdmissionPolicyBinding
    <$> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyBindingApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyBindingKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyBindingMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyBindingSpec :: Maybe V1beta1ValidatingAdmissionPolicyBindingSpec
  
instance Arbitrary V1beta1ValidatingAdmissionPolicyBindingList where
  arbitrary = sized genV1beta1ValidatingAdmissionPolicyBindingList

genV1beta1ValidatingAdmissionPolicyBindingList :: Int -> Gen V1beta1ValidatingAdmissionPolicyBindingList
genV1beta1ValidatingAdmissionPolicyBindingList n =
  V1beta1ValidatingAdmissionPolicyBindingList
    <$> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyBindingListApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyBindingListItems :: Maybe [V1beta1ValidatingAdmissionPolicyBinding]
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyBindingListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyBindingListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta1ValidatingAdmissionPolicyBindingSpec where
  arbitrary = sized genV1beta1ValidatingAdmissionPolicyBindingSpec

genV1beta1ValidatingAdmissionPolicyBindingSpec :: Int -> Gen V1beta1ValidatingAdmissionPolicyBindingSpec
genV1beta1ValidatingAdmissionPolicyBindingSpec n =
  V1beta1ValidatingAdmissionPolicyBindingSpec
    <$> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyBindingSpecMatchResources :: Maybe V1beta1MatchResources
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyBindingSpecParamRef :: Maybe V1beta1ParamRef
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyBindingSpecPolicyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyBindingSpecValidationActions :: Maybe [Text]
  
instance Arbitrary V1beta1ValidatingAdmissionPolicyList where
  arbitrary = sized genV1beta1ValidatingAdmissionPolicyList

genV1beta1ValidatingAdmissionPolicyList :: Int -> Gen V1beta1ValidatingAdmissionPolicyList
genV1beta1ValidatingAdmissionPolicyList n =
  V1beta1ValidatingAdmissionPolicyList
    <$> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyListApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyListItems :: Maybe [V1beta1ValidatingAdmissionPolicy]
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta1ValidatingAdmissionPolicySpec where
  arbitrary = sized genV1beta1ValidatingAdmissionPolicySpec

genV1beta1ValidatingAdmissionPolicySpec :: Int -> Gen V1beta1ValidatingAdmissionPolicySpec
genV1beta1ValidatingAdmissionPolicySpec n =
  V1beta1ValidatingAdmissionPolicySpec
    <$> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicySpecAuditAnnotations :: Maybe [V1beta1AuditAnnotation]
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicySpecFailurePolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicySpecMatchConditions :: Maybe [V1beta1MatchCondition]
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicySpecMatchConstraints :: Maybe V1beta1MatchResources
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicySpecParamKind :: Maybe V1beta1ParamKind
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicySpecValidations :: Maybe [V1beta1Validation]
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicySpecVariables :: Maybe [V1beta1Variable]
  
instance Arbitrary V1beta1ValidatingAdmissionPolicyStatus where
  arbitrary = sized genV1beta1ValidatingAdmissionPolicyStatus

genV1beta1ValidatingAdmissionPolicyStatus :: Int -> Gen V1beta1ValidatingAdmissionPolicyStatus
genV1beta1ValidatingAdmissionPolicyStatus n =
  V1beta1ValidatingAdmissionPolicyStatus
    <$> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyStatusConditions :: Maybe [V1Condition]
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyStatusObservedGeneration :: Maybe Integer
    <*> arbitraryReducedMaybe n -- v1beta1ValidatingAdmissionPolicyStatusTypeChecking :: Maybe V1beta1TypeChecking
  
instance Arbitrary V1beta1Validation where
  arbitrary = sized genV1beta1Validation

genV1beta1Validation :: Int -> Gen V1beta1Validation
genV1beta1Validation n =
  V1beta1Validation
    <$> arbitrary -- v1beta1ValidationExpression :: Text
    <*> arbitraryReducedMaybe n -- v1beta1ValidationMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ValidationMessageExpression :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta1ValidationReason :: Maybe Text
  
instance Arbitrary V1beta1Variable where
  arbitrary = sized genV1beta1Variable

genV1beta1Variable :: Int -> Gen V1beta1Variable
genV1beta1Variable n =
  V1beta1Variable
    <$> arbitrary -- v1beta1VariableExpression :: Text
    <*> arbitrary -- v1beta1VariableName :: Text
  
instance Arbitrary V1beta3ExemptPriorityLevelConfiguration where
  arbitrary = sized genV1beta3ExemptPriorityLevelConfiguration

genV1beta3ExemptPriorityLevelConfiguration :: Int -> Gen V1beta3ExemptPriorityLevelConfiguration
genV1beta3ExemptPriorityLevelConfiguration n =
  V1beta3ExemptPriorityLevelConfiguration
    <$> arbitraryReducedMaybe n -- v1beta3ExemptPriorityLevelConfigurationLendablePercent :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta3ExemptPriorityLevelConfigurationNominalConcurrencyShares :: Maybe Int
  
instance Arbitrary V1beta3FlowDistinguisherMethod where
  arbitrary = sized genV1beta3FlowDistinguisherMethod

genV1beta3FlowDistinguisherMethod :: Int -> Gen V1beta3FlowDistinguisherMethod
genV1beta3FlowDistinguisherMethod n =
  V1beta3FlowDistinguisherMethod
    <$> arbitrary -- v1beta3FlowDistinguisherMethodType :: Text
  
instance Arbitrary V1beta3FlowSchema where
  arbitrary = sized genV1beta3FlowSchema

genV1beta3FlowSchema :: Int -> Gen V1beta3FlowSchema
genV1beta3FlowSchema n =
  V1beta3FlowSchema
    <$> arbitraryReducedMaybe n -- v1beta3FlowSchemaApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta3FlowSchemaKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta3FlowSchemaMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta3FlowSchemaSpec :: Maybe V1beta3FlowSchemaSpec
    <*> arbitraryReducedMaybe n -- v1beta3FlowSchemaStatus :: Maybe V1beta3FlowSchemaStatus
  
instance Arbitrary V1beta3FlowSchemaCondition where
  arbitrary = sized genV1beta3FlowSchemaCondition

genV1beta3FlowSchemaCondition :: Int -> Gen V1beta3FlowSchemaCondition
genV1beta3FlowSchemaCondition n =
  V1beta3FlowSchemaCondition
    <$> arbitraryReducedMaybe n -- v1beta3FlowSchemaConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1beta3FlowSchemaConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta3FlowSchemaConditionReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta3FlowSchemaConditionStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta3FlowSchemaConditionType :: Maybe Text
  
instance Arbitrary V1beta3FlowSchemaList where
  arbitrary = sized genV1beta3FlowSchemaList

genV1beta3FlowSchemaList :: Int -> Gen V1beta3FlowSchemaList
genV1beta3FlowSchemaList n =
  V1beta3FlowSchemaList
    <$> arbitraryReducedMaybe n -- v1beta3FlowSchemaListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta3FlowSchemaListItems :: [V1beta3FlowSchema]
    <*> arbitraryReducedMaybe n -- v1beta3FlowSchemaListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta3FlowSchemaListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta3FlowSchemaSpec where
  arbitrary = sized genV1beta3FlowSchemaSpec

genV1beta3FlowSchemaSpec :: Int -> Gen V1beta3FlowSchemaSpec
genV1beta3FlowSchemaSpec n =
  V1beta3FlowSchemaSpec
    <$> arbitraryReducedMaybe n -- v1beta3FlowSchemaSpecDistinguisherMethod :: Maybe V1beta3FlowDistinguisherMethod
    <*> arbitraryReducedMaybe n -- v1beta3FlowSchemaSpecMatchingPrecedence :: Maybe Int
    <*> arbitraryReduced n -- v1beta3FlowSchemaSpecPriorityLevelConfiguration :: V1beta3PriorityLevelConfigurationReference
    <*> arbitraryReducedMaybe n -- v1beta3FlowSchemaSpecRules :: Maybe [V1beta3PolicyRulesWithSubjects]
  
instance Arbitrary V1beta3FlowSchemaStatus where
  arbitrary = sized genV1beta3FlowSchemaStatus

genV1beta3FlowSchemaStatus :: Int -> Gen V1beta3FlowSchemaStatus
genV1beta3FlowSchemaStatus n =
  V1beta3FlowSchemaStatus
    <$> arbitraryReducedMaybe n -- v1beta3FlowSchemaStatusConditions :: Maybe [V1beta3FlowSchemaCondition]
  
instance Arbitrary V1beta3GroupSubject where
  arbitrary = sized genV1beta3GroupSubject

genV1beta3GroupSubject :: Int -> Gen V1beta3GroupSubject
genV1beta3GroupSubject n =
  V1beta3GroupSubject
    <$> arbitrary -- v1beta3GroupSubjectName :: Text
  
instance Arbitrary V1beta3LimitResponse where
  arbitrary = sized genV1beta3LimitResponse

genV1beta3LimitResponse :: Int -> Gen V1beta3LimitResponse
genV1beta3LimitResponse n =
  V1beta3LimitResponse
    <$> arbitraryReducedMaybe n -- v1beta3LimitResponseQueuing :: Maybe V1beta3QueuingConfiguration
    <*> arbitrary -- v1beta3LimitResponseType :: Text
  
instance Arbitrary V1beta3LimitedPriorityLevelConfiguration where
  arbitrary = sized genV1beta3LimitedPriorityLevelConfiguration

genV1beta3LimitedPriorityLevelConfiguration :: Int -> Gen V1beta3LimitedPriorityLevelConfiguration
genV1beta3LimitedPriorityLevelConfiguration n =
  V1beta3LimitedPriorityLevelConfiguration
    <$> arbitraryReducedMaybe n -- v1beta3LimitedPriorityLevelConfigurationBorrowingLimitPercent :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta3LimitedPriorityLevelConfigurationLendablePercent :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta3LimitedPriorityLevelConfigurationLimitResponse :: Maybe V1beta3LimitResponse
    <*> arbitraryReducedMaybe n -- v1beta3LimitedPriorityLevelConfigurationNominalConcurrencyShares :: Maybe Int
  
instance Arbitrary V1beta3NonResourcePolicyRule where
  arbitrary = sized genV1beta3NonResourcePolicyRule

genV1beta3NonResourcePolicyRule :: Int -> Gen V1beta3NonResourcePolicyRule
genV1beta3NonResourcePolicyRule n =
  V1beta3NonResourcePolicyRule
    <$> arbitrary -- v1beta3NonResourcePolicyRuleNonResourceUrls :: [Text]
    <*> arbitrary -- v1beta3NonResourcePolicyRuleVerbs :: [Text]
  
instance Arbitrary V1beta3PolicyRulesWithSubjects where
  arbitrary = sized genV1beta3PolicyRulesWithSubjects

genV1beta3PolicyRulesWithSubjects :: Int -> Gen V1beta3PolicyRulesWithSubjects
genV1beta3PolicyRulesWithSubjects n =
  V1beta3PolicyRulesWithSubjects
    <$> arbitraryReducedMaybe n -- v1beta3PolicyRulesWithSubjectsNonResourceRules :: Maybe [V1beta3NonResourcePolicyRule]
    <*> arbitraryReducedMaybe n -- v1beta3PolicyRulesWithSubjectsResourceRules :: Maybe [V1beta3ResourcePolicyRule]
    <*> arbitraryReduced n -- v1beta3PolicyRulesWithSubjectsSubjects :: [V1beta3Subject]
  
instance Arbitrary V1beta3PriorityLevelConfiguration where
  arbitrary = sized genV1beta3PriorityLevelConfiguration

genV1beta3PriorityLevelConfiguration :: Int -> Gen V1beta3PriorityLevelConfiguration
genV1beta3PriorityLevelConfiguration n =
  V1beta3PriorityLevelConfiguration
    <$> arbitraryReducedMaybe n -- v1beta3PriorityLevelConfigurationApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta3PriorityLevelConfigurationKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta3PriorityLevelConfigurationMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v1beta3PriorityLevelConfigurationSpec :: Maybe V1beta3PriorityLevelConfigurationSpec
    <*> arbitraryReducedMaybe n -- v1beta3PriorityLevelConfigurationStatus :: Maybe V1beta3PriorityLevelConfigurationStatus
  
instance Arbitrary V1beta3PriorityLevelConfigurationCondition where
  arbitrary = sized genV1beta3PriorityLevelConfigurationCondition

genV1beta3PriorityLevelConfigurationCondition :: Int -> Gen V1beta3PriorityLevelConfigurationCondition
genV1beta3PriorityLevelConfigurationCondition n =
  V1beta3PriorityLevelConfigurationCondition
    <$> arbitraryReducedMaybe n -- v1beta3PriorityLevelConfigurationConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v1beta3PriorityLevelConfigurationConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta3PriorityLevelConfigurationConditionReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta3PriorityLevelConfigurationConditionStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta3PriorityLevelConfigurationConditionType :: Maybe Text
  
instance Arbitrary V1beta3PriorityLevelConfigurationList where
  arbitrary = sized genV1beta3PriorityLevelConfigurationList

genV1beta3PriorityLevelConfigurationList :: Int -> Gen V1beta3PriorityLevelConfigurationList
genV1beta3PriorityLevelConfigurationList n =
  V1beta3PriorityLevelConfigurationList
    <$> arbitraryReducedMaybe n -- v1beta3PriorityLevelConfigurationListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v1beta3PriorityLevelConfigurationListItems :: [V1beta3PriorityLevelConfiguration]
    <*> arbitraryReducedMaybe n -- v1beta3PriorityLevelConfigurationListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1beta3PriorityLevelConfigurationListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V1beta3PriorityLevelConfigurationReference where
  arbitrary = sized genV1beta3PriorityLevelConfigurationReference

genV1beta3PriorityLevelConfigurationReference :: Int -> Gen V1beta3PriorityLevelConfigurationReference
genV1beta3PriorityLevelConfigurationReference n =
  V1beta3PriorityLevelConfigurationReference
    <$> arbitrary -- v1beta3PriorityLevelConfigurationReferenceName :: Text
  
instance Arbitrary V1beta3PriorityLevelConfigurationSpec where
  arbitrary = sized genV1beta3PriorityLevelConfigurationSpec

genV1beta3PriorityLevelConfigurationSpec :: Int -> Gen V1beta3PriorityLevelConfigurationSpec
genV1beta3PriorityLevelConfigurationSpec n =
  V1beta3PriorityLevelConfigurationSpec
    <$> arbitraryReducedMaybe n -- v1beta3PriorityLevelConfigurationSpecExempt :: Maybe V1beta3ExemptPriorityLevelConfiguration
    <*> arbitraryReducedMaybe n -- v1beta3PriorityLevelConfigurationSpecLimited :: Maybe V1beta3LimitedPriorityLevelConfiguration
    <*> arbitrary -- v1beta3PriorityLevelConfigurationSpecType :: Text
  
instance Arbitrary V1beta3PriorityLevelConfigurationStatus where
  arbitrary = sized genV1beta3PriorityLevelConfigurationStatus

genV1beta3PriorityLevelConfigurationStatus :: Int -> Gen V1beta3PriorityLevelConfigurationStatus
genV1beta3PriorityLevelConfigurationStatus n =
  V1beta3PriorityLevelConfigurationStatus
    <$> arbitraryReducedMaybe n -- v1beta3PriorityLevelConfigurationStatusConditions :: Maybe [V1beta3PriorityLevelConfigurationCondition]
  
instance Arbitrary V1beta3QueuingConfiguration where
  arbitrary = sized genV1beta3QueuingConfiguration

genV1beta3QueuingConfiguration :: Int -> Gen V1beta3QueuingConfiguration
genV1beta3QueuingConfiguration n =
  V1beta3QueuingConfiguration
    <$> arbitraryReducedMaybe n -- v1beta3QueuingConfigurationHandSize :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta3QueuingConfigurationQueueLengthLimit :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1beta3QueuingConfigurationQueues :: Maybe Int
  
instance Arbitrary V1beta3ResourcePolicyRule where
  arbitrary = sized genV1beta3ResourcePolicyRule

genV1beta3ResourcePolicyRule :: Int -> Gen V1beta3ResourcePolicyRule
genV1beta3ResourcePolicyRule n =
  V1beta3ResourcePolicyRule
    <$> arbitrary -- v1beta3ResourcePolicyRuleApiGroups :: [Text]
    <*> arbitraryReducedMaybe n -- v1beta3ResourcePolicyRuleClusterScope :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1beta3ResourcePolicyRuleNamespaces :: Maybe [Text]
    <*> arbitrary -- v1beta3ResourcePolicyRuleResources :: [Text]
    <*> arbitrary -- v1beta3ResourcePolicyRuleVerbs :: [Text]
  
instance Arbitrary V1beta3ServiceAccountSubject where
  arbitrary = sized genV1beta3ServiceAccountSubject

genV1beta3ServiceAccountSubject :: Int -> Gen V1beta3ServiceAccountSubject
genV1beta3ServiceAccountSubject n =
  V1beta3ServiceAccountSubject
    <$> arbitrary -- v1beta3ServiceAccountSubjectName :: Text
    <*> arbitrary -- v1beta3ServiceAccountSubjectNamespace :: Text
  
instance Arbitrary V1beta3Subject where
  arbitrary = sized genV1beta3Subject

genV1beta3Subject :: Int -> Gen V1beta3Subject
genV1beta3Subject n =
  V1beta3Subject
    <$> arbitraryReducedMaybe n -- v1beta3SubjectGroup :: Maybe V1beta3GroupSubject
    <*> arbitrary -- v1beta3SubjectKind :: Text
    <*> arbitraryReducedMaybe n -- v1beta3SubjectServiceAccount :: Maybe V1beta3ServiceAccountSubject
    <*> arbitraryReducedMaybe n -- v1beta3SubjectUser :: Maybe V1beta3UserSubject
  
instance Arbitrary V1beta3UserSubject where
  arbitrary = sized genV1beta3UserSubject

genV1beta3UserSubject :: Int -> Gen V1beta3UserSubject
genV1beta3UserSubject n =
  V1beta3UserSubject
    <$> arbitrary -- v1beta3UserSubjectName :: Text
  
instance Arbitrary V2ContainerResourceMetricSource where
  arbitrary = sized genV2ContainerResourceMetricSource

genV2ContainerResourceMetricSource :: Int -> Gen V2ContainerResourceMetricSource
genV2ContainerResourceMetricSource n =
  V2ContainerResourceMetricSource
    <$> arbitrary -- v2ContainerResourceMetricSourceContainer :: Text
    <*> arbitrary -- v2ContainerResourceMetricSourceName :: Text
    <*> arbitraryReduced n -- v2ContainerResourceMetricSourceTarget :: V2MetricTarget
  
instance Arbitrary V2ContainerResourceMetricStatus where
  arbitrary = sized genV2ContainerResourceMetricStatus

genV2ContainerResourceMetricStatus :: Int -> Gen V2ContainerResourceMetricStatus
genV2ContainerResourceMetricStatus n =
  V2ContainerResourceMetricStatus
    <$> arbitrary -- v2ContainerResourceMetricStatusContainer :: Text
    <*> arbitraryReduced n -- v2ContainerResourceMetricStatusCurrent :: V2MetricValueStatus
    <*> arbitrary -- v2ContainerResourceMetricStatusName :: Text
  
instance Arbitrary V2CrossVersionObjectReference where
  arbitrary = sized genV2CrossVersionObjectReference

genV2CrossVersionObjectReference :: Int -> Gen V2CrossVersionObjectReference
genV2CrossVersionObjectReference n =
  V2CrossVersionObjectReference
    <$> arbitraryReducedMaybe n -- v2CrossVersionObjectReferenceApiVersion :: Maybe Text
    <*> arbitrary -- v2CrossVersionObjectReferenceKind :: Text
    <*> arbitrary -- v2CrossVersionObjectReferenceName :: Text
  
instance Arbitrary V2ExternalMetricSource where
  arbitrary = sized genV2ExternalMetricSource

genV2ExternalMetricSource :: Int -> Gen V2ExternalMetricSource
genV2ExternalMetricSource n =
  V2ExternalMetricSource
    <$> arbitraryReduced n -- v2ExternalMetricSourceMetric :: V2MetricIdentifier
    <*> arbitraryReduced n -- v2ExternalMetricSourceTarget :: V2MetricTarget
  
instance Arbitrary V2ExternalMetricStatus where
  arbitrary = sized genV2ExternalMetricStatus

genV2ExternalMetricStatus :: Int -> Gen V2ExternalMetricStatus
genV2ExternalMetricStatus n =
  V2ExternalMetricStatus
    <$> arbitraryReduced n -- v2ExternalMetricStatusCurrent :: V2MetricValueStatus
    <*> arbitraryReduced n -- v2ExternalMetricStatusMetric :: V2MetricIdentifier
  
instance Arbitrary V2HPAScalingPolicy where
  arbitrary = sized genV2HPAScalingPolicy

genV2HPAScalingPolicy :: Int -> Gen V2HPAScalingPolicy
genV2HPAScalingPolicy n =
  V2HPAScalingPolicy
    <$> arbitrary -- v2HPAScalingPolicyPeriodSeconds :: Int
    <*> arbitrary -- v2HPAScalingPolicyType :: Text
    <*> arbitrary -- v2HPAScalingPolicyValue :: Int
  
instance Arbitrary V2HPAScalingRules where
  arbitrary = sized genV2HPAScalingRules

genV2HPAScalingRules :: Int -> Gen V2HPAScalingRules
genV2HPAScalingRules n =
  V2HPAScalingRules
    <$> arbitraryReducedMaybe n -- v2HPAScalingRulesPolicies :: Maybe [V2HPAScalingPolicy]
    <*> arbitraryReducedMaybe n -- v2HPAScalingRulesSelectPolicy :: Maybe Text
    <*> arbitraryReducedMaybe n -- v2HPAScalingRulesStabilizationWindowSeconds :: Maybe Int
  
instance Arbitrary V2HorizontalPodAutoscaler where
  arbitrary = sized genV2HorizontalPodAutoscaler

genV2HorizontalPodAutoscaler :: Int -> Gen V2HorizontalPodAutoscaler
genV2HorizontalPodAutoscaler n =
  V2HorizontalPodAutoscaler
    <$> arbitraryReducedMaybe n -- v2HorizontalPodAutoscalerApiVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v2HorizontalPodAutoscalerKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v2HorizontalPodAutoscalerMetadata :: Maybe V1ObjectMeta
    <*> arbitraryReducedMaybe n -- v2HorizontalPodAutoscalerSpec :: Maybe V2HorizontalPodAutoscalerSpec
    <*> arbitraryReducedMaybe n -- v2HorizontalPodAutoscalerStatus :: Maybe V2HorizontalPodAutoscalerStatus
  
instance Arbitrary V2HorizontalPodAutoscalerBehavior where
  arbitrary = sized genV2HorizontalPodAutoscalerBehavior

genV2HorizontalPodAutoscalerBehavior :: Int -> Gen V2HorizontalPodAutoscalerBehavior
genV2HorizontalPodAutoscalerBehavior n =
  V2HorizontalPodAutoscalerBehavior
    <$> arbitraryReducedMaybe n -- v2HorizontalPodAutoscalerBehaviorScaleDown :: Maybe V2HPAScalingRules
    <*> arbitraryReducedMaybe n -- v2HorizontalPodAutoscalerBehaviorScaleUp :: Maybe V2HPAScalingRules
  
instance Arbitrary V2HorizontalPodAutoscalerCondition where
  arbitrary = sized genV2HorizontalPodAutoscalerCondition

genV2HorizontalPodAutoscalerCondition :: Int -> Gen V2HorizontalPodAutoscalerCondition
genV2HorizontalPodAutoscalerCondition n =
  V2HorizontalPodAutoscalerCondition
    <$> arbitraryReducedMaybe n -- v2HorizontalPodAutoscalerConditionLastTransitionTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v2HorizontalPodAutoscalerConditionMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v2HorizontalPodAutoscalerConditionReason :: Maybe Text
    <*> arbitrary -- v2HorizontalPodAutoscalerConditionStatus :: Text
    <*> arbitrary -- v2HorizontalPodAutoscalerConditionType :: Text
  
instance Arbitrary V2HorizontalPodAutoscalerList where
  arbitrary = sized genV2HorizontalPodAutoscalerList

genV2HorizontalPodAutoscalerList :: Int -> Gen V2HorizontalPodAutoscalerList
genV2HorizontalPodAutoscalerList n =
  V2HorizontalPodAutoscalerList
    <$> arbitraryReducedMaybe n -- v2HorizontalPodAutoscalerListApiVersion :: Maybe Text
    <*> arbitraryReduced n -- v2HorizontalPodAutoscalerListItems :: [V2HorizontalPodAutoscaler]
    <*> arbitraryReducedMaybe n -- v2HorizontalPodAutoscalerListKind :: Maybe Text
    <*> arbitraryReducedMaybe n -- v2HorizontalPodAutoscalerListMetadata :: Maybe V1ListMeta
  
instance Arbitrary V2HorizontalPodAutoscalerSpec where
  arbitrary = sized genV2HorizontalPodAutoscalerSpec

genV2HorizontalPodAutoscalerSpec :: Int -> Gen V2HorizontalPodAutoscalerSpec
genV2HorizontalPodAutoscalerSpec n =
  V2HorizontalPodAutoscalerSpec
    <$> arbitraryReducedMaybe n -- v2HorizontalPodAutoscalerSpecBehavior :: Maybe V2HorizontalPodAutoscalerBehavior
    <*> arbitrary -- v2HorizontalPodAutoscalerSpecMaxReplicas :: Int
    <*> arbitraryReducedMaybe n -- v2HorizontalPodAutoscalerSpecMetrics :: Maybe [V2MetricSpec]
    <*> arbitraryReducedMaybe n -- v2HorizontalPodAutoscalerSpecMinReplicas :: Maybe Int
    <*> arbitraryReduced n -- v2HorizontalPodAutoscalerSpecScaleTargetRef :: V2CrossVersionObjectReference
  
instance Arbitrary V2HorizontalPodAutoscalerStatus where
  arbitrary = sized genV2HorizontalPodAutoscalerStatus

genV2HorizontalPodAutoscalerStatus :: Int -> Gen V2HorizontalPodAutoscalerStatus
genV2HorizontalPodAutoscalerStatus n =
  V2HorizontalPodAutoscalerStatus
    <$> arbitraryReducedMaybe n -- v2HorizontalPodAutoscalerStatusConditions :: Maybe [V2HorizontalPodAutoscalerCondition]
    <*> arbitraryReducedMaybe n -- v2HorizontalPodAutoscalerStatusCurrentMetrics :: Maybe [V2MetricStatus]
    <*> arbitraryReducedMaybe n -- v2HorizontalPodAutoscalerStatusCurrentReplicas :: Maybe Int
    <*> arbitrary -- v2HorizontalPodAutoscalerStatusDesiredReplicas :: Int
    <*> arbitraryReducedMaybe n -- v2HorizontalPodAutoscalerStatusLastScaleTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- v2HorizontalPodAutoscalerStatusObservedGeneration :: Maybe Integer
  
instance Arbitrary V2MetricIdentifier where
  arbitrary = sized genV2MetricIdentifier

genV2MetricIdentifier :: Int -> Gen V2MetricIdentifier
genV2MetricIdentifier n =
  V2MetricIdentifier
    <$> arbitrary -- v2MetricIdentifierName :: Text
    <*> arbitraryReducedMaybe n -- v2MetricIdentifierSelector :: Maybe V1LabelSelector
  
instance Arbitrary V2MetricSpec where
  arbitrary = sized genV2MetricSpec

genV2MetricSpec :: Int -> Gen V2MetricSpec
genV2MetricSpec n =
  V2MetricSpec
    <$> arbitraryReducedMaybe n -- v2MetricSpecContainerResource :: Maybe V2ContainerResourceMetricSource
    <*> arbitraryReducedMaybe n -- v2MetricSpecExternal :: Maybe V2ExternalMetricSource
    <*> arbitraryReducedMaybe n -- v2MetricSpecObject :: Maybe V2ObjectMetricSource
    <*> arbitraryReducedMaybe n -- v2MetricSpecPods :: Maybe V2PodsMetricSource
    <*> arbitraryReducedMaybe n -- v2MetricSpecResource :: Maybe V2ResourceMetricSource
    <*> arbitrary -- v2MetricSpecType :: Text
  
instance Arbitrary V2MetricStatus where
  arbitrary = sized genV2MetricStatus

genV2MetricStatus :: Int -> Gen V2MetricStatus
genV2MetricStatus n =
  V2MetricStatus
    <$> arbitraryReducedMaybe n -- v2MetricStatusContainerResource :: Maybe V2ContainerResourceMetricStatus
    <*> arbitraryReducedMaybe n -- v2MetricStatusExternal :: Maybe V2ExternalMetricStatus
    <*> arbitraryReducedMaybe n -- v2MetricStatusObject :: Maybe V2ObjectMetricStatus
    <*> arbitraryReducedMaybe n -- v2MetricStatusPods :: Maybe V2PodsMetricStatus
    <*> arbitraryReducedMaybe n -- v2MetricStatusResource :: Maybe V2ResourceMetricStatus
    <*> arbitrary -- v2MetricStatusType :: Text
  
instance Arbitrary V2MetricTarget where
  arbitrary = sized genV2MetricTarget

genV2MetricTarget :: Int -> Gen V2MetricTarget
genV2MetricTarget n =
  V2MetricTarget
    <$> arbitraryReducedMaybe n -- v2MetricTargetAverageUtilization :: Maybe Int
    <*> arbitraryReducedMaybe n -- v2MetricTargetAverageValue :: Maybe Quantity
    <*> arbitrary -- v2MetricTargetType :: Text
    <*> arbitraryReducedMaybe n -- v2MetricTargetValue :: Maybe Quantity
  
instance Arbitrary V2MetricValueStatus where
  arbitrary = sized genV2MetricValueStatus

genV2MetricValueStatus :: Int -> Gen V2MetricValueStatus
genV2MetricValueStatus n =
  V2MetricValueStatus
    <$> arbitraryReducedMaybe n -- v2MetricValueStatusAverageUtilization :: Maybe Int
    <*> arbitraryReducedMaybe n -- v2MetricValueStatusAverageValue :: Maybe Quantity
    <*> arbitraryReducedMaybe n -- v2MetricValueStatusValue :: Maybe Quantity
  
instance Arbitrary V2ObjectMetricSource where
  arbitrary = sized genV2ObjectMetricSource

genV2ObjectMetricSource :: Int -> Gen V2ObjectMetricSource
genV2ObjectMetricSource n =
  V2ObjectMetricSource
    <$> arbitraryReduced n -- v2ObjectMetricSourceDescribedObject :: V2CrossVersionObjectReference
    <*> arbitraryReduced n -- v2ObjectMetricSourceMetric :: V2MetricIdentifier
    <*> arbitraryReduced n -- v2ObjectMetricSourceTarget :: V2MetricTarget
  
instance Arbitrary V2ObjectMetricStatus where
  arbitrary = sized genV2ObjectMetricStatus

genV2ObjectMetricStatus :: Int -> Gen V2ObjectMetricStatus
genV2ObjectMetricStatus n =
  V2ObjectMetricStatus
    <$> arbitraryReduced n -- v2ObjectMetricStatusCurrent :: V2MetricValueStatus
    <*> arbitraryReduced n -- v2ObjectMetricStatusDescribedObject :: V2CrossVersionObjectReference
    <*> arbitraryReduced n -- v2ObjectMetricStatusMetric :: V2MetricIdentifier
  
instance Arbitrary V2PodsMetricSource where
  arbitrary = sized genV2PodsMetricSource

genV2PodsMetricSource :: Int -> Gen V2PodsMetricSource
genV2PodsMetricSource n =
  V2PodsMetricSource
    <$> arbitraryReduced n -- v2PodsMetricSourceMetric :: V2MetricIdentifier
    <*> arbitraryReduced n -- v2PodsMetricSourceTarget :: V2MetricTarget
  
instance Arbitrary V2PodsMetricStatus where
  arbitrary = sized genV2PodsMetricStatus

genV2PodsMetricStatus :: Int -> Gen V2PodsMetricStatus
genV2PodsMetricStatus n =
  V2PodsMetricStatus
    <$> arbitraryReduced n -- v2PodsMetricStatusCurrent :: V2MetricValueStatus
    <*> arbitraryReduced n -- v2PodsMetricStatusMetric :: V2MetricIdentifier
  
instance Arbitrary V2ResourceMetricSource where
  arbitrary = sized genV2ResourceMetricSource

genV2ResourceMetricSource :: Int -> Gen V2ResourceMetricSource
genV2ResourceMetricSource n =
  V2ResourceMetricSource
    <$> arbitrary -- v2ResourceMetricSourceName :: Text
    <*> arbitraryReduced n -- v2ResourceMetricSourceTarget :: V2MetricTarget
  
instance Arbitrary V2ResourceMetricStatus where
  arbitrary = sized genV2ResourceMetricStatus

genV2ResourceMetricStatus :: Int -> Gen V2ResourceMetricStatus
genV2ResourceMetricStatus n =
  V2ResourceMetricStatus
    <$> arbitraryReduced n -- v2ResourceMetricStatusCurrent :: V2MetricValueStatus
    <*> arbitrary -- v2ResourceMetricStatusName :: Text
  
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
  



