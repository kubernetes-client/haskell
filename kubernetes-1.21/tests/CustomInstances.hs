module CustomInstances where

import Kubernetes.OpenAPI.Model

import Test.QuickCheck
import Data.Text (pack)

instance Arbitrary IntOrString where
  arbitrary =
    oneof [ IntOrStringI <$> arbitrary
          , IntOrStringS <$> (pack <$> arbitrary)
          ]

instance Arbitrary Quantity where
  arbitrary = Quantity <$> (pack <$> arbitrary)
