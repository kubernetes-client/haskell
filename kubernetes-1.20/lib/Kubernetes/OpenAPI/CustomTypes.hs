{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Kubernetes.OpenAPI.CustomTypes where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as A
import Data.Data (Typeable)
import Data.Text (Text)
import GHC.Base (mzero)

import GHC.Generics

{- | `IntOrString`
  IntOrString is a type that can hold an int32 or a string.  When used
  in JSON or YAML marshalling and unmarshalling, it produces or consumes
  the inner type.  This allows you to have, for example, a JSON field
  that can accept a name or number.
-}
data IntOrString
  = IntOrStringS Text
  | IntOrStringI Int
  deriving (Show, Eq, Typeable, Generic)

instance FromJSON IntOrString where
  parseJSON (A.String t) = return $ IntOrStringS t
  parseJSON (A.Number n) = return $ IntOrStringI (round n)
  parseJSON _ = mzero

instance ToJSON IntOrString where
  toJSON (IntOrStringS t) = A.String t
  toJSON (IntOrStringI n) = A.Number (fromIntegral n)

{- | `Quantity` is a fixed-point representation of a number.
  
  It provides convenient marshaling/unmarshaling in JSON and YAML, in
  addition to String() and Int64() accessors.
  
  The serialization format is:
  
  @
  \<quantity\>        ::= \<signedNumber\>\<suffix\>
  (Note that \<suffix\> may be empty, from the \"\" case in \<decimalSI\>.)
  \<digit\>           ::= 0 | 1 | ... | 9
  \<digits\>          ::= \<digit\> | \<digit\>\<digits\>
  \<number\>          ::= \<digits\> | \<digits\>.\<digits\> | \<digits\>. | .\<digits\>
  \<sign\>            ::= \"+\" | \"-\" \<signedNumber\>    ::= \<number\> | \<sign\>\<number\>
  \<suffix\>          ::= \<binarySI\> | \<decimalExponent\> | \<decimalSI\>
  \<binarySI\>        ::= Ki | Mi | Gi | Ti | Pi | Ei
  (International System of units; See: http://physics.nist.gov/cuu/Units/binary.html)
  \<decimalSI\>       ::= m | \"\" | k | M | G | T | P | E
  (Note that 1024 = 1Ki but 1000 = 1k; I didn't choose the capitalization.)
  \<decimalExponent\> ::= \"e\" \<signedNumber\> | \"E\" \<signedNumber\>
  @
  
  No matter which of the three exponent forms is used, no quantity may
  represent a number greater than 2^63-1 in magnitude, nor may it have
  more than 3 decimal places. Numbers larger or more precise will be
  capped or rounded up. (E.g.: 0.1m will rounded up to 1m.) This may be
  extended in the future if we require larger or smaller quantities.
  
  When a Quantity is parsed from a string, it will remember the type of
  suffix it had, and will use the same type again when it is serialized.
  
  Before serializing, Quantity will be put in "canonical form". This
  means that Exponent/suffix will be adjusted up or down (with a
  corresponding increase or decrease in Mantissa) such that:

    - No precision is lost
    - No fractional digits will be emitted
    - The exponent (or suffix) is as large as possible.

  The sign will be omitted unless the number is negative.
  
  Examples:

    - 1.5 will be serialized as "1500m"
    - 1.5Gi will be serialized as "1536Mi"
  
  Note that the quantity will NEVER be internally represented by a
  floating point number. That is the whole point of this exercise.
  
  Non-canonical values will still parse as long as they are well formed,
  but will be re-emitted in their canonical form. (So always use
  canonical form, or don't diff.)
  
  This format is intended to make it difficult to use these numbers
  without writing some sort of special handling code in the hopes that
  that will cause implementors to also use a fixed point implementation.
-}
newtype Quantity = Quantity { unQuantity :: Text }
  deriving (Show, Eq, Typeable, Generic)

instance FromJSON Quantity where
  parseJSON (A.String t) = return $ Quantity t
  parseJSON _ = mzero

instance ToJSON Quantity where
  toJSON (Quantity t) = A.String t
