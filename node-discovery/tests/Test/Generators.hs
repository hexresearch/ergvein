module Test.Generators where

import Data.IP
import Network.Socket
import Test.QuickCheck
import Test.QuickCheck.Instances

--------------------------------------------------------------------------
-- generators


instance Arbitrary PortNumber where
  arbitrary = fromInteger <$> arbitrary

instance Arbitrary IP where
  arbitrary = arbitrary >>= \isV4 -> 
    if isV4 then
      IPv4 . fromHostAddress <$> arbitrary 
    else
      IPv6 . fromHostAddress6 <$> arbitrary