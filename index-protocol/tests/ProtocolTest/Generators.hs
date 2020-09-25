module ProtocolTest.Generators where

import Control.Monad (replicateM)
import Test.QuickCheck
import Test.QuickCheck.Instances

import Ergvein.Types.Fees
import Ergvein.Index.Protocol.Types
import Ergvein.Index.Protocol.Serialization
import Ergvein.Index.Protocol.Deserialization

import qualified Data.Vector.Unboxed        as UV
import qualified Data.Vector                as V
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BB
import qualified Data.Attoparsec.ByteString as AP

--------------------------------------------------------------------------
-- generators

getRandBounded :: (Enum a, Bounded a) => Gen a
getRandBounded = oneof $ pure <$> [minBound .. maxBound]

getRandBoundedExcluding :: (Eq a, Enum a, Bounded a) => [a] -> Gen a
getRandBoundedExcluding exs = oneof $ fmap pure $ filter (\e -> not $ e `elem` exs) $ [minBound .. maxBound]

arbitraryBSLen :: Int -> Gen BS.ByteString
arbitraryBSLen n = do
  a <- arbitrary
  pure $ BS.pack $ if BS.length a < n
    then [1..(fromIntegral n)]
    else take n . BS.unpack $ a

instance Arbitrary MessageHeader where
  arbitrary = MessageHeader <$> getRandBounded <*> arbitrary

instance Arbitrary ScanBlock where
  arbitrary = ScanBlock <$> getRandBounded <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Version where
  arbitrary = sized $ \n ->
    Version <$> arbitrary <*> arbitrary <*> arbitrary <*> (UV.replicateM n arbitrary)

instance Arbitrary FilterRequest where
  arbitrary = FilterRequest <$> getRandBounded <*> arbitrary <*> arbitrary

instance Arbitrary BlockFilter where
  arbitrary = BlockFilter <$> arbitrary <*> arbitrary

instance Arbitrary FilterResponse where
  arbitrary = sized $ \n -> FilterResponse <$> getRandBounded <*> (V.replicateM n arbitrary)

instance Arbitrary FilterEvent where
  arbitrary = sized $ \n -> FilterEvent <$> getRandBounded <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary FeeResp where
  arbitrary = let
    gen1 = FeeRespBTC <$> arbitrary <*> (FeeBundle <$> arbitrary <*> arbitrary <*> arbitrary)
    gen2 = FeeRespGeneric <$> getRandBoundedExcluding [BTC, TBTC] <*> arbitrary <*> arbitrary <*> arbitrary
    in oneof [gen1, gen2]

instance Arbitrary Address where
  arbitrary = do
    at <- arbitrary
    case at of
      IPV4 -> Address at <$> arbitrary <*> arbitraryBSLen 4
      IPV6 -> Address at <$> arbitrary <*> arbitraryBSLen 16

instance Arbitrary PeerResponse where
  arbitrary = sized $ \n -> PeerResponse <$> arbitrary

instance Arbitrary PeerIntroduce where
  arbitrary = sized $ \n -> PeerIntroduce <$> arbitrary

instance Arbitrary CurrencyCode where
  arbitrary = getRandBounded

instance Arbitrary IPType where
  arbitrary = getRandBounded

unimplementedMessageTypes :: [MessageType]
unimplementedMessageTypes =
  []

fullyImplementedMessageTypes :: [MessageType]
fullyImplementedMessageTypes =
  [ MPeerRequestType
  , MPeerResponseType
  , MIntroducePeerType
  , MPingType
  , MPongType
  , MRejectType
  , MVersionACKType
  , MVersionType
  , MFeeRequestType
  , MFeeResponseType
  , MFiltersRequestType
  , MFiltersResponseType
  , MFilterEventType
  ]

instance Arbitrary Message where
  arbitrary = do
    msgType <- oneof $ fmap pure fullyImplementedMessageTypes
    case msgType of
      MVersionType          -> MVersion <$> arbitrary
      MVersionACKType       -> pure $ MVersionACK VersionACK
      MFiltersRequestType   -> MFiltersRequest <$> arbitrary
      MFiltersResponseType  -> MFiltersResponse <$> arbitrary
      MRejectType           -> (MReject . Reject) <$> getRandBounded
      MPingType             -> MPing <$> arbitrary
      MPongType             -> MPong <$> arbitrary
      MFilterEventType      -> MFiltersEvent <$> arbitrary
      MPeerRequestType      -> pure $ MPeerRequest PeerRequest
      MPeerResponseType     -> MPeerResponse <$> arbitrary
      MIntroducePeerType    -> MPeerIntroduce <$> arbitrary
      MFeeRequestType       -> MFeeRequest <$> arbitrary
      MFeeResponseType      -> MFeeResponse <$> arbitrary


--------------------------------------------------------------------------
-- newtype wrappers
