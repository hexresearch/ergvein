module ProtocolTest.Generators where

import Control.Monad (replicateM)
import Data.Bits
import Data.Fixed
import Test.QuickCheck
import Test.QuickCheck.Instances

import Ergvein.Types.Fees
import Ergvein.Types.Currency (Fiat)
import Ergvein.Index.Protocol.Types
import Ergvein.Index.Protocol.Serialization
import Ergvein.Index.Protocol.Deserialization

import qualified Data.Vector.Unboxed        as UV
import qualified Data.Vector                as V
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Short      as BSS
import qualified Data.ByteString.Builder    as BB
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Map.Strict            as M

--------------------------------------------------------------------------
-- generators

getRandBounded :: (Enum a, Bounded a) => Gen a
getRandBounded = oneof $ pure <$> [minBound .. maxBound]

getRandBoundedExcluding :: (Eq a, Enum a, Bounded a) => [a] -> Gen a
getRandBoundedExcluding exs = oneof $ fmap pure $ filter (\e -> not $ e `elem` exs) $ [minBound .. maxBound]

arbitraryBSSLen :: Int -> Gen BSS.ShortByteString
arbitraryBSSLen n = fmap BSS.pack $ replicateM n arbitrary

arbitraryBSLen :: Int -> Gen BS.ByteString
arbitraryBSLen n = do
  a <- arbitrary
  pure $ BS.pack $ if BS.length a < n
    then [1 .. fromIntegral n]
    else take n . BS.unpack $ a

instance Arbitrary MessageHeader where
  arbitrary = do
    t <- getRandBounded
    if messageHasPayload t
      then MessageHeader <$> pure t <*> arbitrary
      else pure $ MessageHeader t 0

instance Arbitrary ScanBlock where
  arbitrary = ScanBlock <$> getRandBounded <*> (fmap unPVT arbitrary) <*> arbitrary <*> arbitrary

-- | Bounded protocol version (<= 1023)
newtype PVT = PVT {unPVT :: ProtocolVersion}
  deriving (Show, Eq)
instance Arbitrary PVT where
  arbitrary = do
    a <- fmap ((flip shiftR 6) . (flip shiftL 6)) $ arbitrary
    b <- fmap ((flip shiftR 6) . (flip shiftL 6)) $ arbitrary
    c <- fmap ((flip shiftR 6) . (flip shiftL 6)) $ arbitrary
    pure $ PVT (a,b,c)

instance Arbitrary Version where
  arbitrary = sized $ \n ->
    Version <$> (fmap unPVT arbitrary) <*> arbitrary <*> arbitrary <*> (UV.replicateM n arbitrary)

instance Arbitrary Reject where
  arbitrary = Reject <$> getRandBounded <*> getRandBounded <*> arbitrary

instance Arbitrary FilterRequest where
  arbitrary = FilterRequest <$> getRandBounded <*> arbitrary <*> arbitrary

instance Arbitrary BlockFilter where
  arbitrary = BlockFilter <$> arbitraryBSSLen 32 <*> arbitrary

instance Arbitrary FilterResponse where
  arbitrary = sized $ \n -> FilterResponse <$> getRandBounded <*> (V.replicateM n arbitrary)

instance Arbitrary FilterEvent where
  arbitrary = sized $ \n -> FilterEvent <$> getRandBounded <*> arbitrary <*> arbitraryBSSLen 32 <*> arbitrary

instance Arbitrary FeeResp where
  arbitrary = let
    gen1 = FeeRespBTC <$> arbitrary <*> (FeeBundle <$> arbitrary <*> arbitrary <*> arbitrary)
    gen2 = FeeRespGeneric <$> getRandBoundedExcluding [BTC, TBTC] <*> arbitrary <*> arbitrary <*> arbitrary
    in oneof [gen1, gen2]

instance Arbitrary IpV6 where
  arbitrary = IpV6 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Address where
  arbitrary = do
    at <- arbitrary
    case at of
      IPV4 -> AddressIpv4 <$> arbitrary <*> arbitrary
      IPV6 -> AddressIpv6 <$> arbitrary <*> arbitrary
      OnionV3 -> AddressOnionV3 <$> arbitraryBSLen 56 <*> arbitrary

instance Arbitrary PeerResponse where
  arbitrary = sized $ \n -> PeerResponse <$> arbitrary

instance Arbitrary PeerIntroduce where
  arbitrary = sized $ \n -> PeerIntroduce <$> arbitrary

instance Arbitrary CurrencyCode where
  arbitrary = getRandBounded

instance Arbitrary IPType where
  arbitrary = getRandBounded

instance Arbitrary Fiat where
  arbitrary = getRandBounded

newtype Fiats = Fiats {unFiats :: [Fiat]}
instance Arbitrary Fiats where
  arbitrary = fmap Fiats $ sized $ flip replicateM arbitrary

newtype FDS = FDS {unFDS :: M.Map Fiat Centi}
instance Arbitrary FDS where
  arbitrary = fmap (FDS . M.fromList) $ sized $
    flip replicateM $ (,) <$> getRandBounded <*> (fmap abs arbitrary)

instance Arbitrary RatesRequest where
  arbitrary = fmap (RatesRequest . M.fromList) $ sized $
    flip replicateM $ (,) <$> getRandBounded <*> (fmap unFiats arbitrary)

instance Arbitrary RatesResponse where
  arbitrary = fmap (RatesResponse . M.fromList) $ sized $
    flip replicateM $ (,) <$> getRandBounded <*> (fmap unFDS arbitrary)

instance Arbitrary MempoolFilter where
  arbitrary = MempoolFilter <$> arbitrary

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
  , MRatesRequestType
  , MRatesResponseType
  , MFullFilterInvType
  , MGetFullFilterType
  , MGetMemFiltersType
  , MFullFilterType
  , MMemFiltersType
  , MGetMempoolType
  , MMempoolChunkType
  ]

instance Arbitrary Message where
  arbitrary = do
    msgType <- oneof $ fmap pure fullyImplementedMessageTypes
    case msgType of
      MVersionType          -> MVersion <$> arbitrary
      MVersionACKType       -> pure $ MVersionACK VersionACK
      MFiltersRequestType   -> MFiltersRequest <$> arbitrary
      MFiltersResponseType  -> MFiltersResponse <$> arbitrary
      MRejectType           -> MReject <$> arbitrary
      MPingType             -> MPing <$> arbitrary
      MPongType             -> MPong <$> arbitrary
      MFilterEventType      -> MFiltersEvent <$> arbitrary
      MPeerRequestType      -> pure $ MPeerRequest PeerRequest
      MPeerResponseType     -> MPeerResponse <$> arbitrary
      MIntroducePeerType    -> MPeerIntroduce <$> arbitrary
      MFeeRequestType       -> MFeeRequest <$> arbitrary
      MFeeResponseType      -> MFeeResponse <$> arbitrary
      MRatesRequestType     -> MRatesRequest <$> arbitrary
      MRatesResponseType    -> MRatesResponse <$> arbitrary
      MFullFilterInvType    -> pure $ MFullFilterInv $ FullFilterInv
      MGetFullFilterType    -> pure $ MGetFullFilter $ GetFullFilter
      MGetMemFiltersType    -> pure $ MGetMemFilters $ GetMemFilters
      MFullFilterType       -> MFullFilter . MempoolFilter <$> arbitrary
      MMemFiltersType       -> MMemFilters . FilterTree <$> arbitrary
      MGetMempoolType       -> MGetMempool . GetMempool <$> arbitrary
      MMempoolChunkType     -> fmap MMempoolChunk $ MempoolChunk <$> arbitrary <*> arbitrary
--------------------------------------------------------------------------
-- newtype wrappers
