module ProtocolTest.Generators where

import Control.Monad (replicateM)
import Test.QuickCheck
import Test.QuickCheck.Instances

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

instance Arbitrary MessageHeader where
  arbitrary = MessageHeader <$> getRandBounded <*> arbitrary

instance Arbitrary ScanBlock where
  arbitrary = ScanBlock <$> getRandBounded <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary VersionMessage where
  arbitrary = sized $ \n ->
    VersionMessage <$> arbitrary <*> arbitrary <*> arbitrary <*> (UV.replicateM n arbitrary)

instance Arbitrary FilterRequestMessage where
  arbitrary = FilterRequestMessage <$> getRandBounded <*> arbitrary <*> arbitrary

instance Arbitrary BlockFilter where
  arbitrary = BlockFilter <$> arbitrary <*> arbitrary

instance Arbitrary FilterResponseMessage where
  arbitrary = sized $ \n -> FilterResponseMessage <$> getRandBounded <*> (V.replicateM n arbitrary)

instance Arbitrary FilterResponseIncrementalMessage where
  arbitrary = sized $ \n -> FilterResponseIncrementalMessage <$> getRandBounded <*> arbitrary <*> (replicateM n arbitrary)

unimplementedMessageTypes :: [MessageType]
unimplementedMessageTypes =
  [ FilterEvent
  , PeerRequest
  , PeerResponse
  , FeeRequest
  , FeeResponse
  , IntroducePeer
  ]

fullyImplementedMessageTypes :: [MessageType]
fullyImplementedMessageTypes =
  [ Ping
  , Pong
  , Reject
  , VersionACK
  ]

instance Arbitrary Message where
  arbitrary = do
    -- msgType <- oneof $ fmap pure $ filter (\t -> not $ t `elem` unimplementedMessageTypes) [minBound .. maxBound]
    msgType <- oneof $ fmap pure fullyImplementedMessageTypes
    case msgType of
      Version -> VersionMsg <$> arbitrary
      VersionACK -> pure $ VersionACKMsg VersionACKMessage
      FiltersRequest -> FiltersRequestMsg <$> arbitrary
      FiltersResponse -> do
        b <- arbitrary
        if b then FiltersResponseMsg <$> arbitrary else FiltersResponseIncrementalMsg <$> arbitrary
      Reject -> (RejectMsg . RejectMessage) <$> getRandBounded
      Ping -> PingMsg <$> arbitrary
      Pong -> PongMsg <$> arbitrary
      FilterEvent   -> error "Message type: FilterEvent is not implemented"
      PeerRequest   -> error "Message type: PeerRequest is not implemented"
      PeerResponse  -> error "Message type: PeerResponse is not implemented"
      FeeRequest    -> error "Message type: FeeRequest is not implemented"
      FeeResponse   -> error "Message type: FeeResponse is not implemented"
      IntroducePeer -> error "Message type: IntroducePeer is not implemented"

--------------------------------------------------------------------------
-- newtype wrappers
