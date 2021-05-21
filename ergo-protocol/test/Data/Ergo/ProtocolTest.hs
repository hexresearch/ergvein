module Data.Ergo.ProtocolTest where

import Control.Monad
import Data.ByteString.Builder
import Data.Either
import Data.Ergo.Block
import Data.Ergo.Modifier
import Data.Ergo.Protocol
import Data.Ergo.Protocol.Decoder
import Data.Ergo.Protocol.Encoder
import Data.Int
import Data.Maybe
import Data.Persist
import Data.Text (Text)
import Data.Text.Encoding
import Data.Vector (Vector)
import Data.Word
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Instances.Vector ()
import Test.QuickCheck.Utf8
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Vector as V

import Debug.Trace

traceShowIdHex :: BS.ByteString -> BS.ByteString
traceShowIdHex a = traceShow (B16.encode a) a

unit_handshakeParse1 :: IO ()
unit_handshakeParse1 = do
    let mh = runGet handshakeParser handshakeBS
    h <- either (assertFailure . ("Handshake parse: " <>)) pure mh
    -- print h
    pure ()
  where
    handshakeBS = fst $ B16.decode $ "e9f4839dff2e076572676f726566030306126572676f2d6d61696e6e65742d342e302e3001087f000001bc4603100400010001030d01000204fcb48fcdb2b3b4850c02067f000001c646"


prop_encodeDecodeHandshake :: Handshake -> Bool
prop_encodeDecodeHandshake msg = decode (encode msg) == Right msg

-- prop_encodeDecodeSyncInfo :: SyncInfo -> Bool
-- prop_encodeDecodeSyncInfo msg = decode (encode $ TestnetMessage $ MsgSyncInfo msg) == Right (TestnetMessage $ MsgSyncInfo msg)

-- prop_encodeDecodeTestnet :: TestnetMessage -> Bool
-- prop_encodeDecodeTestnet msg = decode (encode msg) == Right msg

prop_encodeDecodeMainnet :: MainnetMessage -> Bool
prop_encodeDecodeMainnet msg = decode (encode msg) == Right msg

instance Arbitrary TestnetMessage where
  arbitrary = TestnetMessage <$> arbitrary
  shrink = genericShrink

instance Arbitrary MainnetMessage where
  arbitrary = MainnetMessage <$> arbitrary
  shrink = genericShrink

instance Arbitrary Message where
  arbitrary = oneof [
      MsgSyncInfo <$> arbitrary
    , MsgInv <$> arbitrary
    ]
  shrink = genericShrink

instance Arbitrary ProtoVer where
  arbitrary = ProtoVer <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary IP where
  arbitrary = frequency [(3, IPV4 <$> arbitrary), (1, IPV6 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)]
  shrink = genericShrink

instance Arbitrary NetAddr where
  arbitrary = NetAddr <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary StateType where
  arbitrary = frequency [(1, pure StateUtxo), (1, pure StateDigest)]

instance Arbitrary OperationModeFeature where
  arbitrary = OperationModeFeature
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink = genericShrink

instance Arbitrary SessionFeature where
  arbitrary = SessionFeature
    <$> arbitrary
    <*> arbitrary
  shrink = genericShrink

instance Arbitrary LocalAddressFeature where
  arbitrary = LocalAddressFeature <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary PeerFeature where
  arbitrary = oneof [
      FeatureOperationMode <$> arbitrary
    , FeatureSession <$> arbitrary
    , FeatureLocalAddress <$> arbitrary
    ]
  shrink = genericShrink

instance Arbitrary SyncInfo where
  arbitrary = SyncInfo <$> arbitrary
  shrink = genericShrink

instance Arbitrary InvMsg where
  arbitrary = InvMsg <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary RequestModifierMsg where
  arbitrary = RequestModifierMsg <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary ModifierMsg where
  arbitrary = ModifierMsg <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary ModifierId where
  arbitrary = ModifierId . BS8.pack <$> replicateM 32 arbitrary
  shrink = shrinkNothing

instance Arbitrary ModifierType where
  arbitrary = oneof [
      pure ModifierTx
    , pure ModifierBlockHeader
    , pure ModifierBlockTxs
    , pure ModifierBlockProof
    , pure ModifierBlockExt
    ]
  shrink = genericShrink

instance Arbitrary Modifier where
  arbitrary = UnknownModifierBody <$> arbitrary <*> (BS8.pack <$> replicateM 10 arbitrary)
  shrink = shrinkNothing

instance Arbitrary Handshake where
  arbitrary = Handshake
    <$> (getNonNegative <$> (arbitrary :: Gen (NonNegative Int64)))
    <*> arbitraryTextLimit 10
    <*> arbitrary
    <*> arbitraryTextLimit 10
    <*> arbitrary
    <*> arbitraryVecLimit 10
  -- shrink v = []
  shrink v =
      (if V.null (peerFeatures v) then [] else [
          v { peerFeatures = V.drop 1 $ peerFeatures v }
        , v { peerFeatures = V.init $ peerFeatures v }
        ])
      ++
      (if T.null (agentName v) then [] else [v { agentName = ""}])
      ++
      (if T.null (peerName v) then [] else [v { peerName = ""}])
      ++
      (if isNothing (publicAddr v) then [] else [ v { publicAddr = Nothing }])

instance Arbitrary Text where
  arbitrary = genValidUtf8
  shrink v = [ T.drop i v | i <- [1 .. T.length v] ]

arbitraryTextLimit :: Int -> Gen Text
arbitraryTextLimit n = do
  i <- choose (0, n)
  t <- T.pack <$> replicateM i arbitrary
  pure $ fitSize t
  where
    fitSize t | BS.length (encodeUtf8 t) <= n = t
              | otherwise = fitSize $ T.init t

arbitraryVecLimit :: Arbitrary a => Int -> Gen (Vector a)
arbitraryVecLimit n = do
  i <- choose (0, n)
  V.replicateM i arbitrary
