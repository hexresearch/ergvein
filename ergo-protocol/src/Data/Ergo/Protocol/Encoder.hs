module Data.Ergo.Protocol.Encoder(
    encodeMessage
  ) where

import Data.ByteString.Builder
import Data.ByteString.Lazy (ByteString)
import Data.Ergo.Protocol.Check
import Data.Ergo.Protocol.Types
import Data.Text (Text)
import Data.Text.Encoding
import Data.Vector (Vector)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V

encodeMessage :: Network -> Message -> Builder
encodeMessage net msg = case msg of
  MsgHandshake hmsg -> mconcat [
      word32BE (magicBytes net)
    , word8 handshakeId
    , word32BE (fromIntegral $ BS.length bbody)
    , byteString bbody
    , byteString (checkSum bbody)
    ]
    where
      bbody = BSL.toStrict . toLazyByteString $! encodeHandshake hmsg

encodeVarText :: Text -> Builder
encodeVarText t = word8 l <> byteString (BS.take (fromIntegral l) bs)
  where
    l = fromIntegral $ min 256 $ BS.length bs
    bs = encodeUtf8 t

encodeVersion :: ProtoVer -> Builder
encodeVersion (ProtoVer a b c) = word8 a <> word8 b <> word8 c

encodeOptional :: (a -> Builder) -> Maybe a -> Builder
encodeOptional _ Nothing = word8 0
encodeOptional f (Just a) = word8 1 <> f a

encodeIP :: IP -> Builder
encodeIP (IPV4 a) = word8 4 <> word32LE a
encodeIP (IPV6 a b c d) = word8 16 <> word32LE a <> word32LE b <> word32LE c <> word32LE d

encodeNetAddr :: NetAddr -> Builder
encodeNetAddr (NetAddr ip p) = encodeIP ip <> word32BE p

encodeVector :: (a -> Builder) -> Vector a -> Builder
encodeVector f v = word8 l <> foldMap f v
  where
    l = fromIntegral $ min 256 $ V.length v

encodeBool :: Bool -> Builder
encodeBool v = word8 $ if v then 1 else 0

encodeStateType :: StateType -> Builder
encodeStateType StateUtxo = word8 0
encodeStateType StateDigest = word8 1

encodeHandshake :: Handshake -> Builder
encodeHandshake Handshake{..} = mconcat [
    int64BE time
  , encodeVarText agentName
  , encodeVersion version
  , encodeVarText peerName
  , encodeOptional encodeNetAddr publicAddr
  , encodeVector encodeFeature peerFeatures
  ]

encodeOpMode :: OperationModeFeature -> Builder
encodeOpMode OperationModeFeature{..} = mconcat [
    encodeStateType stateType
  , encodeBool verifying
  , encodeOptional word32BE nipopowSuffix
  , maybe (int32BE (-1)) word32BE blocksStored
  ]

encodeFeature :: PeerFeature -> Builder
encodeFeature (FeatureOperationMode v) =
     word8 featureOperationModeId
  <> word16BE (fromIntegral $ BS.length bs)
  <> byteString bs
  where
    bs = BSL.toStrict . toLazyByteString $ encodeOpMode v
encodeFeature (UnknownFeature i bs) =
     word8 i
  <> word16BE (fromIntegral $ BS.length bs)
  <> byteString bs 
