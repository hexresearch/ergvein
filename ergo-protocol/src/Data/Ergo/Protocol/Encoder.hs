module Data.Ergo.Protocol.Encoder(
    encodeMessage
  , encodeHandshake
  , messageEncoder
  , handshakeEncoder
  ) where

import Data.Bits
import Control.Monad
import Data.ByteString (ByteString)
import Data.Ergo.Protocol.Check
import Data.Ergo.Protocol.Shift
import Data.Ergo.Protocol.Types
import Data.Foldable (traverse_)
import Data.Int
import Data.Persist
import Data.Text (Text)
import Data.Text.Encoding
import Data.Vector (Vector)
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Vector as V

int64BE :: Int64 -> Put ()
int64BE = put . BigEndian

int32BE :: Int32 -> Put ()
int32BE = put . BigEndian

word64BE :: Word64 -> Put ()
word64BE = put . BigEndian

word32BE :: Word32 -> Put ()
word32BE = put . BigEndian

word32LE :: Word32 -> Put ()
word32LE = put . LittleEndian

word16BE :: Word16 -> Put ()
word16BE = put . BigEndian

word8 :: Word8 -> Put ()
word8 = put

vlq :: (Integral a, ShiftRS a, Bits a) => a -> Put ()
vlq w | w .&. complement 0x7F == 0 = word8 $ fromIntegral w
      | otherwise = do
        word8 $ fromIntegral ((fromIntegral (w .&. 0x7F) :: Word32) .|. 0x80)
        vlq $ shiftRS w 7

int64Vlq :: Int64 -> Put ()
int64Vlq = vlq

word16Vlq :: Word16 -> Put ()
word16Vlq = vlq

encodeMessage :: Network -> Message -> ByteString
encodeMessage net msg = runPut $ messageEncoder net msg

encodeHandshake :: Handshake -> ByteString
encodeHandshake = runPut . handshakeEncoder

messageEncoder :: Network -> Message -> Put ()
messageEncoder net msg = case msg of
  MsgHandshake hmsg -> handshakeEncoder hmsg
  MsgSyncInfo smsg -> wrapBody syncInfoId $ syncInfoEncoder smsg
  where
    wrapBody i b = do
      word32BE (magicBytes net)
      word8 i
      word32BE l
      when (l > 0) $ do
        putByteString bbody
        putByteString (checkSum bbody)
      where
        l = fromIntegral $ BS.length bbody
        bbody = runPut b

encodeVarText :: Text -> Put ()
encodeVarText t
  | l > 255   = encodeVarText $ T.init t
  | otherwise = word8 (fromIntegral l) >> putByteString bs
  where
    l = BS.length bs
    bs = encodeUtf8 t

encodeVersion :: ProtoVer -> Put ()
encodeVersion (ProtoVer a b c) = word8 a >> word8 b >> word8 c

encodeOptional :: (a -> Put ()) -> Maybe a -> Put ()
encodeOptional _ Nothing = word8 0
encodeOptional f (Just a) = word8 1 >> f a

encodeIP :: IP -> Put ()
encodeIP (IPV4 a) = word8 4 >> word32LE a
encodeIP (IPV6 a b c d) = word8 16 >> word32LE a >> word32LE b >> word32LE c >> word32LE d

encodeNetAddr :: NetAddr -> Put ()
encodeNetAddr (NetAddr ip p) = encodeIP ip >> word32BE p

encodeVector :: (a -> Put ()) -> Vector a -> Put ()
encodeVector f v = word8 l >> traverse_ f (V.take (fromIntegral l) v)
  where
    l = fromIntegral $ min 255 $ V.length v

encodeBool :: Bool -> Put ()
encodeBool v = word8 $ if v then 1 else 0

encodeStateType :: StateType -> Put ()
encodeStateType StateUtxo = word8 0
encodeStateType StateDigest = word8 1

handshakeEncoder :: Handshake -> Put ()
handshakeEncoder Handshake{..} = do
  int64Vlq time
  encodeVarText agentName
  encodeVersion version
  encodeVarText peerName
  encodeOptional encodeNetAddr publicAddr
  encodeVector encodeFeature peerFeatures

encodeOpMode :: OperationModeFeature -> Put ()
encodeOpMode OperationModeFeature{..} = do
  encodeStateType stateType
  encodeBool verifying
  encodeOptional word32BE nipopowSuffix
  int32BE blocksStored

encodeSessionFeature :: SessionFeature -> Put ()
encodeSessionFeature SessionFeature{..} = do
  word32BE networkMagic
  word64BE sessionId

encodeFeature :: PeerFeature -> Put ()
encodeFeature v = do
  word8 $ featureId v
  word16Vlq (fromIntegral $ BS.length bs)
  putByteString bs
  where
    bs = case v of
      FeatureOperationMode v -> runPut $ encodeOpMode v
      FeatureSession v -> runPut $ encodeSessionFeature v
      UnknownFeature _ bs -> bs

syncInfoEncoder :: SyncInfo -> Put ()
syncInfoEncoder SyncInfo{..} = encodeVector headerIdEncoder syncHeaders

headerIdEncoder :: HeaderId -> Put ()
headerIdEncoder (HeaderId h) = putByteString h
