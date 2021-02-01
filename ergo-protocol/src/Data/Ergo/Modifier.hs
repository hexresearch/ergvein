module Data.Ergo.Modifier(
    BlockId
  , ModifierId(..)
  , encodeModifierId
  , decodeModifierId
  , nullModifierId
  , ModifierType(..)
  , encodeModifierType
  , decodeModifierType
  , Modifier(..)
  ) where

import Data.ByteString (ByteString)
import Data.Ergo.Vlq
import Data.Maybe (fromMaybe)
import Data.Persist
import Data.String
import Data.Text.Encoding
import Data.Text (Text, pack)
import Data.Word
import GHC.Generics

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as BS

type BlockId = ModifierId

-- | 32 Byte hash of something (block, tx)
newtype ModifierId = ModifierId { unModifierId :: ByteString }
  deriving(Eq, Ord, Show, Read)

instance Persist ModifierId where
  put = putByteString . unModifierId
  {-# INLINE put #-}
  get = fmap ModifierId $ getBytes 32
  {-# INLINE get #-}

-- | Convert header to hex string
encodeModifierId :: ModifierId -> Text
encodeModifierId = decodeUtf8 . B16.encode . unModifierId

-- | Convert hex string to header hash. Need to be 32 byte length.
decodeModifierId :: Text -> Maybe ModifierId
decodeModifierId = check . fst . B16.decode . encodeUtf8
  where
    check bs | BS.length bs == 32 = Just $ ModifierId bs
             | otherwise = Nothing

instance IsString ModifierId where
  fromString s = fromMaybe (error $ "Failed to parse modifier id: " <> s) . decodeModifierId . pack $! s

-- | Modifier id that is filled with zeros. It is used as request for recent
-- headers.
nullModifierId :: ModifierId
nullModifierId = ModifierId $ BS.replicate 32 0

-- | Modifier type tag
data ModifierType =
    ModifierTx
  | ModifierBlockHeader -- ^ Header of block
  | ModifierBlockTxs -- ^ Part of block with txs
  | ModifierBlockProof -- ^ Proof for valid state transformation
  | ModifierBlockExt -- ^ Block extension (including NiPoPow vector)
  | UnknownModifier !Word8
  deriving (Generic, Show, Read, Eq)

encodeModifierType :: ModifierType -> Word8
encodeModifierType v = case v of
  ModifierTx -> 2
  ModifierBlockHeader -> 101
  ModifierBlockTxs -> 102
  ModifierBlockProof -> 104
  ModifierBlockExt -> 108
  UnknownModifier w -> w

decodeModifierType :: Word8 -> ModifierType
decodeModifierType w = case w of
  2 -> ModifierTx
  101 -> ModifierBlockHeader
  102 -> ModifierBlockTxs
  104 -> ModifierBlockProof
  108 -> ModifierBlockExt
  _ -> UnknownModifier w

instance Persist ModifierType where
  put = put . encodeModifierType
  {-# INLINE put #-}
  get = fmap decodeModifierType get
  {-# INLINE get #-}

-- | Modifier data like blocks headers, block bodies or transactions or proofs or block extensions
-- TODO: parse content
data Modifier = UnknownModifierBody {
    modifierId   :: !ModifierId
  , modifierBody :: !ByteString
  } deriving (Generic, Show, Read, Eq)

instance Persist Modifier where
  put (UnknownModifierBody i bs) = do
    put i
    encodeVlq (fromIntegral $ BS.length bs :: Word32)
    putByteString bs
  {-# INLINE put #-}
  get = do
    mid <- get
    l :: Word32 <- decodeVlq
    bs <- getByteString (fromIntegral l)
    pure $ UnknownModifierBody mid bs
  {-# INLINE get #-}
