module Ergvein.Interfaces.Ergo.Scorex.Crypto.Authds where

import Data.Aeson
import Data.ByteString
import Data.Serialize                     as S (Serialize (..), decode, encode, get, put)
import Data.Serialize.Get                 as S
import Data.Serialize.Put                 as S
import Data.String
import Data.Word

import Ergvein.Aeson

import Ergvein.Interfaces.Ergo.Scorex.Util.Package

-- Commented types are the port of original scala authds.scala source file. They are not needed yet.

-- newtype LeafData = LeafData { unLeafData :: ByteString }  --  TaggedType[Array[Byte]]

-- newtype Side = Side { unSide :: Word8 }  --  TaggedType[Byte]
--   deriving (Serialize)

newtype ADKey = ADKey { unADKey :: ByteString }  --  TaggedType[Array[Byte]]
  deriving (Eq)

instance Show ADKey where
    show = show . toHex . unADKey

instance IsString ADKey where
    fromString = ADKey . fromHex . fromString

instance ToJSON ADKey where
  toJSON = String . toHex . unADKey
  {-# INLINE toJSON #-}

instance FromJSON ADKey where
  parseJSON = withText "ADKey" $
    either fail (pure . ADKey) . fromHexTextEither
  {-# INLINE parseJSON #-}

-- newtype ADValue = ADValue { unADValue :: ByteString }  --  TaggedType[Array[Byte]]

-- newtype Balance = Balance { unBalance :: Word8 }  --  TaggedType[Byte]
--   deriving (Serialize)

-- //33 bytes! extra byte with tree height here!
newtype ADDigest = ADDigest { unADDigest :: ByteString }  --  TaggedType[Array[Byte]]
  deriving (Eq)

instance Show ADDigest where
    show = show . toHex . unADDigest

instance IsString ADDigest where
    fromString = ADDigest . fromHex . fromString

instance Serialize ADDigest where
    get = ADDigest <$> S.getBytes 33
    put = S.putByteString . unADDigest

instance ToJSON ADDigest where
  toJSON = String . toHex . unADDigest
  {-# INLINE toJSON #-}

instance FromJSON ADDigest where
  parseJSON = withText "ADDigest" $
    either fail (pure . ADDigest) . fromHexTextEither
  {-# INLINE parseJSON #-}
