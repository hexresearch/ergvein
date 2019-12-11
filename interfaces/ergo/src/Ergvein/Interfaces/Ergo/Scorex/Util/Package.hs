module Ergvein.Interfaces.Ergo.Scorex.Util.Package where

import Data.ByteString
import Data.Serialize                     as S (Serialize (..), decode, encode, get, put)
import Data.Serialize.Get                 as S
import Data.Serialize.Put                 as S
import Data.String
import Data.Text (Text)
import Numeric.Natural
import Safe.Partial

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.Text.Encoding as TE

newtype ModifierId = ModifierId { unModifierId :: ByteString }
  deriving (Eq)

instance Serialize ModifierId where
    get = ModifierId <$> S.getBytes 32
    put = S.putByteString . unModifierId

instance Show ModifierId where
    show = show . toHex . unModifierId

instance IsString ModifierId where
    fromString = ModifierId . fromHex . fromString

toHex :: ByteString -> Text
toHex = TE.decodeUtf8 . BS16.encode

-- `Partial` constraint used to get the better stacktrace in case of an error
fromHex :: Partial => Text -> ByteString
fromHex = either error id . fromHexEither

fromHexEither :: Partial => Text -> Either String ByteString
fromHexEither bs = let
    (a, b) = BS16.decode . TE.encodeUtf8 $ bs
  in if BS.null b
      then Right a
      else Left ("Not a valid hex string: " <> show bs)
