module Ergvein.Types.Transaction.Ergo (
      ErgTxId(..)
    , ergTxHashToStr
    , ergTxHashFromStr
    , ErgTxRaw(..)
    , ergTxToString
    , ergTxFromString
    , ErgTx(..)
  ) where

import Control.DeepSeq
import Data.Aeson as A
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.Hashable (Hashable)
import Data.SafeCopy
import Data.Serialize (get, put)
import Data.Serialize as S
import Data.Text as T
import GHC.Generics (Generic)

import Ergvein.Crypto.Util
import Ergvein.Types.Orphanage ()
import Ergvein.Types.Transaction.Meta

import qualified Data.ByteString.Short       as BSS

newtype ErgTxId = ErgTxId { unErgTxId :: ShortByteString }
  deriving (Eq, Ord, Show, Read, Hashable, Serialize, Generic, NFData)

instance SafeCopy ErgTxId where
  putCopy = contain . put
  getCopy = contain get

ergTxHashToStr :: ErgTxId -> Text
ergTxHashToStr = encodeHex . BSS.fromShort . unErgTxId

ergTxHashFromStr :: Text -> Maybe ErgTxId
ergTxHashFromStr t = ErgTxId . BSS.toShort <$> decodeHex t

newtype ErgTxRaw = ErgTransaction ByteString
  deriving (Eq, Show, Read, Generic, Serialize)

ergTxToString :: ErgTxRaw -> Text
ergTxToString (ErgTransaction tx) = encodeHex tx

ergTxFromString :: Text -> Maybe ErgTxRaw
ergTxFromString t = ErgTransaction <$> decodeHex t

instance FromJSON ErgTxRaw where
  parseJSON = withText "ErgTxRaw" $ \t ->
    case ergTxFromString t of
      Nothing -> fail "could not decode ERGO transaction"
      Just x  -> return x

instance ToJSON ErgTxRaw where
  toJSON = A.String . ergTxToString

data ErgTx = ErgTx { getErgTx :: !ErgTxRaw, getErgTxMeta :: !(Maybe EgvTxMeta)}
  deriving (Eq, Show, Read)

instance SafeCopy ErgTx where
  putCopy (ErgTx btx meta) = contain $ put btx >> safePut meta
  getCopy = contain $ ErgTx <$> get <*> safeGet
