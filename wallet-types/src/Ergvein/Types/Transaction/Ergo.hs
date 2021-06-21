module Ergvein.Types.Transaction.Ergo (
      ErgTxId(..)
    , ergTxHashToStr
    , ergTxHashFromStr
    , ErgTxRaw
    , ergTxToString
    , ergTxFromString
    , ErgTx(..)
    , mockErgoTx
    , module Data.Ergo.Transaction
  ) where

import Control.DeepSeq
-- import Data.Aeson as A
-- import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.Hashable (Hashable)
import Data.SafeCopy
import Data.Ergo.Block (Digest32(..))
import Data.Maybe (fromMaybe)
import Data.Serialize (get, put)
import Data.Serialize as S
import Data.Text as T
import GHC.Generics (Generic)

import Data.Ergo.Transaction
import Ergvein.Crypto.Util
import Ergvein.Types.Orphanage ()
import Ergvein.Types.Transaction.Meta

import qualified Data.ByteString             as BS
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

type ErgTxRaw = Transaction

ergTxToString :: ErgTxRaw -> Text
ergTxToString _ = undefined

ergTxFromString :: Text -> Maybe ErgTxRaw
ergTxFromString _ = undefined

-- instance FromJSON ErgTxRaw where
--   parseJSON = withText "ErgTxRaw" $ \t ->
--     case ergTxFromString t of
--       Nothing -> fail "could not decode ERGO transaction"
--       Just x  -> return x

-- instance ToJSON ErgTxRaw where
--   toJSON = A.String . ergTxToString

data ErgTx = ErgTx { getErgTx :: !ErgTxRaw, getErgTxMeta :: !(Maybe EgvTxMeta)}
  deriving (Eq, Show, Read)

instance SafeCopy ErgTx where
  putCopy = undefined
  getCopy = undefined
  -- putCopy (ErgTx btx meta) = contain $ put btx >> safePut meta
  -- getCopy = contain $ ErgTx <$> get <*> safeGet

-- https://testnet.ergoplatform.com/en/transactions/7b6668423e5f1fa9a7c37006f43c0e82af5cc78bd5bad7a8e3e897787de246c3
mockErgoTx :: ErgTxRaw
mockErgoTx = Transaction [mockTxIn] [] [mockErgoBoxCandidate]

mockTxIn :: TxInput
mockTxIn = TxInput {
    boxId = Digest32 $ fromMaybe BS.empty $ decodeHex "db8c0e48a4b2806e2e2dea2fc40de048942a1b9e75e6681451f52011ae1121a7"
  , spendingProof = BS.empty
}

mockErgoBoxCandidate :: ErgoBoxCandidate
mockErgoBoxCandidate = ErgoBoxCandidate {
    ergoBoxCandidate'boxValue = 1000000
  , ergoBoxCandidate'ergoTree = fromMaybe BS.empty $ decodeHex "100204900108cd02c968d8d8c6eafb8f4682c6959a7b2073ae2dfae02eb20a3be82e4b497d5e6c53ea02d192a39a8cc7a70173007301"
  , ergoBoxCandidate'tokens = []
  , ergoBoxCandidate'additionalRegisters = BS.empty
  , ergoBoxCandidate'creationHeight = 7588
}
