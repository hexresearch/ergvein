module Ergvein.Types.Transaction.Btc (
      BtcTxId
    , btcTxHashToStr
    , btcTxHashFromStr
    , BtcTxRaw
    , btcTxToString
    , btcTxFromString
    , BtcTx(..)
  ) where

import Control.Monad ((<=<))
import Data.SafeCopy
import Data.Serialize (put, get)
import Data.Serialize as S
import Data.Text as T

import Ergvein.Crypto.Util
import Ergvein.Either
import Ergvein.Types.Orphanage ()
import Ergvein.Types.Transaction.Meta

import qualified Network.Haskoin.Transaction as HK

type BtcTxId = HK.TxHash

btcTxHashToStr :: BtcTxId -> Text
btcTxHashToStr = HK.txHashToHex

btcTxHashFromStr :: Text -> Maybe BtcTxId
btcTxHashFromStr = HK.hexToTxHash

type BtcTxRaw = HK.Tx

btcTxToString :: BtcTxRaw -> Text
btcTxToString = encodeHex . S.encode

btcTxFromString :: Text -> Maybe BtcTxRaw
btcTxFromString = eitherToMaybe . S.decode <=< decodeHex

data BtcTx = BtcTx { getBtcTx :: !BtcTxRaw, getBtcTxMeta :: !(Maybe EgvTxMeta) }
  deriving (Eq, Show, Read)

instance SafeCopy BtcTx where
  putCopy (BtcTx btx meta) = contain $ put btx >> safePut meta
  getCopy = contain $ BtcTx <$> get <*> safeGet

