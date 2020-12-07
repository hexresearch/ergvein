module Ergvein.Types.Utxo.Btc
  ( BtcUtxoSet
  , BtcUtxoUpdate
  , BtcUtxoMeta(..)
  , isUtxoConfirmed
  , updateBtcUtxoSetPure
  , reconfirmBtcUtxoSetPure
  , confirmationGap
  ) where

import Data.List (foldl')
import Data.SafeCopy
import Data.Serialize
import Data.Word
import Network.Haskoin.Script
import Network.Haskoin.Transaction

import Ergvein.Aeson
import Ergvein.Types.Keys
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo.Status

import qualified Data.Map.Strict as M

data BtcUtxoMeta = BtcUtxoMeta {
  btcUtxo'index   :: !Int
, btcUtxo'purpose :: !KeyPurpose
, btcUtxo'amount  :: !Word64
, btcUtxo'script  :: !ScriptOutput
, btcUtxo'status  :: !EgvUtxoStatus
} deriving (Eq, Show, Read)

$(deriveJSON aesonOptionsStripToApostroph ''BtcUtxoMeta)

instance SafeCopy BtcUtxoMeta where
  version = 1
  putCopy BtcUtxoMeta{..} = contain $ do
    put btcUtxo'index
    put btcUtxo'purpose
    put btcUtxo'amount
    put $ encodeOutputBS btcUtxo'script
    safePut btcUtxo'status
  getCopy = contain $ do
    index <- get
    purpose <- get
    amount <- get
    escript <- fmap decodeOutputBS get
    status <- safeGet
    case escript of
      Left err -> fail $ "failed to decode output script! " <> show err
      Right script -> pure $ BtcUtxoMeta index purpose amount script status

type BtcUtxoSet = M.Map OutPoint BtcUtxoMeta

-- | fst -- wallet's unspent outputs
-- snd -- wallet's spent outputs
-- snd's bool: True - confirmed, must be deleted from UTXO set, False - set status to EUtxoSending
type BtcUtxoUpdate = (BtcUtxoSet, [(OutPoint, Bool)])

updateBtcUtxoSetPure :: BtcUtxoUpdate -> BtcUtxoSet -> BtcUtxoSet
updateBtcUtxoSetPure (outs, ins) s = foo (M.union outs s) ins $ \m (op, b) ->
  M.update (\meta -> if b then Nothing else Just meta {btcUtxo'status = EUtxoSending Nothing}) op m
  where foo b ta f = foldl' f b ta

confirmationGap :: Word64
confirmationGap = 3

staleGap :: Word64
staleGap = 2016     -- ~1 blk / 10 min. 14 days * 24 h * 6 blocks/h.

reconfirmBtcUtxoSetPure :: BlockHeight -> BtcUtxoSet -> BtcUtxoSet
reconfirmBtcUtxoSetPure bh bs = flip M.mapMaybe bs $ \meta -> case btcUtxo'status meta of
  EUtxoConfirmed -> Just $ meta
  EUtxoSemiConfirmed bh0 -> Just $ if bh - bh0 >= confirmationGap - 1 then meta {btcUtxo'status = EUtxoConfirmed} else meta
  EUtxoSending mh -> case mh of
    Nothing -> Just $ meta {btcUtxo'status = EUtxoSending $ Just bh}
    Just bh0 -> if bh - bh0 >= staleGap - 1 then Nothing else Just meta
  EUtxoReceiving mh -> case mh of
    Nothing -> Just $ meta {btcUtxo'status = EUtxoReceiving $ Just bh}
    Just bh0 -> if bh - bh0 >= staleGap - 1 then Nothing else Just meta
