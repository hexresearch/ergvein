{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

--------------------------------------------------------------------------
-- imports

import Network.Haskoin.Transaction
import Control.DeepSeq
import Control.Monad (replicateM)
import Control.Parallel.Strategies
import Test.QuickCheck
import Test.QuickCheck.Instances

import Test.Generators
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Serialize
import Ergvein.Index.Server.DB.Serialize.Tx
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.DB.Schema.Indexer
import Ergvein.Types.Currency

import qualified Data.Vector.Unboxed        as UV
import qualified Data.Vector                as V
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BB
import qualified Data.Attoparsec.ByteString as AP


--------------------------------------------------------------------------
-- Special case only for implemented messages

--------------------------------------------------------------------------
-- Serialize-deserialize

_prop_encdec_lastScannedBlockHeaderHashRec :: LastScannedBlockHeaderHashRec -> Bool
_prop_encdec_lastScannedBlockHeaderHashRec msg = either (const False) (msg ==) dec
  where
    enc = egvSerialize BTC msg
    dec = egvDeserialize BTC enc

_prop_encdec_ScannedHeightRec :: ScannedHeightRec -> Bool
_prop_encdec_ScannedHeightRec msg = either (const False) (msg ==) dec
  where dec = egvDeserialize BTC $ egvSerialize BTC msg
_prop_encdec_TxRec :: TxRec -> Bool
_prop_encdec_TxRec msg = either (const False) (msg ==) dec
  where dec = egvDeserialize BTC $ egvSerialize BTC msg
_prop_encdec_BlockMetaRec :: BlockMetaRec -> Bool
_prop_encdec_BlockMetaRec msg = either (const False) (msg ==) dec
  where dec = egvDeserialize BTC $ egvSerialize BTC msg
_prop_encdec_KnownPeerRecItem :: KnownPeerRecItem -> Bool
_prop_encdec_KnownPeerRecItem msg = either (const False) (msg ==) dec
  where dec = egvDeserialize BTC $ egvSerialize BTC msg
_prop_encdec_KnownPeersRec :: KnownPeersRec -> Bool
_prop_encdec_KnownPeersRec msg = either (const False) (msg ==) dec
  where dec = egvDeserialize BTC $ egvSerialize BTC msg
_prop_encdec_LastScannedBlockHeaderHashRec :: LastScannedBlockHeaderHashRec -> Bool
_prop_encdec_LastScannedBlockHeaderHashRec msg = either (const False) (msg ==) dec
  where dec = egvDeserialize BTC $ egvSerialize BTC msg
_prop_encdec_ContentHistoryRecItem :: ContentHistoryRecItem -> Bool
_prop_encdec_ContentHistoryRecItem msg = either (const False) (msg ==) dec
  where dec = egvDeserialize BTC $ egvSerialize BTC msg
_prop_encdec_ContentHistoryRec :: ContentHistoryRec -> Bool
_prop_encdec_ContentHistoryRec msg = either (const False) (msg ==) dec
  where dec = egvDeserialize BTC $ egvSerialize BTC msg
_prop_encdec_TxInfos :: [TxInfo] -> Bool
_prop_encdec_TxInfos infos = either (const False) (recs ==) dec
  where
    enc = parMap rdeepseq (serializeToTxRec BTC) (force infos)
    dec = sequence $ egvDeserialize BTC <$> enc
    recs = flip fmap infos $ \(TxInfo a b c) -> TxRec a b c
prop_encdec_Tx :: Tx -> Bool
prop_encdec_Tx msg = either (const False) (msg ==) dec
  where dec = egvDeserialize BTC $ egvSerialize BTC msg

prop_encdec_TxIn :: TxIn -> Bool
prop_encdec_TxIn msg = either (const False) (msg ==) dec
  where dec = egvDeserialize BTC $ egvSerialize BTC msg

prop_encdec_TxOut :: TxOut -> Bool
prop_encdec_TxOut msg = either (const False) (msg ==) dec
  where dec = egvDeserialize BTC $ egvSerialize BTC msg
--------------------------------------------------------------------------
-- main

return []
main = $quickCheckAll

--------------------------------------------------------------------------
-- the end.
