{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

--------------------------------------------------------------------------
-- imports

import Control.DeepSeq
import Control.Monad (replicateM)
import Control.Parallel.Strategies
import Network.Haskoin.Transaction
import Test.QuickCheck
import Test.QuickCheck.Instances

import Test.Generators
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.DB.Schema.Indexer
import Ergvein.Index.Server.DB.Schema.Utxo
import Ergvein.Index.Server.DB.Serialize
import Ergvein.Types.Currency

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Vector                as V
import qualified Data.Vector.Unboxed        as UV


--------------------------------------------------------------------------
-- Special case only for implemented messages

--------------------------------------------------------------------------
-- Serialize-deserialize

prop_encdec_lastScannedBlockHeaderHashRec :: LastScannedBlockHeaderHashRec -> Bool
prop_encdec_lastScannedBlockHeaderHashRec msg = either (const False) (msg ==) dec
  where
    enc = egvSerialize BTC msg
    dec = egvDeserialize BTC enc

prop_encdec_ScannedHeightRec :: ScannedHeightRec -> Bool
prop_encdec_ScannedHeightRec msg = either (const False) (msg ==) dec
  where dec = egvDeserialize BTC $ egvSerialize BTC msg
prop_encdec_TxRecBytes :: TxRecBytes -> Bool
prop_encdec_TxRecBytes msg = either (const False) (msg ==) dec
  where dec = egvDeserialize BTC $ egvSerialize BTC msg
prop_encdec_TxRecHeight :: TxRecHeight -> Bool
prop_encdec_TxRecHeight msg = either (const False) (msg ==) dec
  where dec = egvDeserialize BTC $ egvSerialize BTC msg
prop_encdec_TxRecUnspent :: TxRecUnspent -> Bool
prop_encdec_TxRecUnspent msg = either (const False) (msg ==) dec
  where dec = egvDeserialize BTC $ egvSerialize BTC msg
prop_encdec_BlockMetaRec :: BlockInfoRec -> Bool
prop_encdec_BlockMetaRec msg = either (const False) (msg ==) dec
  where dec = egvDeserialize BTC $ egvSerialize BTC msg
prop_encdec_KnownPeerRecItem :: KnownPeerRecItem -> Bool
prop_encdec_KnownPeerRecItem msg = either (const False) (msg ==) dec
  where dec = egvDeserialize BTC $ egvSerialize BTC msg
prop_encdec_KnownPeersRec :: KnownPeersRec -> Bool
prop_encdec_KnownPeersRec msg = either (const False) (msg ==) dec
  where dec = egvDeserialize BTC $ egvSerialize BTC msg
prop_encdec_LastScannedBlockHeaderHashRec :: LastScannedBlockHeaderHashRec -> Bool
prop_encdec_LastScannedBlockHeaderHashRec msg = either (const False) (msg ==) dec
  where dec = egvDeserialize BTC $ egvSerialize BTC msg

prop_encdec_RollbackSequence :: RollbackSequence -> Bool
prop_encdec_RollbackSequence msg = either (const False) (msg ==) dec
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
