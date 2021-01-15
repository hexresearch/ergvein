{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Transaction.Fee(
    guessTxFee
  , guessTxVsize
  , chooseCoins
) where

import Control.Monad.Identity (runIdentity)
import Data.Conduit (ConduitT, Void, runConduit, (.|))
import Data.Conduit.List (sourceList)
import Data.Ratio
import Data.Serialize (encode)
import Data.Word
import Network.Haskoin.Network
import Network.Haskoin.Transaction (Coin(..), greedyAddSink)
import Network.Haskoin.Util

import qualified Data.ByteString as B

{-
  This module contains modificated functions from Network.Haskoin.Transaction.Builder module.
  https://hackage.haskell.org/package/haskoin-core-0.12.0/src/src/Network/Haskoin/Transaction/Builder.hs
  These functions have been modified to support the fee rate specified in satoshi per virtual byte.
-}

-- | Coin selection algorithm for normal (non-multisig) transactions. This
-- function returns the selected coins together with the amount of change to
-- send back to yourself, taking the fee into account.
chooseCoins ::
     Coin c
  => Word64 -- ^ value to send
  -> Word64 -- ^ fee per vbyte
  -> Int    -- ^ number of outputs (including change)
  -> Bool   -- ^ try to find better solutions
  -> [c]    -- ^ list of ordered coins to choose from
  -> Either String ([c], Word64) -- ^ coin selection and change
chooseCoins target fee nOut continue coins =
  runIdentity . runConduit $
  sourceList coins .| chooseCoinsSink target fee nOut continue

-- | Coin selection algorithm for normal (non-multisig) transactions. This
-- function returns the selected coins together with the amount of change to
-- send back to yourself, taking the fee into account. This version uses a Sink
-- for conduit-based coin selection.
chooseCoinsSink ::
     (Monad m, Coin c)
  => Word64 -- ^ value to send
  -> Word64 -- ^ fee per vbyte
  -> Int    -- ^ number of outputs (including change)
  -> Bool   -- ^ try to find better solution
  -> ConduitT c Void m (Either String ([c], Word64))
  -- ^ coin selection and change
chooseCoinsSink target fee nOut continue
  | target > 0 =
    maybeToEither err <$>
      greedyAddSink target (guessTxFee fee nOut) continue
  | otherwise = return $ Left "chooseCoins: Target must be > 0"
  where
  err = "chooseCoins: No solution found"

-- | Estimate tranasction fee to pay based on transaction virtual size estimation.
guessTxFee :: Word64 -> Int -> Int -> Word64
guessTxFee vbyteFee nOut nIn =
  vbyteFee * fromIntegral (guessTxVsize nIn nOut 0)

-- | Computes an upper bound on the virtual size of a transaction based on some known
-- properties of the transaction.
guessTxVsize ::
     Int -- ^ number of P2WPKH inputs
  -> Int -- ^ number of P2WPKH outputs
  -> Int -- ^ number of P2WSH outputs
  -> Int -- ^ upper bound on transaction virtual size in vbytes
guessTxVsize pki pkout sout =
  ceiling $ (4 * (8 + inpLen + inp + outLen + out) + 2 + witness) % 4
  where
    inpLen = B.length $ encode $ VarInt $ fromIntegral $ pki
    inp = pki * 41 -- (32: prev_out_hash, 4: prev_out_index, 1: script_length, 0: script, 4: sequence)
    outLen = B.length $ encode $ VarInt $ fromIntegral $ pkout + sout
    out =
      pkout * 31 + -- (8: out_value, 1: out_script_size, 22: script)
      sout * 34 -- (8: out_value, 1: out_script_size, 25: script)
    witness = pki * 108 -- (1: number_of_stack_items, 1: size_of_item_1, 72: item_1, 1: size_of_item_2, 33: item_2)
