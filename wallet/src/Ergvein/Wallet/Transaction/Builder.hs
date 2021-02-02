-- {-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Transaction.Builder(
    guessTxFee
  , guessTxVsize
  , chooseCoins
  , BtcAddressType(..)
  , Coin(..)
) where

import Network.Haskoin.Network
import Control.Monad.Identity (runIdentity)
import Data.Conduit (ConduitT, Void, runConduit, (.|), await)
import Data.Conduit.List (sourceList)
import Data.Ratio
import Data.Serialize (encode)
import Data.Word
import Network.Haskoin.Util

import qualified Data.ByteString as B

{-
  This module contains modificated functions from Network.Haskoin.Transaction.Builder module.
  https://hackage.haskell.org/package/haskoin-core-0.12.0/src/src/Network/Haskoin/Transaction/Builder.hs
  These functions have been modified to support the fee rate specified in satoshi per virtual byte.
-}

class Coin c where
  coinValue :: c -> Word64
  coinType  :: c -> BtcAddressType

data BtcAddressType = P2PKH | P2SH | P2WPKHInP2SH
  | P2WSHInP2SH | P2WPKH | P2WSH | P2TR
  deriving (Read, Show, Eq)

type OutputType = BtcAddressType

type InputType = BtcAddressType

-- | Coin selection algorithm for transactions. This
-- function returns the selected coins together with the amount of change to
-- send back to yourself, taking the fee into account.
chooseCoins ::
     Coin c
  => Word64         -- ^ value to send
  -> Word64         -- ^ fee per vbyte
  -> [OutputType]   -- ^ list of output types (including change)
  -> Bool           -- ^ try to find better solutions
  -> [c]            -- ^ list of ordered coins to choose from
  -> Either String ([c], Word64) -- ^ coin selection and change
chooseCoins target fee outTypes continue coins =
  runIdentity . runConduit $
  sourceList coins .| chooseCoinsSink target fee outTypes continue

-- | Coin selection algorithm for normal (non-multisig) transactions. This
-- function returns the selected coins together with the amount of change to
-- send back to yourself, taking the fee into account. This version uses a Sink
-- for conduit-based coin selection.
chooseCoinsSink ::
     (Monad m, Coin c)
  => Word64         -- ^ value to send
  -> Word64         -- ^ fee per vbyte
  -> [OutputType]   -- ^ list of outputs types (including change)
  -> Bool           -- ^ try to find better solution
  -> ConduitT c Void m (Either String ([c], Word64))
  -- ^ coin selection and change
chooseCoinsSink target fee outTypes continue
  | target > 0 =
    maybeToEither err <$>
      greedyAddSink target ((guessTxFee fee outTypes) . (map coinType)) continue
  | otherwise = return $ Left "chooseCoins: Target must be > 0"
  where
  err = "chooseCoins: No solution found"

-- | Select coins greedily by starting from an empty solution. If the 'continue'
-- flag is set, the algorithm will try to find a better solution in the stream
-- after a solution is found. If the next solution found is not strictly better
-- than the previously found solution, the algorithm stops and returns the
-- previous solution. If the continue flag is not set, the algorithm will return
-- the first solution it finds in the stream.
greedyAddSink :: (Monad m, Coin c)
              => Word64          -- ^ value to send
              -> ([c] -> Word64) -- ^ coins to fee function
              -> Bool            -- ^ try to find better solutions
              -> ConduitT c Void m (Maybe ([c], Word64))
              -- ^ coin selection and change
greedyAddSink target guessFee continue =
    go [] 0 [] 0
  where
    -- The goal is the value we must reach (including the fee) for a certain
    -- amount of selected coins.
    goal c = target + guessFee c
    go acc aTot ps pTot = await >>= \case
        -- A coin is available in the stream
        Just coin -> do
            let val = coinValue coin
            -- We have reached the goal using this coin
            if val + aTot >= goal (coin:acc)
                -- If we want to continue searching for better solutions
                then if continue
                    -- This solution is the first one or
                    -- This solution is better than the previous one
                    then if pTot == 0 || val + aTot < pTot
                        -- Continue searching for better solutions in the stream
                        then go [] 0 (coin:acc) (val + aTot)
                        -- Otherwise, we stop here and return the previous
                        -- solution
                        else return $ Just (ps, pTot - goal ps)
                    -- Otherwise, return this solution
                    else return $
                        Just (coin : acc, val + aTot - goal (coin:acc))
                -- We have not yet reached the goal. Add the coin to the
                -- accumulator
                else go (coin:acc) (val + aTot) ps pTot
        -- We reached the end of the stream
        Nothing ->
            return $ if null ps
                -- If no solution was found, return Nothing
                then Nothing
                -- If we have a solution, return it
                else Just (ps, pTot - goal ps)

-- | Estimate tranasction fee to pay based on transaction virtual size estimation.
guessTxFee :: Word64 -> [OutputType] -> [InputType] -> Word64
guessTxFee vbyteFee outTypes inTypes =
  vbyteFee * fromIntegral (guessTxVsize inTypes outTypes)

-- | Returns input size in weight units including witness data
-- for P2SH, P2WSHInP2SH and P2WSH inputs we assume 1 pubkey and 1 signature
-- TODO: fix for more complex inputs
getInputWeight :: InputType -> Word64
getInputWeight = \case
  P2PKH -> 592
  P2SH -> 612
  P2WPKHInP2SH -> 364
  P2WSHInP2SH -> 412
  P2WPKH -> 271
  P2WSH -> 272
  P2TR -> 229

-- | Returns putput size in weight units
getOutputWeight :: OutputType -> Word64
getOutputWeight = \case
  P2PKH -> 136
  P2SH -> 128
  P2WPKHInP2SH -> 128
  P2WSHInP2SH -> 128
  P2WPKH -> 124
  P2WSH -> 172
  P2TR -> 172

-- | Computes an upper bound on the virtual size of a transaction based on
-- inputs and outputs sets of the transaction.
guessTxVsize ::
     [InputType]
  -> [OutputType]
  -> Int
guessTxVsize inputs outputs =
  ceiling $ (sum (getInputWeight <$> inputs) + sum (getOutputWeight <$> outputs) + overhead) % 4
  where
    varIntSize = B.length . encode . VarInt . fromIntegral
    segWitInputs = filter (\x -> x /= P2PKH && x /= P2SH) inputs
    witnessOverheadWeight = if null segWitInputs then 0 else 2 + varIntSize (length segWitInputs)
    overhead = fromIntegral $ (4 * (8 + varIntSize (length inputs) +  varIntSize (length outputs))) + witnessOverheadWeight
