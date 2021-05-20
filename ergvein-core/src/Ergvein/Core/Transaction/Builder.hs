{-# OPTIONS_GHC -Wall #-}

module Ergvein.Core.Transaction.Builder(
    buildTx
  , buildAddrTx
  , chooseCoins
) where

import Control.Monad.Identity (runIdentity)
import Data.Conduit (ConduitT, Void, runConduit, (.|), await)
import Data.Conduit.List (sourceList)
import Data.Maybe (fromMaybe)
import Data.Word
import Network.Haskoin.Util

import Ergvein.Core.Transaction.Btc
import Ergvein.Types.Utxo.Btc

{-
  Functions listed below are modificated functions from Network.Haskoin.Transaction.Builder module.
  https://hackage.haskell.org/package/haskoin-core-0.12.0/src/src/Network/Haskoin/Transaction/Builder.hs
  These functions have been modified to support the fee rate specified in satoshi per virtual byte.
-}

-- | Coin selection algorithm for transactions. This
-- function returns the selected coins together with the amount of change to
-- send back to yourself, taking the fee into account.
chooseCoins ::
     Coin c
  => Word64          -- ^ value to send
  -> Word64          -- ^ fee per vbyte
  -> [BtcOutputType] -- ^ list of output types (including change)
  -> Maybe [c]       -- ^ coins that should persist in the solution
  -> Bool            -- ^ try to find better solutions
  -> [c]             -- ^ list of ordered coins to choose from
  -> Either String ([c], Word64) -- ^ coin selection and change
chooseCoins target fee outTypes mFixedCoins continue coins =
  runIdentity . runConduit $
  sourceList coins .| chooseCoinsSink target fee outTypes mFixedCoins continue

-- | Coin selection algorithm for normal (non-multisig) transactions. This
-- function returns the selected coins together with the amount of change to
-- send back to yourself, taking the fee into account. This version uses a Sink
-- for conduit-based coin selection.
chooseCoinsSink ::
     (Monad m, Coin c)
  => Word64          -- ^ value to send
  -> Word64          -- ^ fee per vbyte
  -> [BtcOutputType] -- ^ list of output types (including change)
  -> Maybe [c]       -- ^ fixed coins that should persist in the solution
  -> Bool            -- ^ try to find better solution
  -> ConduitT c Void m (Either String ([c], Word64))
  -- ^ coin selection and change
chooseCoinsSink target fee outTypes mFixedCoins continue
  | target > 0 =
    maybeToEither err <$>
      greedyAddSink target ((guessTxFee fee outTypes) . (map coinType)) mFixedCoins continue
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
              -> Maybe [c]       -- ^ coins that should persist in the solution
              -> Bool            -- ^ try to find better solutions
              -> ConduitT c Void m (Maybe ([c], Word64))
              -- ^ coin selection and change
greedyAddSink target guessFee mFixedCoins continue =
    go initAcc initATot initPS initPTot
  where
    initAcc = fromMaybe [] mFixedCoins
    initATot = sum $ coinValue <$> initAcc
    initPS = if initATot >= goal initAcc then initAcc else []
    initPTot = if initATot >= goal initAcc then initATot else 0
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
