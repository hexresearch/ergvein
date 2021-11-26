{-# OPTIONS_GHC -Wall #-}

module Ergvein.Core.Transaction.Builder.Btc
  ( chooseCoins,
    buildTx,
    buildAddrTx,
  )
where

import Control.Monad.Identity (runIdentity)
import Data.Conduit (ConduitT, Void, await, runConduit, (.|))
import Data.Conduit.List (sourceList)
import Data.List ((\\))
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Text (Text)
import Data.Word (Word64)
import Network.Haskoin.Address (addressToOutput, stringToAddr)
import Network.Haskoin.Script (ScriptOutput (..), encodeOutputBS)
import Network.Haskoin.Transaction (OutPoint (..), Tx (..), TxIn (..), TxOut (..))
import Network.Haskoin.Util (maybeToEither)

import Ergvein.Core.Transaction.Fee.Btc (BtcInputType, BtcOutputType, guessTxFee, getDustThresholdByOutType)
import Ergvein.Types.Network (Network)
import Ergvein.Types.Transaction (RbfEnabled)
import Ergvein.Types.Utxo.Btc (Coin (..))

import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.Text as T

{-
  Functions listed below are modificated functions from Network.Haskoin.Transaction.Builder module.
  https://hackage.haskell.org/package/haskoin-core-0.12.0/src/src/Network/Haskoin/Transaction/Builder.hs
  These functions have been modified to support the fee rate specified in satoshi per virtual byte.
-}

-- | Coin selection algorithm for transactions. This
-- function returns the selected coins together with the amount of change to
-- send back to yourself, taking the fee into account.
chooseCoins ::
  Coin c =>
  -- | value to send
  Word64 ->
  -- | fee per vbyte
  Word64 ->
  -- | list of output types (change not included)
  [BtcOutputType] ->
  -- | change output type
  BtcOutputType ->
  -- | coins that should persist in the solution.
  -- Note: fixed coins may or may not be included in the list of coins to choose from
  -- they will still be removed from the list later
  Maybe [c] ->
  -- | list of ordered coins to choose from
  [c] ->
  -- | coin selection and change
  Either String ([c], Maybe Word64)
chooseCoins target feeRate outTypes changeOutType mFixedCoins coins
  -- If any of coins matches the target it will be used.
  | isJust mCoinMatchingTarget = Right ([fromJust mCoinMatchingTarget], Nothing)
  -- If the sum of all coins smaller than the target happens to match the target, they will be used.
  | sumOfAllCoinsSmallerThanTarget == target + guessTxFee feeRate outTypes (coinType <$> allCoinsSmallerThanTarget) = Right (allCoinsSmallerThanTarget, Nothing)
  -- Otherwise, the FIFO approach is used.
  | otherwise =
    runIdentity . runConduit $
      sourceList (coins \\ fromMaybe [] mFixedCoins) .| chooseCoinsSink target feeRate outTypes changeOutType mFixedCoins
  where
    coinMatchesTarget :: Coin c => c -> Bool
    coinMatchesTarget c = coinValue c == target + guessTxFee feeRate outTypes [coinType c]
    mCoinMatchingTarget = L.find coinMatchesTarget coins
    allCoinsSmallerThanTarget = filter (\coin -> coinValue coin < target) coins
    sumOfAllCoinsSmallerThanTarget = sum $ coinValue <$> allCoinsSmallerThanTarget

-- | Coin selection algorithm for transactions. This
-- function returns the selected coins together with the amount of change to
-- send back to yourself, taking the fee into account. This version uses a Sink
-- for conduit-based coin selection.
chooseCoinsSink ::
  (Monad m, Coin c) =>
  -- | value to send
  Word64 ->
  -- | fee per vbyte
  Word64 ->
  -- | list of output types (change not included)
  [BtcOutputType] ->
  -- | change output type
  BtcOutputType ->
  -- | fixed coins that should persist in the solution
  Maybe [c] ->
  -- | coin selection and change
  ConduitT c Void m (Either String ([c], Maybe Word64))
chooseCoinsSink target feeRate outTypes changeOutType mFixedCoins
  | target > 0 =
    maybeToEither err
      <$> greedyAddSink target (guessTxFee feeRate) outTypes changeOutType mFixedCoins
  | otherwise = return $ Left "chooseCoins: Target must be > 0"
  where
    err = "chooseCoins: No solution found"

-- | Select coins greedily by starting from an empty solution. The algorithm will return
-- the first solution it finds in the stream.
greedyAddSink ::
  (Monad m, Coin c) =>
  -- | value to send
  Word64 ->
  -- | coins to fee function
  ([BtcOutputType] -> [BtcInputType] -> Word64) ->
  -- | list of output types (change not included)
  [BtcOutputType] ->
  -- | change output type
  BtcOutputType ->
  -- | coins that should persist in the solution
  Maybe [c] ->
  -- | coin selection and change
  ConduitT c Void m (Maybe ([c], Maybe Word64))
greedyAddSink target guessFee outTypes changeOutType mFixedCoins =
  go initAcc initATot
  where
    initAcc = fromMaybe [] mFixedCoins
    initATot = sum $ coinValue <$> initAcc
    -- The goal is the value we must reach (including the fee) for a certain
    -- amount of selected coins.
    goal :: Coin c => [c] -> Maybe BtcOutputType -> Word64
    goal coins mChangeOutType = target + guessFee (maybe outTypes (:outTypes) mChangeOutType) (coinType <$> coins)
    go acc aTot =
      await >>= \case
        -- A coin is available in the stream
        Just coin -> do
          let val = coinValue coin
          if val + aTot >= goal (coin : acc) Nothing
            -- We have reached the goal using this coin
            then
              let
                change = fromIntegral val + fromIntegral aTot - fromIntegral (goal (coin : acc) (Just changeOutType))
              in
                if change >= getDustThresholdByOutType changeOutType
                  -- It makes sense to return the change
                  then
                    return $ Just (coin : acc, Just $ fromIntegral change)
                  -- No change is needed
                  else
                    return $ Just (coin : acc, Nothing)
            -- We have not yet reached the goal. Add the coin to the accumulator
            else
              go (coin : acc) (val + aTot)
        -- We reached the end of the stream, no solution was found
        Nothing -> return Nothing

-- | Build a transaction by providing a list of outpoints as inputs
-- and a list of 'ScriptOutput' and amounts as outputs.
buildTx :: RbfEnabled -> [OutPoint] -> [(ScriptOutput, Word64)] -> Either String Tx
buildTx rbfEnabled xs ys =
  mapM fo ys >>= \os -> return $ Tx 1 (map fi xs) os [] 0
  where
    fi outPoint = TxIn outPoint B.empty rbf
    rbf = if rbfEnabled then maxBound - 2 else maxBound
    fo (o, v)
      | v <= 2100000000000000 = return $ TxOut v $ encodeOutputBS o
      | otherwise = Left $ "buildTx: Invalid amount " ++ show v

-- | Build a transaction by providing a list of outpoints as inputs
-- and a list of recipient addresses and amounts as outputs.
buildAddrTx :: Network -> RbfEnabled -> [OutPoint] -> [(Text, Word64)] -> Either String Tx
buildAddrTx net rbfEnabled xs ys = buildTx rbfEnabled xs =<< mapM f ys
  where
    f (s, v) =
      maybe (Left ("buildAddrTx: Invalid address " <> T.unpack s)) Right $ do
        a <- stringToAddr net s
        let o = addressToOutput a
        return (o, v)
