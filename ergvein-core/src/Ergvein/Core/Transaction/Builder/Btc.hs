module Ergvein.Core.Transaction.Builder.Btc
  ( CoinSelectionError (..),
    TxCreationError (..),
    chooseCoins,
    buildTx,
    buildAddrTx,
  )
where

import Control.Monad.Identity (runIdentity)
import qualified Data.ByteString as B
import Data.Conduit (ConduitT, Void, await, runConduit, (.|))
import Data.Conduit.List (sourceList)
import Data.List ((\\))
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word64)
import Ergvein.Core.Transaction.Fee.Btc (BtcInputType, BtcOutputType, getDustThresholdByOutType, guessTxFee)
import Ergvein.Types.Network (Network)
import Ergvein.Types.Transaction (RbfEnabled)
import Ergvein.Types.Utxo.Btc (Coin (..))
import Network.Haskoin.Address (addressToOutput, stringToAddr)
import Network.Haskoin.Script (ScriptOutput (..), encodeOutputBS)
import Network.Haskoin.Transaction (OutPoint (..), Tx (..), TxIn (..), TxOut (..))
import Network.Haskoin.Util (maybeToEither)

data CoinSelectionError
  = InsufficientFunds
  | TargetMustBePositive
  deriving (Eq, Show)

{-
  Functions listed below are modificated functions from Network.Haskoin.Transaction.Builder module.
  https://hackage.haskell.org/package/haskoin-core-0.12.0/src/src/Network/Haskoin/Transaction/Builder.hs
  These functions have been modified to support the fee rate specified in satoshi per virtual byte.
-}

-- | When checking coins for a match with the target,
-- we assume that the coin may exceed the target by the tolerance value.
-- Specified in satoshi.
tolerance :: Word64
tolerance = 100

valueMatchesGoal :: Word64 -> Word64 -> Bool
valueMatchesGoal value goal = value >= goal && value <= goal + tolerance

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
  Either CoinSelectionError ([c], Maybe Word64)
chooseCoins target feeRate outTypes changeOutType mFixedCoins coins =
  case mFixedCoins of
    Nothing ->
      let
        coinMatchesTarget :: Coin c => c -> Bool
        coinMatchesTarget c =
          let goal = target + guessTxFee feeRate outTypes [coinType c]
              value = coinValue c
          in valueMatchesGoal value goal
        allCoinsLessThanTarget = filter (\coin -> coinValue coin < target) coins
        sumOfallCoinsLessThanTarget = sum $ coinValue <$> allCoinsLessThanTarget
      in
        case L.sortOn coinValue $ L.filter coinMatchesTarget coins of
          -- If any of coins matches the target, it will be used.
          (coin:xs) -> Right ([coin], Nothing)
          [] ->
            if
              let goal = target + guessTxFee feeRate outTypes (coinType <$> allCoinsLessThanTarget)
              in valueMatchesGoal sumOfallCoinsLessThanTarget goal
              then -- If the sum of all coins less than the target happens to match the target, they will be used.
                Right (allCoinsLessThanTarget, Nothing)
              else -- Otherwise, the FIFO approach is used.
                runIdentity . runConduit $
                  sourceList coins .| chooseCoinsSink target feeRate outTypes changeOutType Nothing
    Just fixedCoins ->
      let fixedCoinsTypes = coinType <$> fixedCoins
          goalWithoutChange = target + guessTxFee feeRate outTypes fixedCoinsTypes
          goalWithChange = target + guessTxFee feeRate (changeOutType : outTypes) fixedCoinsTypes
          fixedCoinsValue = sum $ coinValue <$> fixedCoins
       in if fixedCoinsValue >= goalWithoutChange
            then -- Fixed coins are enough to fund the transaction
              let mChange =
                    if fixedCoinsValue - goalWithChange >= fromIntegral (getDustThresholdByOutType changeOutType)
                      then Just $ fixedCoinsValue - goalWithChange
                      else Nothing
               in Right (fixedCoins, mChange)
            else -- Fixed coins are not enough to fund the transaction, trying to find additional coins
              let unfixedCoins = coins \\ fixedCoins
                  coinPlusFixedMatchesTarget :: Coin c => c -> Bool
                  coinPlusFixedMatchesTarget c =
                    let goal = target + guessTxFee feeRate outTypes (coinType c : fixedCoinsTypes)
                        value = coinValue c + fixedCoinsValue
                    in valueMatchesGoal value goal
                  allCoinsLessThanTarget = filter (\coin -> coinValue coin < target - fixedCoinsValue) unfixedCoins
                  sumOfallCoinsLessThanTargetMinusFixed = sum $ coinValue <$> allCoinsLessThanTarget
               in case L.find coinPlusFixedMatchesTarget unfixedCoins of
                    -- If any of coins plus fixed coins match the target, it will be used.
                    Just coin -> Right (coin : fixedCoins, Nothing)
                    Nothing ->
                      let
                        goal = target + guessTxFee feeRate outTypes ((coinType <$> allCoinsLessThanTarget) ++ fixedCoinsTypes)
                        value = sumOfallCoinsLessThanTargetMinusFixed + fixedCoinsValue
                      in
                        if valueMatchesGoal value goal
                          then -- If the sum of all coins smaller than the target plus fixed coins happen to match the target, they will be used.
                            Right (fixedCoins ++ allCoinsLessThanTarget, Nothing)
                          else -- Otherwise, the FIFO approach is used.
                            runIdentity . runConduit $
                              sourceList unfixedCoins .| chooseCoinsSink target feeRate outTypes changeOutType (Just fixedCoins)

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
  ConduitT c Void m (Either CoinSelectionError ([c], Maybe Word64))
chooseCoinsSink target feeRate outTypes changeOutType mFixedCoins
  | target > 0 =
    maybeToEither InsufficientFunds
      <$> greedyAddSink target (guessTxFee feeRate) outTypes changeOutType mFixedCoins
  | otherwise = return $ Left TargetMustBePositive

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
    goal coins mChangeOutType = target + guessFee (maybe outTypes (: outTypes) mChangeOutType) (coinType <$> coins)
    go acc aTot =
      await >>= \case
        -- A coin is available in the stream
        Just coin -> do
          let val = coinValue coin
          if val + aTot >= goal (coin : acc) Nothing
            then -- We have reached the goal using this coin
              let change = fromIntegral val + fromIntegral aTot - fromIntegral (goal (coin : acc) (Just changeOutType))
               in if change >= getDustThresholdByOutType changeOutType
                    then -- It makes sense to return the change
                      return $ Just (coin : acc, Just $ fromIntegral change)
                    else -- No change is needed
                      return $ Just (coin : acc, Nothing)
            else -- We have not yet reached the goal. Add the coin to the accumulator
              go (coin : acc) (val + aTot)
        -- We reached the end of the stream, no solution was found
        Nothing -> return Nothing

data TxCreationError
  = InvalidAddress Text
  | InvalidAmount Word64
  deriving (Eq, Show)

-- | Build a transaction by providing a list of outpoints as inputs
-- and a list of 'ScriptOutput' and amounts as outputs.
buildTx :: RbfEnabled -> [OutPoint] -> [(ScriptOutput, Word64)] -> Either TxCreationError Tx
buildTx rbfEnabled xs ys =
  mapM fo ys >>= \os -> return $ Tx 1 (map fi xs) os [] 0
  where
    fi outPoint = TxIn outPoint B.empty rbf
    rbf = if rbfEnabled then maxBound - 2 else maxBound
    fo (o, v)
      | v <= 2100000000000000 = return $ TxOut v $ encodeOutputBS o
      | otherwise = Left $ InvalidAmount v

-- | Build a transaction by providing a list of outpoints as inputs
-- and a list of recipient addresses and amounts as outputs.
buildAddrTx :: Network -> RbfEnabled -> [OutPoint] -> [(Text, Word64)] -> Either TxCreationError Tx
buildAddrTx net rbfEnabled xs ys = buildTx rbfEnabled xs =<< mapM f ys
  where
    f (s, v) =
      maybe (Left $ InvalidAddress s) Right $ do
        a <- stringToAddr net s
        let o = addressToOutput a
        return (o, v)
