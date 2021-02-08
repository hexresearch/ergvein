-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}

module Ergvein.Wallet.Page.Transaction(
    transactionInfoPage
  , showTime
  , symb
  ) where

import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import Data.Text as T
import Data.Time
import Data.Word

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Fee
import Ergvein.Wallet.Localization.History
import Ergvein.Wallet.Localization.Util
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Transaction.Util
import Ergvein.Wallet.Transaction.View
import Ergvein.Wallet.Widget.Input.BTC.Fee
import Ergvein.Wallet.Wrapper

import qualified Data.List as L
import qualified Network.Haskoin.Transaction as HT

transactionInfoPage :: MonadFront t m => Currency -> TransactionView -> m ()
transactionInfoPage cur tr@TransactionView{..} = do
  title <- localized HistoryTITitle
  moneyUnits <- fmap (fromMaybe defUnits . settingsUnits) getSettings
  let thisWidget = Just $ pure $ transactionInfoPage cur tr
  wrapper False title thisWidget $ divClass "tx-info-page" $ do
    infoPageElementExpEl HistoryTITransactionId $ hyperlink "link" (txId txInfoView) (txUrl txInfoView)
    infoPageElementEl HistoryTIAmount $ (symbCol txInOut) $ text $ showMoneyUnit txAmount moneyUnits <> " " <> symbolUnit cur moneyUnits
    infoPageElementEl HistoryTIWalletChanges $ (transTypeCol txInOut) $ text $ case txInOut of
      TransRefill -> (showMoneyUnit (Money BTC (maybe 0 moneyAmount txPrevAm)) moneyUnits) <> " -> " <> (showMoneyUnit (Money BTC ((maybe 0 moneyAmount txPrevAm) + (moneyAmount txAmount))) moneyUnits) <> " " <> symbolUnit cur moneyUnits
      TransWithdraw -> (showMoneyUnit (Money BTC (maybe 0 moneyAmount txPrevAm)) moneyUnits) <> " -> " <> (showMoneyUnit (Money BTC ((maybe 0 moneyAmount txPrevAm) - (moneyAmount txAmount) - (maybe 0 moneyAmount (txFee txInfoView)))) moneyUnits) <> " " <> symbolUnit cur moneyUnits
    case txInOut of
      TransRefill -> pure ()
      TransWithdraw -> infoPageElement HistoryTIFee $ maybe "unknown" (\a -> (showMoneyUnit a moneyUnits) <> " " <> symbolUnit cur moneyUnits) $ txFee txInfoView
    infoPageElementEl HistoryTIRbf $ do
      text $ showt $ txRbfEnabled txInfoView
      let bumpFeePossible = txInOut == TransWithdraw && txRbfEnabled txInfoView && txConfirmations txInfoView == 0
      when (bumpFeePossible) $ do
        bumpFeeE <- divClass "mt-1" $ outlineButton HistoryTIBumpFeeBtn
        void $ nextWidget $ ffor bumpFeeE $ \_ -> Retractable {
          retractableNext = bumpFeeWidget cur tr Nothing
        , retractablePrev = thisWidget
        }
      pure ()
    case txConflictingTxs txInfoView of
      [] -> pure ()
      conflictingTxs -> infoPageElementExpEl HistoryTIConflictingTxs $ do
        void $ traverse (makeNumberedTxIdLink cur) (L.zip [1..] conflictingTxs)
    case txReplacedTxs txInfoView of
      [] -> pure ()
      replacedTxs -> infoPageElementExpEl HistoryTIReplacedTxs $ do
        void $ traverse (makeNumberedTxIdLink cur) (L.zip [1..] replacedTxs)
    case txPossiblyReplacedTxs txInfoView of
      (_, []) -> pure ()
      (_, possiblyReplacedTxs) -> infoPageElementExpEl HistoryTIPossiblyReplacedTxs $ do
        void $ traverse (makeNumberedTxIdLink cur) (L.zip [1..] possiblyReplacedTxs)
    infoPageElementEl HistoryTITime $ showTime tr
    infoPageElement HistoryTIConfirmations $ showt $ txConfirmations txInfoView
    infoPageElementExpEl HistoryTIBlock $ maybe (text "unknown") (\(bllink,bl) -> hyperlink "link" bl bllink) $ txBlock txInfoView
    case txInOut of
      TransRefill -> pure ()
      TransWithdraw -> infoPageElementEl HistoryTIInputs $ divClass "tx-info-page-outputs-inputs" $ do
        void $ flip traverse (txInputs txInfoView) $ \(oAddress, oValue) -> do
          divClass "pr-1" $ localizedText HistoryTIOutputsValue
          divClass "" $ text $ showMoneyUnit oValue moneyUnits <> " " <> symbolUnit cur moneyUnits
          divClass "pr-1 mb-1" $ localizedText HistoryTIOutputsAddress
          divClass "tx-info-page-expanded mb-1" $ case oAddress of
            Nothing -> localizedText HistoryTIAddressUndefined
            Just address -> text address
        pure ()
    infoPageElementEl HistoryTIOutputs $ divClass "tx-info-page-outputs-inputs" $ do
      void $ flip traverse (txOutputs txInfoView) $ \(oAddress, oValue, oStatus, isOur) -> do
        divClass (oBld "pr-1" isOur) $ localizedText HistoryTIOutputsValue
        divClass (oBld "" isOur) $ text $ showMoneyUnit oValue moneyUnits <> " " <> symbolUnit cur moneyUnits
        if isOur
          then divClass (oBld "pr-1" isOur) $ localizedText HistoryTIOutputsOurAddress
          else divClass (oBld "pr-1" isOur) $ localizedText HistoryTIOutputsAddress
        divClass (oBld "tx-info-page-expanded" isOur) $ case oAddress of
          Nothing -> localizedText HistoryTIAddressUndefined
          Just address -> text address
        divClass (oBld "mb-1 pr-1" isOur) $ localizedText HistoryTIOutputsStatus
        divClass (oBld "mb-1" isOur) $ localizedText oStatus
      pure ()
      where
        oBld txt isOur = if isOur then (txt <> " tx-info-our-address") else txt

makeNumberedTxIdLink :: MonadFront t m => Currency -> (Int, TxId) -> m ()
makeNumberedTxIdLink _ (num, txId) = do
  settings <- getSettings
  let txIdText = egvTxHashToStr txId
      urlPrefixes = btcSettings'explorerUrls $ getBtcSettings settings
      urlPrefix = if isTestnet then testnetUrl urlPrefixes else mainnetUrl urlPrefixes
  text $ showt num <> ". "
  hyperlink "link" txIdText (urlPrefix <> "/tx/" <> txIdText)
  br

showTime :: MonadFront t m => TransactionView -> m ()
showTime TransactionView{..} = case txDate of
  TxTime Nothing -> localizedText $ if txStatus == TransUncofirmedParents then HistoryUnconfirmedParents else HistoryUnconfirmed
  TxTime (Just date) -> text $ T.pack $ formatTime defaultTimeLocale "%H:%M:%S %d/%m/%Y" $ date

infoPageElement :: MonadFront t m => HistoryPageStrings -> Text -> m ()
infoPageElement hps txt = divClass "tx-info-page-element" $ do
  par $ bold $ localizedText hps
  par $ text txt

infoPageElementEl :: MonadFront t m => HistoryPageStrings -> m () -> m ()
infoPageElementEl hps m = divClass "tx-info-page-element" $ do
  par $ bold $ localizedText hps
  m

infoPageElementExpEl :: MonadFront t m => HistoryPageStrings -> m () -> m ()
infoPageElementExpEl hps m = divClass "tx-info-page-element" $ do
  par $ bold $ localizedText hps
  parClass "tx-info-page-expanded" m

symb :: MonadFront t m => TransType -> m a -> m a
symb txInOut ma = case txInOut of
  TransRefill -> do
    spanClass "history-page-sign-icon" $ elClass "i" "fas fa-plus fa-fw" blank
    ma
  TransWithdraw -> do
    spanClass "history-page-sign-icon" $ elClass "i" "fas fa-minus fa-fw" blank
    ma

symbCol :: MonadFront t m => TransType -> m a -> m a
symbCol txInOut ma = divClass ("history-amount-" <> ((T.toLower . showt) txInOut)) $ do
  case txInOut of
    TransRefill -> do
      spanClass "history-page-sign-icon" $ elClass "i" "fas fa-plus fa-fw" blank
      ma
    TransWithdraw -> do
      spanClass "history-page-sign-icon" $ elClass "i" "fas fa-minus fa-fw" blank
      ma

transTypeCol :: MonadFront t m => TransType -> m a -> m a
transTypeCol txInOut ma = divClass ("history-amount-" <> ((T.toLower . showt) txInOut)) ma

bumpFeeWidget :: MonadFront t m => Currency -> TransactionView -> Maybe (BTCFeeMode, Word64) -> m ()
bumpFeeWidget cur tr@TransactionView{..} mInit = do
  title <- localized BumpFeeTitle
  let thisWidget = Just $ pure $ bumpFeeWidget cur tr mInit
  void $ wrapper False title thisWidget $ divClass "bump-fee-page" $ mdo
    elClass "h4" "mb-1" $ localizedText BumpFeeHeader
    workflow $ setNewFeeRate cur tr mInit

setNewFeeRate :: MonadFront t m => Currency -> TransactionView -> Maybe (BTCFeeMode, Word64) -> Workflow t m ()
setNewFeeRate cur TransactionView{..} mInit = Workflow $ mdo
  let feeRate = calcFeeRate (txFee txInfoView) (txRaw txInfoView)
  moneyUnits <- fmap (fromMaybe defUnits . settingsUnits) getSettings
  makeBlock BumpFeeCurrentFee $ maybe "unknown" (\a -> (showMoneyUnit a moneyUnits) <> " " <> symbolUnit cur moneyUnits) $ txFee txInfoView
  makeBlock BumpFeeCurrentFeeRate $ maybe "unknown" (\a -> (showf 3 $ (realToFrac a :: Double)) <> " " <> symbolUnit cur smallestUnits <> "/vbyte") feeRate
  feeD <- btcFeeSelectionWidget BumpFeeNewFeeRate mInit feeRate submitE
  submitE <- outlineButton CSSubmit
  let goE = attachWithMaybe (\mFee _ -> ((,) (txRaw txInfoView)) . snd <$> mFee) (current feeD) submitE
  pure ((), (uncurry makeRbfTx) <$> goE)

makeRbfTx :: MonadFront t m => EgvTx -> Word64 -> Workflow t m ()
makeRbfTx (TxBtc txToReplace) newFeeRate = Workflow $ do
  let inputs = HT.txIn $ getBtcTx txToReplace
      outputs = HT.txOut $ getBtcTx txToReplace
  mNewTx <- bumpFeeThroughCoinChooser inputs outputs newFeeRate
  -- newTx <- case mNewTx of
  --   Nothing -> bumpFeeThroughDecreasingOutputs
  --   Just tx -> pure tx
  pure ((), never)
makeRbfTx (TxErg txToReplace) newFeeRate = Workflow $ pure ((), never) -- TODO: implement for ERGO

-- method 1: keep all inputs, keep all not isMine outputs,
-- allow adding new inputs
bumpFeeThroughCoinChooser :: MonadFront t m => [HT.TxIn] -> [HT.TxOut] -> Word64 -> m (Maybe BtcTx)
bumpFeeThroughCoinChooser inputs outputs newFeeRate = do
  pubStorage <- getPubStorage
  let inputsToKeep = inputs
      allOurBtcAddrs = getAllBtcAddrs pubStorage

  -- which outputs to keep?
  -- not our change addresses, TODO: check if output is pointing to change address
  outputsToKeep <- filterM ((fmap not) . checkOutIsOursBtc allOurBtcAddrs) outputs
  logWrite $ "inputst to keep" <> showt inputsToKeep
  logWrite $ "outputs to keep" <> showt inputsToKeep
  pure Nothing


-- method 2: keep all inputs, no new inputs are added,
-- allow decreasing and removing outputs (change is decreased first)
-- This is less "safe" as it might end up decreasing e.g. a payment to a merchant;
-- but e.g. if the user has sent "Max" previously, this is the only way to RBF.
-- bumpFeeThroughDecreasingOutputs :: MonadFront t m => [HT.TxIn] -> [HT.TxOut] -> Word64 -> m BtcTx
-- bumpFeeThroughDecreasingOutputs inputs outputs newFeeRate = do

makeBlock :: (MonadFront t m, LocalizedPrint l) => l -> Text -> m ()
makeBlock a t = divClass "mb-1" $ do
  elClass "span" "font-bold" $ localizedText a
  br
  text t

smallestUnits :: Units
smallestUnits = Units {
    unitBTC  = Just BtcSat
  , unitERGO = Just ErgNano
  }

calcFeeRate :: Maybe Money -> EgvTx -> Maybe Rational
calcFeeRate (Just money) (TxBtc btcTx) =
  let txVsize = calcTxVsize $ getBtcTx btcTx
      fee = moneyToRationalUnit money smallestUnits
  in Just $ fee / (fromIntegral txVsize)
calcFeeRate (Just money) (TxErg ergTx) = Nothing -- TODO: implement for ERGO
calcFeeRate _ _ = Nothing

-- bumpFeeConfirmationWidget :: MonadFront t m => EgvTx -> Word64 -> m ()
-- bumpFeeConfirmationWidget (TxBtc tx) feeRate = do
--   title <- localized BumpFeeConfirmation
--   let thisWidget = Just $ pure $ bumpFeeConfirmationWidget (TxBtc tx) feeRate
--   wrapper False title thisWidget $ divClass "send-confirm-box" $ do
--     pure ()
-- bumpFeeConfirmationWidget (TxErg tx) feeRate = pure ()
