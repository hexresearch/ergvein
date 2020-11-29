{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Page.Transaction(
    transactionInfoPage
  , transactionsGetting
  , showTime
  , symb
  ) where

import Control.Monad.Reader
import Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text as T
import Data.Time

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.History
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Transaction.Get
import Ergvein.Wallet.Transaction.View
import Ergvein.Wallet.Wrapper

transactionInfoPage :: MonadFront t m => Currency -> TransactionView -> m ()
transactionInfoPage cur tr@TransactionView{..} = do
  title <- localized HistoryTITitle
  moneyUnits <- fmap (fromMaybe defUnits . _settingsUnits) getSettings
  wrapper False title (Just $ pure $ transactionInfoPage cur tr) $ divClass "tx-info-page" $ do
    infoPageElementExpEl HistoryTITransactionId $ hyperlink "link" (txId txInfoView) (txUrl txInfoView)
    infoPageElementEl HistoryTIAmount $ (symbCol txInOut) $ text $ showMoneyUnit txAmount moneyUnits <> " " <> symbolUnit cur moneyUnits
    infoPageElementEl HistoryTIWalletChanges $ (transTypeCol txInOut) $ text $ case txInOut of
      TransRefill -> (showMoneyUnit (Money BTC (maybe 0 moneyAmount txPrevAm)) moneyUnits) <> " -> " <> (showMoneyUnit (Money BTC ((maybe 0 moneyAmount txPrevAm) + (moneyAmount txAmount))) moneyUnits) <> " " <> symbolUnit cur moneyUnits
      TransWithdraw -> (showMoneyUnit (Money BTC (maybe 0 moneyAmount txPrevAm)) moneyUnits) <> " -> " <> (showMoneyUnit (Money BTC ((maybe 0 moneyAmount txPrevAm) - (moneyAmount txAmount) - (maybe 0 moneyAmount (txFee txInfoView)))) moneyUnits) <> " " <> symbolUnit cur moneyUnits
    case txInOut of
      TransRefill -> pure ()
      TransWithdraw -> infoPageElement HistoryTIFee $ maybe "unknown" (\a -> (showMoneyUnit a moneyUnits) <> " " <> symbolUnit cur moneyUnits) $ txFee txInfoView
    infoPageElement HistoryTIRbf $ showt $ txRbfEnabled txInfoView
    case txConflictingTxs of
      [] -> pure ()
      conflictingTxs -> infoPageElementExpEl HistoryTIConflictingTxs $ do
        void $ traverse (makeTxIdLink cur) conflictingTxs
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

makeTxIdLink :: MonadFront t m => Currency -> TxId -> m ()
makeTxIdLink cur txId = do
  settings <- getSettings
  let txIdText = egvTxHashToStr txId
      mExplorerPrefixes = Map.lookup cur $ _settingsExplorerUrl settings
      urlPrefixes = maybe btcDefaultExplorerUrls id mExplorerPrefixes
      urlPrefix = if isTestnet then testnetUrl urlPrefixes else mainnetUrl urlPrefixes
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
