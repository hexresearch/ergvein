{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Page.TxInfo.Erg(
    txInfoPage
  , showTime
) where

import Data.Time

import Ergvein.Core.Transaction.View.Erg
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.TxInfo.Common
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Wrapper
import Sepulcas.Elements
import Sepulcas.Text (Display(..))

import qualified Data.List as L
import qualified Data.Text as T

txInfoPage :: MonadFront t m => TxView -> m ()
txInfoPage txView@TxView{..} = do
  let currency = ERGO
  title <- localized HistoryTITitle
  moneyUnits <- getSettingsUnitErg
  let thisWidget = Just $ pure $ txInfoPage txView
  wrapper False title thisWidget $ divClass "tx-info-page" $ do
    infoPageElementExpEl HistoryTITransactionId $ hyperlink "link" (txDetailedView'txId txView'detailedView) (txDetailedView'explorerUrl txView'detailedView)
    infoPageElementEl HistoryTIAmount $ symbCol txView'inOut $ text $ showMoneyUnit txView'amount moneyUnits <> " " <> display moneyUnits
    infoPageElementEl HistoryTIWalletChanges $ transTypeCol txView'inOut $ text $ case txView'inOut of
      TransRefill -> showMoneyUnit (Money currency (maybe 0 moneyAmount txView'prevAmount)) moneyUnits <> " -> " <> showMoneyUnit (Money currency (maybe 0 moneyAmount txView'prevAmount + moneyAmount txView'amount)) moneyUnits <> " " <> display moneyUnits
      TransWithdraw -> showMoneyUnit (Money currency (maybe 0 moneyAmount txView'prevAmount)) moneyUnits <> " -> " <> showMoneyUnit (Money currency (maybe 0 moneyAmount txView'prevAmount - moneyAmount txView'amount - maybe 0 moneyAmount (txDetailedView'fee txView'detailedView))) moneyUnits <> " " <> display moneyUnits
    case txView'inOut of
      TransRefill -> pure ()
      TransWithdraw -> infoPageElement HistoryTIFee $ maybe "unknown" (\a -> showMoneyUnit a moneyUnits <> " " <> display moneyUnits) $ txDetailedView'fee txView'detailedView
    case txDetailedView'conflictingTxs txView'detailedView of
      [] -> pure ()
      conflictingTxs -> infoPageElementExpEl HistoryTIConflictingTxs $ do
        traverse_ (makeNumberedTxIdLink currency) (L.zip [1..] conflictingTxs)
    case txDetailedView'replacedTxs txView'detailedView of
      [] -> pure ()
      replacedTxs -> infoPageElementExpEl HistoryTIReplacedTxs $ do
        traverse_ (makeNumberedTxIdLink currency) (L.zip [1..] replacedTxs)
    case txDetailedView'possiblyReplacedTxs txView'detailedView of
      (_, []) -> pure ()
      (_, possiblyReplacedTxs) -> infoPageElementExpEl HistoryTIPossiblyReplacedTxs $ do
        traverse_ (makeNumberedTxIdLink currency) (L.zip [1..] possiblyReplacedTxs)
    infoPageElementEl HistoryTITime $ showTime txView
    infoPageElement HistoryTIConfirmations $ showt $ txDetailedView'confirmations txView'detailedView
    infoPageElementExpEl HistoryTIBlock $ maybe (text "unknown") (\(bllink,bl) -> hyperlink "link" bl bllink) $ txDetailedView'block txView'detailedView
    case txView'inOut of
      TransRefill -> pure ()
      TransWithdraw -> infoPageElementEl HistoryTIInputs $ divClass "tx-info-page-outputs-inputs" $ do
        for_ (txDetailedView'inputs txView'detailedView) $ \(oAddress, oValue) -> do
          divClass "pr-1" $ localizedText HistoryTIOutputsValue
          divClass "" $ text $ showMoneyUnit oValue moneyUnits <> " " <> display moneyUnits
          divClass "pr-1 mb-1" $ localizedText HistoryTIOutputsAddress
          divClass "tx-info-page-expanded mb-1" $ case oAddress of
            Nothing -> localizedText HistoryTIAddressUndefined
            Just address -> text address
    infoPageElementEl HistoryTIOutputs $ divClass "tx-info-page-outputs-inputs" $ do
      for_ (txDetailedView'outputs txView'detailedView) $ \(oAddress, oValue, oStatus, isOur) -> do
        divClass (oBld "pr-1" isOur) $ localizedText HistoryTIOutputsValue
        divClass (oBld "" isOur) $ text $ showMoneyUnit oValue moneyUnits <> " " <> display moneyUnits
        if isOur
          then divClass (oBld "pr-1" isOur) $ localizedText HistoryTIOutputsOurAddress
          else divClass (oBld "pr-1" isOur) $ localizedText HistoryTIOutputsAddress
        divClass (oBld "tx-info-page-expanded" isOur) $ case oAddress of
          Nothing -> localizedText HistoryTIAddressUndefined
          Just address -> text address
        divClass (oBld "mb-1 pr-1" isOur) $ localizedText HistoryTIOutputsStatus
        divClass (oBld "mb-1" isOur) $ localizedText oStatus
      where
        oBld txt isOur = if isOur then txt <> " tx-info-our-address" else txt

showTime :: MonadFront t m => TxView -> m ()
showTime TxView{..} = case txView'time of
  TxTime Nothing -> localizedText $ if txView'confirmationStatus == TransUncofirmedParents then HistoryUnconfirmedParents else HistoryUnconfirmed
  TxTime (Just date) -> text $ T.pack $ formatTime defaultTimeLocale "%H:%M:%S %d/%m/%Y" date
