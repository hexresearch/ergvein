{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Page.History.Btc (
  historyPage
) where

import Ergvein.Core.Transaction.Get.Btc
import Ergvein.Core.Transaction.View.Btc
import Ergvein.Types.Utxo.Btc
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types
import Ergvein.Wallet.Page.History.Common ( noTxsPlaceholder )
import Ergvein.Wallet.Page.Seed
import Ergvein.Wallet.Page.TxInfo.Btc
import Ergvein.Wallet.Page.TxInfo.Common
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Wrapper
import Sepulcas.Elements

import Data.Map.Strict as Map
import Data.Text as T
import Data.Word
import Safe

import qualified Data.List as L

historyPage :: MonadFront t m => m ()
historyPage = do
  walletName <- getWalletName
  title <- localized walletName
  let thisWidget = Just $ pure historyPage
      navbar = if isAndroid
        then navbarWidgetAndroid BTC thisWidget
        else navbarWidget BTC thisWidget NavbarHistory
  goE <- wrapperGeneric False title thisWidget (Just navbar) "history-page" historyTableWidget
  void $ nextWidget $ ffor goE $ \tr -> Retractable {
      retractableNext = txInfoPage tr
    , retractablePrev = thisWidget
    }

historyTableWidget :: MonadFront t m => m (Event t TxView)
historyTableWidget = do
  backupButton
  (txsD, hghtD) <- getTransactionsBtc
  let txMapD = Map.fromList . L.zip [(0 :: Int)..] <$> txsD
  resD <- networkHoldDyn $ ffor txMapD $ \txMap -> if Map.null txMap
    then do
      noTxsPlaceholder
      pure never
    else do
      mapED <- divClass "history-table" $ listWithKey txMapD (\_ -> historyTableRowD hghtD)
      let txClickE = switchDyn $ mergeMap <$> mapED
      pure $ fmapMaybe id $ headMay . Map.elems <$> txClickE
  pure $ switchDyn resD

backupButton :: MonadFront t m => m ()
backupButton = do
  seedBackupRequiredD <- (holdUniqDyn . fmap _pubStorage'seedBackupRequired) =<< getPubStorageD
  void $ networkHoldDyn $ ffor seedBackupRequiredD $ \case
    False -> pure ()
    True -> do
      backupE <- buttonClass "seed-backup-btn" HistorySeedBackupRequired
      mnemonicE <- withWallet $ ffor backupE $ \_ prvStorage -> pure $ _prvStorage'mnemonic prvStorage
      void $ nextWidget $ ffor mnemonicE $ \mnemonic -> Retractable {
          retractableNext = mnemonicPage $ Just mnemonic
        , retractablePrev = Just $ pure historyPage
        }

historyTableRowD :: MonadFront t m => Dynamic t Word64 -> Dynamic t TxView -> m (Event t TxView)
historyTableRowD _ trD = fmap switchDyn $ networkHoldDyn $ ffor trD $ \tr@TxView{..} -> divButton "history-table-row" $ do
  moneyUnits <- getSettingsUnitBtc
  let txAmountPlusFee = moneyFromRational BTC (moneyToRational txView'amount + maybe 0 moneyToRational (txDetailedView'fee txView'detailedView))
      fullAmount = case txView'inOut of
        TransWithdraw -> symb TransWithdraw $ text $ showMoneyUnit txAmountPlusFee moneyUnits
        TransRefill -> symb TransRefill $ text $ showMoneyUnit txView'amount moneyUnits
  divClass ("history-amount-" <> (T.toLower . showt) txView'inOut) fullAmount
  divClass "history-date" $ showTxStatus tr
  divClass ("history-status-" <> (T.toLower . showt) txView'inOut <> " history-" <> confsClass tr) $ confsText tr
  pure tr
  where
    confs tr = txDetailedView'confirmations $ txView'detailedView tr
    unconfirmedParents tr = case txView'confirmationStatus tr of
      TransUncofirmedParents -> True
      _ -> False
    confsClass tr
      | confs tr == 0 = "unconfirmed"
      | confs tr > 0 && confs tr < confirmationGap = "partially-confirmed"
      | otherwise = "confirmed"

    confsText tr
      | confs tr >= confirmationGap = spanClass "history-page-status-icon" $ elClass "i" "fas fa-check fa-fw" blank
      | unconfirmedParents tr = do
          text $ showt (confs tr) <> "/" <> showt confirmationGap
          spanClass "history-page-status-text-icon" $ elClass "i" "fas fa-exclamation-triangle fa-fw" blank
      | otherwise = text $ showt (confs tr) <> "/" <> showt confirmationGap

showTxStatus :: MonadFront t m => TxView -> m ()
showTxStatus tr@TxView{..} = case txView'confirmationStatus of
  TransConfirmed -> showTime tr
  TransUncofirmed -> showStatus HistoryUnconfirmed
  TransUncofirmedParents -> showStatus HistoryUnconfirmedParents
  where
    rbfEnabled = txDetailedView'rbfEnabled txView'detailedView
    conflictingTxs = not $ L.null $ txDetailedView'conflictingTxs txView'detailedView
    txPossiblyReplacedTxsWrapper = txDetailedView'possiblyReplacedTxs txView'detailedView
    possibleReplacedTxs = not $ L.null $ snd txPossiblyReplacedTxsWrapper
    possiblyReplacing = fst txPossiblyReplacedTxsWrapper

    showStatus :: (MonadFront t m, LocalizedPrint l) => l -> m ()
    showStatus status = do
      localizedText status
      when rbfEnabled (text " " >> badge "badge-info-2" ("rbf" :: Text)) -- text " " is needed for proper word wrapping
      when conflictingTxs (text " " >> badge "badge-danger" ("double spend" :: Text))
      when possibleReplacedTxs (text " " >> badge "badge-warning" (if possiblyReplacing
        then "possibly replacing" :: Text
        else "possibly replaced" :: Text))
