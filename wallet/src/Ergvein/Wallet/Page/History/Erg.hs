{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Page.History.Erg (
  historyPage
) where

import Data.Text (Text)
import Data.Word
import Safe

import Ergvein.Core.Transaction.Get.Erg
import Ergvein.Core.Transaction.View.Erg
import Ergvein.Types.Utxo.Ergo
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types
import Ergvein.Wallet.Page.History.Common
import Ergvein.Wallet.Page.TxInfo.Common
import Ergvein.Wallet.Page.TxInfo.Erg
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Wrapper
import Sepulcas.Elements

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T

historyPage :: MonadFront t m => m ()
historyPage = do
  walletName <- getWalletName
  title <- localized walletName
  let thisWidget = Just $ pure historyPage
      navbar = if isAndroid
        then navbarWidgetAndroid ERGO thisWidget
        else navbarWidget ERGO thisWidget NavbarHistory
  goE <- wrapperGeneric False title thisWidget (Just navbar) "history-page" historyTableWidget
  void $ nextWidget $ ffor goE $ \tr -> Retractable {
      retractableNext = txInfoPage tr
    , retractablePrev = thisWidget
    }

historyTableWidget :: MonadFront t m => m (Event t TxView)
historyTableWidget = do
  (txsD, hghtD) <- getTransactionsErgMock
  let txMapD = M.fromList . L.zip [(0 :: Int)..] <$> txsD
  resD <- networkHoldDyn $ ffor txMapD $ \txMap -> if M.null txMap
    then do
      noTxsPlaceholder
      pure never
    else do
      mapED <- divClass "history-table" $ listWithKey txMapD (\_ -> historyTableRowD hghtD)
      let txClickE = switchDyn $ mergeMap <$> mapED
      pure $ fmapMaybe id $ headMay . M.elems <$> txClickE
  pure $ switchDyn resD

historyTableRowD :: MonadFront t m => Dynamic t Word64 -> Dynamic t TxView -> m (Event t TxView)
historyTableRowD _ trD = fmap switchDyn $ networkHoldDyn $ ffor trD $ \txView@TxView{..} -> divButton "history-table-row" $ do
  moneyUnits <- getSettingsUnitBtc
  let txAmountPlusFee = moneyFromRational ERGO (moneyToRational txView'amount + maybe 0 moneyToRational (txDetailedView'fee txView'detailedView))
      fullAmount = case txView'inOut of
        TransWithdraw -> symb TransWithdraw $ text $ showMoneyUnit txAmountPlusFee moneyUnits
        TransRefill -> symb TransRefill $ text $ showMoneyUnit txView'amount moneyUnits
  divClass ("history-amount-" <> (T.toLower . showt) txView'inOut) fullAmount
  divClass "history-date" $ showTxStatus txView
  divClass ("history-status-" <> (T.toLower . showt) txView'inOut <> " history-" <> confsClass txView) $ confsText txView
  pure txView
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
      | confs tr  >= confirmationGap = spanClass "history-page-status-icon" $ elClass "i" "fas fa-check fa-fw" blank
      | unconfirmedParents tr = do
          text $ showt (confs tr) <> "/" <> showt confirmationGap
          spanClass "history-page-status-text-icon" $ elClass "i" "fas fa-exclamation-triangle fa-fw" blank
      | otherwise = text $ showt (confs tr) <> "/" <> showt confirmationGap

showTxStatus :: MonadFront t m => TxView -> m ()
showTxStatus txView@TxView{..} = case txView'confirmationStatus of
  TransConfirmed -> showTime txView
  TransUncofirmed -> showStatus HistoryUnconfirmed
  TransUncofirmedParents -> showStatus HistoryUnconfirmedParents
  where
    conflictingTxs = not $ L.null $ txDetailedView'conflictingTxs txView'detailedView
    txPossiblyReplacedTxsWrapper = txDetailedView'possiblyReplacedTxs txView'detailedView
    possibleReplacedTxs = not $ L.null $ snd txPossiblyReplacedTxsWrapper
    possiblyReplacing = fst txPossiblyReplacedTxsWrapper

    showStatus :: (MonadFront t m, LocalizedPrint l) => l -> m ()
    showStatus status = do
      localizedText status
      when conflictingTxs (text " " >> badge "badge-danger" ("double spend" :: Text))
      when possibleReplacedTxs (text " " >> badge "badge-warning" (if possiblyReplacing
        then "possibly replacing" :: Text
        else "possibly replaced" :: Text))
