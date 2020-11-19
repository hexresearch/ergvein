module Ergvein.Wallet.Page.History(
    historyPage
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Derive
import Ergvein.Types.Utxo
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.History
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types
import Ergvein.Wallet.Node.BTC.Blocks
import Ergvein.Wallet.Node.BTC.Mempool
import Ergvein.Wallet.Page.Transaction
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Widget.Balance
import Ergvein.Wallet.Wrapper

import Data.Map.Strict as Map
import Data.Maybe
import Data.Text as T
import Data.Word
import Safe

import qualified Data.List as L

historyPage :: MonadFront t m => Currency -> m ()
historyPage cur = do
  walletName <- getWalletName
  title <- localized walletName
  let thisWidget = Just $ pure $ historyPage cur
      navbar = if isAndroid
        then navbarWidgetAndroid cur thisWidget
        else navbarWidget cur thisWidget NavbarHistory
  goE <- wrapperNavbar False title thisWidget navbar $ historyTableWidget cur
  void $ nextWidget $ ffor goE $ \tr -> Retractable {
      retractableNext = transactionInfoPage cur tr
    , retractablePrev = thisWidget
    }

historyTableWidget :: MonadFront t m => Currency -> m (Event t TransactionView)
historyTableWidget cur = divClass "history-table" $ case cur of
  BTC -> do
    (txsD, hghtD) <- transactionsGetting BTC
    let txMapD = Map.fromList . L.zip [(0 :: Int)..] <$> txsD
    mapED <- listWithKey txMapD (\_ -> historyTableRowD hghtD)
    let txClickE = switchDyn $ mergeMap <$> mapED
    pure $ fmapMaybe id $ headMay . Map.elems <$> txClickE
  ERGO -> do
    txClickE <- traverse historyTableRow []
    pure $ leftmost txClickE

historyTableRow :: MonadFront t m => TransactionView -> m (Event t TransactionView)
historyTableRow tr@TransactionView{..} = divButton "history-table-row" $ do
  moneyUnits <- fmap (fromMaybe defUnits . _settingsUnits) getSettings
  divClass ("history-amount-" <> ((T.toLower . showt) txInOut)) $ (symb txInOut) $ text $ showMoneyUnit txAmount moneyUnits
  divClass "history-date" $ showTxStatus tr
  divClass ("history-status-" <> ((T.toLower . showt) txInOut) <> " history-" <> confsClass) confsText
  pure tr
  where
    confs = txConfirmations txInfoView
    confsClass =
      if (confs == 0)
        then "unconfirmed"
      else if (confs > 0 && confs < confirmationGap)
        then "partially-confirmed"
      else "confirmed"
    confsText = if confs < confirmationGap
          then text $ showt confs <> "/" <> showt confirmationGap
          else spanClass "history-page-status-icon" $ elClass "i" "fas fa-check fa-fw" $ blank

historyTableRowD :: MonadFront t m => Dynamic t Word64 -> Dynamic t TransactionView -> m (Event t TransactionView)
historyTableRowD _ trD = fmap switchDyn $ widgetHoldDyn $ ffor trD $ \tr@TransactionView{..} -> divButton "history-table-row" $ do
    moneyUnits <- fmap (fromMaybe defUnits . _settingsUnits) getSettings
    divClass ("history-amount-" <> ((T.toLower . showt) txInOut)) $ symb txInOut $ text $ showMoneyUnit txAmount moneyUnits
    divClass "history-date" $ showTxStatus tr
    divClass ("history-status-" <> ((T.toLower . showt) txInOut) <> " history-" <> (confsClass tr)) $ confsText tr
    pure tr
  where
    confs tr = txConfirmations $ txInfoView tr
    unconfirmedParents tr = case txStatus tr of
      TransUncofirmedParents -> True
      _ -> False
    confsClass tr =
      if ((confs tr) == 0)
        then "unconfirmed"
      else if ((confs tr) > 0 && (confs tr) < confirmationGap)
        then "partially-confirmed"
      else "confirmed"
    confsText tr =
      if (confs tr)>= confirmationGap
        then spanClass "history-page-status-icon" $ elClass "i" "fas fa-check fa-fw" $ blank
      else if unconfirmedParents tr
        then do
          text $ showt (confs tr) <> "/" <> showt confirmationGap
          spanClass "history-page-status-text-icon" $ elClass "i" "fas fa-exclamation-triangle fa-fw" $ blank
      else text $ showt (confs tr) <> "/" <> showt confirmationGap

showTxStatus :: MonadFront t m => TransactionView -> m ()
showTxStatus tr@TransactionView{..} = case txStatus of
  TransUncofirmed -> localizedText HistoryUnconfirmed
  TransUncofirmedParents -> localizedText HistoryUnconfirmedParents
  TransConfirmed -> showTime tr
