module Ergvein.Wallet.Page.History(
    historyPage
  ) where

import Ergvein.Types.Utxo.Btc
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types
import Ergvein.Wallet.Page.Transaction
import Ergvein.Wallet.Wrapper
import Sepulcas.Elements

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
  goE <- wrapperGeneric False title thisWidget (Just navbar) "history-page" $ historyTableWidget cur
  void $ nextWidget $ ffor goE $ \tr -> Retractable {
      retractableNext = transactionInfoPage cur tr
    , retractablePrev = thisWidget
    }

historyTableWidget :: MonadFront t m => Currency -> m (Event t TransactionView)
historyTableWidget cur = case cur of
  BTC -> do
    (txsD, hghtD) <- transactionsGetting BTC
    let txMapD = Map.fromList . L.zip [(0 :: Int)..] <$> txsD
    resD <- networkHoldDyn $ ffor txMapD $ \txMap -> if Map.null txMap
      then do
        noTxsPlaceholder
        pure never
      else do
        mapED <- divClass "history-table" $ listWithKey txMapD (\_ -> historyTableRowD BTC hghtD)
        let txClickE = switchDyn $ mergeMap <$> mapED
        pure $ fmapMaybe id $ headMay . Map.elems <$> txClickE
    pure $ switchDyn resD
  ERGO -> do
    txClickE <- divClass "history-table" $ traverse (historyTableRow ERGO) []
    pure $ leftmost txClickE

noTxsPlaceholder :: MonadFront t m => m ()
noTxsPlaceholder = divClass "history-empty-placeholder text-muted" $ do
  par $ localizedText HistoryNoTxs

historyTableRow :: MonadFront t m => Currency -> TransactionView -> m (Event t TransactionView)
historyTableRow cur tr@TransactionView{..} = divButton "history-table-row" $ do
  moneyUnits <- fmap (fromMaybe defUnits . settingsUnits) getSettings
  let txAmountPlusFee = moneyFromRational cur (moneyToRational txAmount + fromMaybe 0 (moneyToRational <$> txFee txInfoView))
      fullAmount = case txInOut of
        TransWithdraw -> symb TransWithdraw $ text $ showMoneyUnit txAmountPlusFee moneyUnits
        TransRefill -> symb TransRefill $ text $ showMoneyUnit txAmount moneyUnits
  divClass ("history-amount-" <> ((T.toLower . showt) txInOut)) (fullAmount)
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

historyTableRowD :: MonadFront t m => Currency -> Dynamic t Word64 -> Dynamic t TransactionView -> m (Event t TransactionView)
historyTableRowD cur _ trD = fmap switchDyn $ networkHoldDyn $ ffor trD $ \tr@TransactionView{..} -> divButton "history-table-row" $ do
    moneyUnits <- fmap (fromMaybe defUnits . settingsUnits) getSettings
    let txAmountPlusFee = moneyFromRational cur (moneyToRational txAmount + fromMaybe 0 (moneyToRational <$> txFee txInfoView))
        fullAmount = case txInOut of
          TransWithdraw -> symb TransWithdraw $ text $ showMoneyUnit txAmountPlusFee moneyUnits
          TransRefill -> symb TransRefill $ text $ showMoneyUnit txAmount moneyUnits
    divClass ("history-amount-" <> ((T.toLower . showt) txInOut)) fullAmount
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
      if (confs tr)  >= confirmationGap
        then spanClass "history-page-status-icon" $ elClass "i" "fas fa-check fa-fw" $ blank
      else if unconfirmedParents tr
        then do
          text $ showt (confs tr) <> "/" <> showt confirmationGap
          spanClass "history-page-status-text-icon" $ elClass "i" "fas fa-exclamation-triangle fa-fw" $ blank
      else text $ showt (confs tr) <> "/" <> showt confirmationGap

showTxStatus :: MonadFront t m => TransactionView -> m ()
showTxStatus tr@TransactionView{..} = case txStatus of
  TransConfirmed -> showTime tr
  TransUncofirmed -> showStatus HistoryUnconfirmed
  TransUncofirmedParents -> showStatus HistoryUnconfirmedParents
  where
    rbfEnabled = txRbfEnabled txInfoView
    conflictingTxs = not $ L.null $ txConflictingTxs txInfoView
    txPossiblyReplacedTxsWrapper = txPossiblyReplacedTxs txInfoView
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
