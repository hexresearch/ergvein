{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Page.Balances(
    balancesPage
  ) where

import Data.Maybe (fromMaybe)

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Settings
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.History
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Page.PatternKey
import Ergvein.Wallet.Page.Send
import Ergvein.Wallet.Wrapper

import Control.Monad.IO.Class
import Data.Map.Strict as Map

data BalanceTitle = BalanceTitle

instance LocalizedPrint BalanceTitle where
  localizedShow l v = case l of
    English -> case v of
      BalanceTitle  -> "Default wallet"
    Russian -> case v of
      BalanceTitle  -> "Стандартный кошелек"

data ButtonSend = ButtonSend

instance LocalizedPrint ButtonSend where
  localizedShow l _ = case l of
    English ->  "Send"
    Russian ->  "Отправить"

data ButtonRecieve = ButtonRecieve

instance LocalizedPrint ButtonRecieve where
  localizedShow l _ = case l of
    English -> "Recieve"
    Russian -> "Получить"

balancesPage :: MonadFront t m => m ()
balancesPage = do
  anon_name <- getWalletName
#ifdef ANDROID
  c <- liftIO $ loadCounter
  liftIO $ saveCounter $ PatternTries $ Map.insert anon_name 0 (patterntriesCount c)
#endif
  let thisWidget = Just $ pure balancesPage
  menuWidget BalanceTitle thisWidget
  wrapper False $ divClass "balances-wrapper" $ do
    syncWidget
    currenciesList

syncWidget :: MonadFront t m => m ()
syncWidget = do
  progressD <- getSyncProgress
  void $ widgetHoldDyn $ ffor progressD $ \sp -> case sp of
    ScanSynced -> pure ()
    _ -> divClass "sync-progress" $ localizedText sp

data ScanProgress = ScanDays !Int | ScanHours !Int | ScanSynced

instance LocalizedPrint ScanProgress where
  localizedShow l v = case l of
    English -> case v of
      ScanDays d  -> showt d <> if d == 1 then " day behind..." else " days behind..."
      ScanHours h -> showt h <> if h == 1 then " hour behind..." else " hours behind..."
      ScanSynced  -> "Fully synced!"
    Russian -> case v of
      ScanDays d  -> "Отстаём на " <> showt d <> case (d `mod` 10) of
        1 -> " день..."
        2 -> " дня..."
        3 -> " дня..."
        _ -> " дней..."
      ScanHours h -> "Отстаём на " <> showt h <> case (h `mod` 10) of
        1 -> " час..."
        2 -> " часа..."
        3 -> " часа..."
        _ -> " часов..."
      ScanSynced  -> "Синхронизировано!"

-- | Return current amount of days remaining to scan
getSyncProgress :: MonadFront t m => m (Dynamic t ScanProgress)
getSyncProgress = pure $ pure $ ScanDays 10

currenciesList :: MonadFront t m => m ()
currenciesList = divClass "currency-content" $ do
  historyE <- leftmost <$> traverse currencyLine allCurrencies
  let thisWidget = Just $ pure balancesPage
  void $ nextWidget $ ffor historyE $ \cur -> Retractable {
    retractableNext = historyPage cur
  , retractablePrev = thisWidget
  }
  where
    currencyLine cur = do
      (e, _) <- divClass' "currency-row" $ do
        bal <- currencyBalance cur
        settings <- getSettings
        let setUs = getSettingsUnits settings
        langD <- getLanguage
        divClass "currency-name"    $ text $ currencyName cur
        divClass "currency-balance" $ do
          elClass "span" "currency-value" $ dynText $ (\v -> showMoneyUnit v setUs) <$> bal
          elClass "span" "currency-unit"  $ dynText $ ffor2 bal langD $ \(Money cur _) lang -> symbolUnit cur lang setUs
          elClass "span" "currency-arrow" $ text "〉"
      pure $ cur <$ domEvent Click e
    getSettingsUnits = fromMaybe defUnits . settingsUnits

currencyBalance :: MonadFront t m => Currency -> m (Dynamic t Money)
currencyBalance cur = pure $ pure $ Money cur 1

symbolUnit :: Currency -> Language -> Units -> Text
symbolUnit cur lang units =
  case cur of
    BTC  -> case getUnitBTC units of
              BtcWhole    -> "btc"
              BtcMilli    -> "mbtc"
              BtcSat      -> "sat"
    ERGO -> case getUnitERGO units of
              ErgWhole    -> "erg"
              ErgMilli    -> "merg"
              ErgNano     -> "sat"

