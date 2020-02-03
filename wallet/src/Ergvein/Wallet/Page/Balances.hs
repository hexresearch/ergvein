module Ergvein.Wallet.Page.Balances(
    balancesPage
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.History
import Ergvein.Wallet.Wrapper

data BalanceTitle = BalanceTitle

instance LocalizedPrint BalanceTitle where
  localizedShow l v = case l of
    English -> case v of
      BalanceTitle  -> "Default wallet"
    Russian -> case v of
      BalanceTitle  -> "Настройки"

balancesPage :: MonadFront t m => m ()
balancesPage = do
  let thisWidget = Just $ pure balancesPage
  menuWidget BalanceTitle thisWidget
  wrapper False $ do
    syncWidget
    historyE <- currenciesList
    void $ nextWidget $ ffor historyE $ \cur -> Retractable {
        retractableNext = historyPage cur
      , retractablePrev = thisWidget
      }

syncWidget :: MonadFront t m => m ()
syncWidget = divClass "currency-wrapper" $ do
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

currenciesList :: MonadFront t m => m (Event t Currency)
currenciesList = fmap leftmost $ traverse currencyLine allCurrencies
  where
    currencyLine cur = do
      (e, _) <- divClass' "currency-wrapper" $ divClass "currency-line" $ do
        divClass "currency-name" $ text $ currencyName cur
        divClass "currency-balance" $ do
          bal <- currencyBalance cur
          dynText $ do
            m <- showMoney <$> bal
            pure $ m <> "〉"
      pure $ cur <$ domEvent Click e

currencyBalance :: MonadFront t m => Currency -> m (Dynamic t Money)
currencyBalance cur = pure $ pure $ Money cur 1
