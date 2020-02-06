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
import Ergvein.Wallet.Sync.Widget
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
    syncWidget =<< getSyncProgress
    historyE <- currenciesList
    void $ nextWidget $ ffor historyE $ \cur -> Retractable {
        retractableNext = historyPage cur
      , retractablePrev = thisWidget
      }

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
