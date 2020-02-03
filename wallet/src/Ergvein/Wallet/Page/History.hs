module Ergvein.Wallet.Page.History(
    historyPage
  ) where

import Ergvein.Types.Currency
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Wrapper

data CurrencyTitle = CurrencyTitle !Currency

instance LocalizedPrint CurrencyTitle where
  localizedShow l v = case l of
    English -> case v of
      CurrencyTitle c -> "History " <> currencyName c
    Russian -> case v of
      CurrencyTitle c -> "История " <> currencyName c

historyPage :: MonadFront t m => Currency -> m ()
historyPage cur = do
  let thisWidget = Just $ pure $ historyPage cur
  menuWidget (CurrencyTitle cur) thisWidget
  wrapper True $ do
    h3 $ localizedText $ CurrencyTitle cur
    pure ()
