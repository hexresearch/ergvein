module Ergvein.Wallet.Page.Recieve (
    recievePage
  ) where

import Ergvein.Types.Currency
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types
import Ergvein.Wallet.Wrapper

data CurrencyTitle = CurrencyTitle !Currency

instance LocalizedPrint CurrencyTitle where
  localizedShow l (CurrencyTitle c) = case l of
    English -> "Recieve " <> currencyName c
    Russian -> "Получение " <> currencyName c

recievePage :: MonadFront t m => Currency -> m ()
recievePage cur = do
  let thisWidget = Just $ pure $ recievePage cur
  menuWidget (CurrencyTitle cur) thisWidget
  navbarWidget cur thisWidget NavbarRecieve
  wrapper True $ do
    h3 $ localizedText $ CurrencyTitle cur
    pure ()
