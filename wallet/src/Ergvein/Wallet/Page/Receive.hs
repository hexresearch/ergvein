module Ergvein.Wallet.Page.Receive (
    receivePage
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
    English -> "Receive " <> currencyName c
    Russian -> "Получение " <> currencyName c

receivePage :: MonadFront t m => Currency -> m ()
receivePage cur = do
  let thisWidget = Just $ pure $ receivePage cur
  menuWidget (CurrencyTitle cur) thisWidget
  navbarWidget cur thisWidget NavbarReceive
  wrapper True $ do
    h3 $ localizedText $ CurrencyTitle cur
    pure ()
