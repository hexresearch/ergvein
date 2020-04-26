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

newtype RecieveTitle = RecieveTitle Currency

instance LocalizedPrint RecieveTitle where
  localizedShow l (RecieveTitle c) = case l of
    English -> "Receive " <> currencyName c
    Russian -> "Получить " <> currencyName c

receivePage :: MonadFront t m => Currency -> m ()
receivePage cur = wrapper (RecieveTitle cur) (Just $ pure $ receivePage cur) $ do
  let thisWidget = Just $ pure $ receivePage cur
  navbarWidget cur thisWidget NavbarReceive
  void $ divClass "centered-wrapper" $ divClass "centered-content" $ h3 $ localizedText $ RecieveTitle cur
