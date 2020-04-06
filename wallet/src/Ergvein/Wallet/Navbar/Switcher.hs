module Ergvein.Wallet.Navbar.Switcher (
    switchNavbar
  ) where

import Ergvein.Types.Currency
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Navbar.Types

import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Page.History
import Ergvein.Wallet.Page.Receive
import Ergvein.Wallet.Page.Send

switchNavbar :: MonadFront t m => Currency -> Maybe (Dynamic t (m ())) -> Event t NavbarItem -> m ()
switchNavbar cur prevWidget e = void $ nextWidget $ fforMaybe e $ \go -> let
  mkNext n = Retractable {
    retractableNext = n
  , retractablePrev = Just $ pure balancesPage
  }
  in case go of
    NavbarSend    -> Just $ mkNext $ sendPage cur
    NavbarHistory -> Just $ mkNext $ historyPage cur
    NavbarReceive -> Just $ mkNext $ receivePage cur
