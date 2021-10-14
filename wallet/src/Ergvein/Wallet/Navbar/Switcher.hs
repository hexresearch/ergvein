module Ergvein.Wallet.Navbar.Switcher (
    switchNavbar
  ) where

import Ergvein.Types.Currency
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Navbar.Types

import Ergvein.Wallet.Page.Send.Btc
import Ergvein.Wallet.Page.History
import Ergvein.Wallet.Page.Receive

switchNavbar :: MonadFront t m => Currency -> Maybe (Dynamic t (m ())) -> Event t NavbarItem -> m ()
switchNavbar cur prevWidget e = void $ nextWidget $ fforMaybe e $ \go -> let
  mkNext n = Retractable {
    retractableNext = n
  , retractablePrev = prevWidget
  }
  sendPage = case cur of
    BTC -> sendPageBtc Nothing
  in case go of
    NavbarSend    -> Just $ mkNext sendPage
    NavbarHistory -> Just $ mkNext $ historyPage cur
    NavbarReceive -> Just $ mkNext $ receivePage cur
