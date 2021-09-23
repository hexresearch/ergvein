module Ergvein.Wallet.Menu.Switcher(
    switchMenu
  ) where

import Ergvein.Wallet.Menu.Types
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.About
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Page.History
import Ergvein.Wallet.Page.Network
import Ergvein.Wallet.Page.Settings

switchMenu :: MonadFront t m => Maybe (Dynamic t (m ())) -> Event t MenuItem -> m ()
switchMenu prevWidget e = void $ nextWidget $ fforMaybe e $ \go -> let
  mkNext n = Retractable {
    retractableNext = n
  , retractablePrev = prevWidget
  }
  in case go of
      MenuBalances -> Just $ mkNext balancesPage
      MenuSingleBalance cur -> Just $ mkNext $ historyPage cur
      MenuNetwork  -> Just $ mkNext $ networkPage Nothing
      MenuSettings -> Just $ mkNext settingsPage
      MenuAbout    -> Just $ mkNext aboutPage
      MenuLogs     -> Nothing
      MenuSwitch   -> Just $ mkNext $ do
        buildE <- delay 0.1 =<< getPostBuild
        storedE <- storeWalletNow "switch-menu" True buildE
        void $ setWalletInfo $ Nothing <$ storedE
