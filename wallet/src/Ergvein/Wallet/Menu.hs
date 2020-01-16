module Ergvein.Wallet.Menu(
    menuWidget
  ) where

import Ergvein.Wallet.Elements
import Ergvein.Wallet.Embed
import Ergvein.Wallet.Embed.TH
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad

import {-# SOURCE #-} Ergvein.Wallet.Page.Settings (settingsPage)

data MenuItem = MenuNetwork | MenuSettings | MenuAbout | MenuLogs | MenuSwitch

instance LocalizedPrint MenuItem where
  localizedShow l v = case l of
    English -> case v of
      MenuNetwork -> "Network"
      MenuSettings -> "Settings"
      MenuAbout -> "About"
      MenuLogs -> "Logs"
      MenuSwitch -> "Switch wallet"
    Russian -> case v of
      MenuNetwork -> "Сеть"
      MenuSettings -> "Настройки"
      MenuAbout -> "О программе"
      MenuLogs -> "Логи"
      MenuSwitch -> "Сменить кошелёк"

menuWidget :: MonadFront t m => Text -> Maybe (Dynamic t (m ())) -> m ()
menuWidget titleTxt prevWidget = divClass "menu-header" $ do
  divClass "menu-wallet-name" $ text titleTxt -- "Default wallet"
  divClass "menu-wallet-menu" $ do
    menuIconUrl <- createObjectURL menuIcon
    divClass "menu-dropdown-wrapper" $ do
      btnE <- divButton "menu-button" $ imgClass menuIconUrl ""
      divClass "menu-dropdown" $ do
        let menuBtn v = (v <$) <$> clearButton v
        netE <- menuBtn MenuNetwork
        setE <- menuBtn MenuSettings
        abtE <- menuBtn MenuAbout
        logE <- menuBtn MenuLogs
        switchE <- menuBtn MenuSwitch
        switchMenu prevWidget $ leftmost [netE, setE, abtE, logE, switchE]

switchMenu :: MonadFront t m => Maybe (Dynamic t (m ())) -> Event t MenuItem -> m ()
switchMenu prevWidget e = void $ nextWidget $ fforMaybe e $ \go -> let
  mkNext n = Retractable {
    retractableNext = n
  , retractablePrev = prevWidget
  }
  in case go of
      MenuNetwork  -> Nothing -- TODO: use mkNext when we have corresponding pages
      MenuSettings -> Just $ mkNext settingsPage
      MenuAbout    -> Nothing
      MenuLogs     -> Nothing
      MenuSwitch   -> Just $ mkNext $ do
        buildE <- delay 0.1 =<< getPostBuild
        void $ setAuthInfo $ Nothing <$ buildE
