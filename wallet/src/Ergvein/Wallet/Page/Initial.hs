module Ergvein.Wallet.Page.Initial(
    initialPage
  , initialAuthedPage
  ) where

import Ergvein.Wallet.Alert
import Ergvein.Wallet.Alert.Type
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Password
import Ergvein.Wallet.Page.Seed
import Ergvein.Wallet.Wrapper

import Control.Monad.IO.Class
import Ergvein.Wallet.Clipboard
import Ergvein.Wallet.Password

data GoPage = GoSeed | GoRestore | Insta Text

data InitialPageStrings =
    IPSCreate
  | IPSRestore

instance LocalizedPrint InitialPageStrings where
  localizedShow l v = case l of
    English -> case v of
      IPSCreate   -> "Create wallet"
      IPSRestore  -> "Restore wallet"
    Russian -> case v of
      IPSCreate   -> "Создать кошелёк"
      IPSRestore  -> "Восстановить кошелёк"

initialPage :: MonadFrontBase t m => m ()
initialPage = wrapper True $ divClass "initial-options" $ do
    newE <- fmap (GoSeed <$) $ outlineButton IPSCreate
    restoreE <- fmap (GoRestore <$) $ outlineButton IPSRestore
    let goE = leftmost [newE, restoreE]
    void $ nextWidget $ ffor goE $ \go -> Retractable {
        retractableNext = case go of
          GoSeed -> mnemonicPage
          GoRestore -> seedRestorePage
      , retractablePrev = Just $ pure initialPage
      }

initialAuthedPage :: MonadFront t m => m ()
initialAuthedPage = wrapper True $ divClass "main-page" $ do
  anon_name <- getWalletName
  h4 $ text $ "Congrats " <> anon_name <> "! You've made it!"
  logoutE <- row . outlineButton $ ("Logout" :: Text)
  void $ setAuthInfo $ Nothing <$ logoutE
