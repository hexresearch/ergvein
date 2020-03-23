{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Page.Info(
    infoPage
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Network
import Ergvein.Types.Storage
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Id
import Ergvein.Wallet.Input
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Info
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Wrapper

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

infoPage :: MonadFront t m => Currency -> m ()
infoPage cur = wrapper (InfoTitle cur) (Just $ pure $ infoPage cur) False $ divClass "info-content" $ do
  walName <- getWalletName
  textLabel NameWallet $ text walName
  vertSpacer

  settings <- getSettings
  let setUs = getSettingsUnits settings

  bal <- currencyBalance cur
  let balD = (\v -> showMoneyUnit v setUs) <$> bal
  balVal <- sample . current $ balD
  textLabel TotalBalance $ text balVal
  vertSpacer

  balCfrm <- currencyBalanceConfirm cur
  let balCfrmD = (\v -> showMoneyUnit v setUs) <$> balCfrm
  balCfrmVal <- sample . current $ balCfrmD
  textLabel ConfirmedBalance $ text balCfrmVal
  vertSpacer

  pks :: PublicKeystore <- getPublicKeystore
  let masterPKeyMb = (xPubExport (getCurrencyNetwork cur) . egvXPubKey . egvPubKeyсhain'master) <$> M.lookup cur pks
      partsPKey = T.chunksOf 20 $ fromMaybe "" masterPKeyMb
  textLabel MasterPublicKey $ mapM_ (\v -> text v >> br) partsPKey
  pure ()
  where
    getSettingsUnits = fromMaybe defUnits . settingsUnits

textLabel :: (MonadFrontBase t m, LocalizedPrint l)
  => l -- ^ Label
  -> m a -- ^ Value
  -> m ()
textLabel lbl val = do
  i <- genId
  label i $ localizedText lbl
  elAttr "div" [("class","info-block-value"),("id",i)] $ val
  pure ()

vertSpacer :: MonadFrontBase t m => m ()
vertSpacer = divClass "info-v-spacer" blank

currencyBalance :: MonadFront t m => Currency -> m (Dynamic t Money)
currencyBalance cur = pure $ pure $ Money cur 1

currencyBalanceConfirm :: MonadFront t m => Currency -> m (Dynamic t Money)
currencyBalanceConfirm = currencyBalance
