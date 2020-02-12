module Ergvein.Wallet.Page.Info(
    infoPage
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Keys
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
import Network.Haskoin.Keys

infoPage :: MonadFront t m => Currency -> m ()
infoPage cur = do
  let thisWidget = Just $ pure $ infoPage cur
  menuWidget (InfoTitle cur) thisWidget
  wrapper True $ do
    walName <- getWalletName
    textLabel NameWallet walName
    vertSpacer

    settings <- getSettings
    let setUs = getSettingsUnits settings

    bal <- currencyBalance cur
    let balD = (\v -> showMoneyUnit v setUs) <$> bal
    balVal <- sample . current $ balD
    textLabel TotalBalance balVal
    vertSpacer

    balCfrm <- currencyBalanceConfirm cur
    let balCfrmD = (\v -> showMoneyUnit v setUs) <$> balCfrm
    balCfrmVal <- sample . current $ balCfrmD
    textLabel ConfirmedBalance balCfrmVal
    vertSpacer

    pks :: PublicKeystore <- getPublicKeystore
    let masterPKeyMb = (xPubExport (getCurrencyNetwork cur)  . egvXPubKey . egvPubKeyсhain'master) <$> M.lookup cur pks
    textLabel MasterPublicKey $ fromMaybe "" masterPKeyMb
    pure ()
    where
      getSettingsUnits = fromMaybe defUnits . settingsUnits

textLabel :: (MonadFrontBase t m, LocalizedPrint l)
  => l -- ^ Label
  -> Text -- ^ Value
  -> m ()
textLabel lbl val = do
  i <- genId
  label i $ localizedText lbl
  let cfg = def { _textInputConfig_initialValue = val }
  textInput cfg {
      _textInputConfig_attributes = do
        as <- _textInputConfig_attributes cfg
        pure $ "id" =: i <> "readonly" =: "" <> as
    }
  pure ()

vertSpacer :: MonadFrontBase t m => m ()
vertSpacer = divClass "info-v-spacer" blank

currencyBalance :: MonadFront t m => Currency -> m (Dynamic t Money)
currencyBalance cur = pure $ pure $ Money cur 1

currencyBalanceConfirm :: MonadFront t m => Currency -> m (Dynamic t Money)
currencyBalanceConfirm = currencyBalance
