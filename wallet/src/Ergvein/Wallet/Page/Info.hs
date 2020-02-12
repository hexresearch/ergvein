module Ergvein.Wallet.Page.Info(
    infoPage
  ) where

import Ergvein.Types.Currency
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Id
import Ergvein.Wallet.Input
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Info
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Wrapper

infoPage :: MonadFront t m => Currency -> m ()
infoPage cur = do
  let thisWidget = Just $ pure $ infoPage cur
  menuWidget (InfoTitle cur) thisWidget
  wrapper True $ do
    textLabel NameWallet "my_name_wallet"
    vertSpacer
    textLabel TotalBalance "my_name_wallet"
    vertSpacer
    textLabel ConfirmedBalance "my_name_wallet"
    vertSpacer
    textLabel MasterPublicKey "my_name_wallet"
    pure ()

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
