module Ergvein.Wallet.Page.Info(
    infoPage
  ) where

import Ergvein.Types.Currency
import Ergvein.Wallet.Elements
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
    h3 $ localizedText $ InfoTitle cur
    pure ()
