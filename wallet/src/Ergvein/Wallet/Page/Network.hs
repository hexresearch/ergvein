module Ergvein.Wallet.Page.Network(
    networkPage
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Network
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Wrapper


networkPage :: MonadFront t m => m ()
networkPage = do
  let thisWidget = Just $ pure $ networkPage
  menuWidget NPSTitle thisWidget
  wrapper True $ do
    h3 $ localizedText $ NPSTitle
    pure ()
