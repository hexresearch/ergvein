module Ergvein.Wallet.Page.About(
    aboutPage
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Network
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Wrapper

aboutPage :: MonadFront t m => m ()
aboutPage = do
  let thisWidget = Just $ pure $ aboutPage
  menuWidget NPSTitle thisWidget
  wrapper True $ do
    h3 $ localizedText $ NPSTitle
    pure ()
