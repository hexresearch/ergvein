module Ergvein.Wallet.Page.About(
    aboutPage
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Wrapper

data AboutTitle = AboutTitle

instance LocalizedPrint AboutTitle where
  localizedShow l v = case l of
    English -> case v of
      AboutTitle  -> "About"
    Russian -> case v of
      AboutTitle  -> "О программе"

aboutPage :: MonadFront t m => m ()
aboutPage = do
  let thisWidget = Just $ pure $ aboutPage
  menuWidget AboutTitle thisWidget
  wrapper True $ do
    h3 $ localizedText $ AboutTitle
    pure ()
