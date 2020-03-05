module Ergvein.Wallet.Page.History(
    historyPage
  ) where

import Ergvein.Types.Currency
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types
--import Ergvein.Wallet.Page.Info
--import Ergvein.Wallet.Page.Share
import Ergvein.Wallet.Wrapper

data CurrencyTitle = CurrencyTitle !Currency

instance LocalizedPrint CurrencyTitle where
  localizedShow l (CurrencyTitle c) = case l of
    English -> "History " <> currencyName c
    Russian -> "История " <> currencyName c

historyPage :: MonadFront t m => Currency -> m ()
historyPage cur = do
  let thisWidget = Just $ pure $ historyPage cur
  menuWidget (CurrencyTitle cur) thisWidget
  navbarWidget cur thisWidget NavbarHistory
  wrapper True $ do
    h3 $ localizedText $ CurrencyTitle cur
--    goE <- fmap (cur <$) $ outlineButton ("Debug info"::Text)
--    void $ nextWidget $ ffor goE $ \cr -> Retractable {
--        retractableNext = sharePage cr
--      , retractablePrev = thisWidget
--      }
--    goE <- fmap (cur <$) $ outlineButton ("Debug info"::Text)
--    void $ nextWidget $ ffor goE $ \cr -> Retractable {
--        retractableNext = infoPage cr
--      , retractablePrev = thisWidget
--      }
    pure ()
