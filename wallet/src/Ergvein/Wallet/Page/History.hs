module Ergvein.Wallet.Page.History(
    historyPage
  ) where

import Ergvein.Types.Currency
--import Ergvein.Wallet.Camera
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Navbar.Types
--import Ergvein.Wallet.Page.Info
--import Ergvein.Wallet.Page.Share
import Ergvein.Wallet.Wrapper

newtype HistoryTitle = HistoryTitle Currency

instance LocalizedPrint HistoryTitle where
  localizedShow l (HistoryTitle c) = case l of
    English -> "History " <> currencyName c
    Russian -> "История " <> currencyName c

historyPage :: MonadFront t m => Currency -> m ()
historyPage cur = divClass "base-container" $ do
  let thisWidget = Just $ pure $ historyPage cur
  headerWidget (HistoryTitle cur) thisWidget
  navbarWidget cur thisWidget NavbarHistory
  void $ divClass "centered-wrapper" $ divClass "centered-content" $ h3 $ localizedText $ HistoryTitle cur
--    cameraE <- fmap ("Test" <$) $ outlineButton ("Debug QR scan"::Text)
--    _ <- openCamara cameraE
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
