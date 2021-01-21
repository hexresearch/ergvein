{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Status.Widget(
    statusBarWidget
  ) where

import Data.Time

import Ergvein.Types.Currency
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Status.Types
import Ergvein.Wallet.Util
import Ergvein.Wallet.Widget.Balance

statusDisplayTime :: NominalDiffTime
statusDisplayTime = 5

statusBarWidget :: MonadFront t m => Bool -> Currency -> m ()
statusBarWidget isVerbose cur = divClass "sync-widget-wrapper" $ do
  statD <- getStatusUpdates cur
  balD  <- balanceRatedOnlyWidget cur
  langD <- getLanguage
  void $ widgetHoldDyn $ ffor balD $ \case
    Nothing -> void $ widgetHoldDyn $ renderStatus <$> statD
    Just bal -> mdo
      let updE = updated statD
      let renderE = leftmost [Just <$> updE, Nothing <$ tE]
      tE <- widgetHoldE (text bal >> pure never) $ ffor updE $ \sp -> do
        renderStatus sp
        (e,_) <- elAttr' "span" [("class", "ml-1")] $ elClass "i" "fas fa-times" $ pure ()
        let closeE = domEvent Click e
        timeoutE <- delay statusDisplayTime =<< getPostBuild
        pure $ leftmost [closeE, timeoutE]
      pure ()
  where
    renderStatus sp = if isVerbose
      then localizedText $ CurrencyStatus cur sp
      else localizedText sp
