{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Status.Widget(
    statusBarWidget
  ) where

import Data.Time
import Text.Printf
import Control.Monad.IO.Class

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Status.Types
import Ergvein.Wallet.Widget.Balance

import qualified Data.Text as T

statusDisplayTime :: NominalDiffTime
statusDisplayTime = 5

statusBarWidget :: MonadFront t m => Bool -> Currency -> m ()
statusBarWidget isVerbose cur = divClass "sync-widget-wrapper" $ do
  statD <- getWalletStatus cur
  balD <- balanceRatedOnlyWidget cur
  void $ networkHoldDyn $ ffor balD $ \case
    Nothing -> void $ networkHoldDyn $ (renderStatus . walletStatus'normal) <$> statD
    Just bal -> mdo
      let updE = updated statD
      let renderE = leftmost [(Just . walletStatus'normal) <$> updE, Nothing <$ tE]
      tE <- networkHoldE (balWidget cur bal) $ ffor renderE $ \case
        Nothing -> balWidget cur bal
        Just sp -> do
          renderStatus sp
          (e,_) <- elAttr' "span" [("class", "ml-1")] $ elClass "i" "fas fa-times" $ pure ()
          let closeE = domEvent Click e
          performFork_ $ ffor closeE $ const $ liftIO $ print "closeE"
          timeoutE <- delay statusDisplayTime =<< getPostBuild
          pure $ leftmost [closeE, timeoutE]
      pure ()
  where
    renderStatus status = if isVerbose
      then localizedText $ CurrencyStatus cur status
      else localizedText status

balWidget :: MonadFront t m => Currency -> T.Text -> m (Event t ())
balWidget cur bal = do
  text bal
  rateFiatD <- (fmap . fmap) settingsRateFiat getSettingsD
  networkHoldDyn $ ffor rateFiatD $ \mf -> maybeW mf $ \f -> do
    rateD <- getRateByFiatD cur f
    void $ networkHoldDyn $ ffor rateD $ \mr -> maybeW mr $ \r -> do
      let r' = T.pack $ printf "%.2f" (realToFrac r :: Double)
      text $ " (" <> r' <> " " <> showt cur <> "/" <> showt f <> ")"
  pure never
  where
    maybeW mv f = case mv of
      Nothing -> pure ()
      Just v -> f v
