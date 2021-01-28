module Ergvein.Wallet.Widget.Balance(
    balancesWidget
  , balancesRatedWidget
  , balanceRatedOnlyWidget
  , balanceTitleWidget
  , balanceTitleWidgetSimple
  ) where

import Control.Lens
import Data.Maybe (fromMaybe)

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Utxo.Btc
import Ergvein.Types.Utxo.Status
import Ergvein.Types.Storage.Currency.Public.Btc
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.History
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Util

import qualified Data.List as L
import qualified Data.Map.Strict as M

ergoBalances :: MonadFront t m => m (Dynamic t Money)
ergoBalances = pure $ pure $ Money ERGO 0

btcBalances :: MonadFront t m => m (Dynamic t Money)
btcBalances = do
  pubStorageD <- getPubStorageBtcD
  pure $ ffor pubStorageD $ \case
    Nothing -> Money BTC 0
    Just pubStorage -> let
      utxos = M.elems $ pubStorage ^. btcPubStorage'utxos
      in Money BTC $ L.foldl' helper 0 utxos
  where
    helper :: MoneyUnit -> BtcUtxoMeta -> MoneyUnit
    helper balance BtcUtxoMeta{btcUtxo'status = EUtxoSending _} = balance
    helper balance BtcUtxoMeta{..} = balance + btcUtxo'amount

balancesWidget :: MonadFront t m => Currency -> m (Dynamic t Money)
balancesWidget cur = case cur of
  ERGO -> ergoBalances
  BTC  -> btcBalances

balancesRatedWidget :: MonadFront t m => Currency -> m (Dynamic t Text, Dynamic t Text)
balancesRatedWidget cur = do
  settings <- getSettings
  balD <- balancesWidget cur
  let setUs = getSettingsUnits settings
  let mFiat = settingsFiatCurr settings
  case mFiat of
    Nothing -> pure $ splitDynPure $ ffor balD $ \bal ->
      let u = symbolUnit cur setUs
          b = showMoneyUnit bal setUs
      in (b, u)
    Just f -> do
      rateD <- getRateByFiatD cur f
      pure $ splitDynPure $ do
        bal <- balD
        rate <- rateD
        let unrated = (showMoneyUnit bal setUs, symbolUnit cur setUs)
        pure $ case rate of
          Nothing -> unrated
          Just r -> if cur == BTC
            then (showMoneyRated bal r, showt f)
            else unrated
  where getSettingsUnits = fromMaybe defUnits . settingsUnits

balanceRatedOnlyWidget :: MonadFront t m => Currency -> m (Dynamic t (Maybe Text))
balanceRatedOnlyWidget cur = if cur /= BTC then pure (pure Nothing) else do
  mRateSymbolD <- (fmap . fmap) settingsFiatCurr getSettingsD
  fmap join $ widgetHoldDyn $ ffor mRateSymbolD $ \case
    Nothing -> pure $ pure Nothing
    Just rs -> do
      balD <- balancesWidget cur
      rateD <- getRateByFiatD cur rs
      pure $ do
        bal <- balD
        mRate <- rateD
        pure $ case mRate of
          Nothing -> Nothing
          Just r -> Just $ showMoneyRated bal r <> " " <> showt rs

balanceTitleWidget :: MonadFront t m => Currency -> m (Dynamic t Text)
balanceTitleWidget cur = do
  bal <- balancesWidget cur
  settings <- getSettings
  titleText <- localized $ HistoryBalance
  let getSettingsUnits = fromMaybe defUnits . settingsUnits
      setUs = getSettingsUnits settings
      titleVal = ffor bal (\v -> showMoneyUnit v setUs)
      curSymbol = symbolUnit cur setUs
      title = zipDynWith (\x y -> x <> ": " <> y <> " " <> curSymbol) titleText titleVal
  pure title

balanceTitleWidgetSimple :: MonadFront t m => Currency -> m (Dynamic t Text)
balanceTitleWidgetSimple cur = do
  bal <- balancesWidget cur
  settings <- getSettings
  let getSettingsUnits = fromMaybe defUnits . settingsUnits
      setUs = getSettingsUnits settings
      titleVal = ffor bal (\v -> showMoneyUnit v setUs)
      curSymbol = symbolUnit cur setUs
      title = (\x -> x <> " " <> curSymbol) <$> titleVal
  pure title
