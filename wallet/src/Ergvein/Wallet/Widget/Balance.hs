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
import Ergvein.Types.Storage.Currency.Public.Btc
import Ergvein.Types.Utxo.Btc
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Settings
import Sepulcas.Text (Display(..))

import qualified Data.List as L
import qualified Data.Map.Strict as M

btcBalances :: MonadFront t m => m (Dynamic t Money)
btcBalances = do
  pubStorageD <- getPubStorageBtcD
  pure $ ffor pubStorageD $ \case
    Nothing -> Money BTC 0
    Just pubStorage -> let
      utxos = M.elems $ pubStorage ^. btcPubStorage'utxos
      in Money BTC $ L.foldl' helper 0 utxos
  where
    helper :: MoneyAmount -> BtcUtxoMeta -> MoneyAmount
    helper balance BtcUtxoMeta{btcUtxo'status = EUtxoSending _} = balance
    helper balance BtcUtxoMeta{..} = balance + btcUtxo'amount

ergoBalances :: MonadFront t m => m (Dynamic t Money)
ergoBalances = pure $ pure $ Money ERGO 0

balancesWidget :: MonadFront t m => Currency -> m (Dynamic t Money)
balancesWidget cur = case cur of
  BTC  -> btcBalances
  ERGO -> ergoBalances

balancesRatedWidget :: MonadFront t m => Currency -> m (Dynamic t Text, Dynamic t Text)
balancesRatedWidget cur = case cur of
  BTC  -> btcBalancesRatedWidget
  ERGO -> ergBalancesRatedWidget

btcBalancesRatedWidget :: MonadFront t m => m (Dynamic t Text, Dynamic t Text)
btcBalancesRatedWidget = do
  settings <- getSettings
  balD <- btcBalances
  units <- getSettingsUnitBtc
  let mFiat = settingsFiatCurr settings
  case mFiat of
    Nothing -> pure $ splitDynPure $ ffor balD $ \bal ->
      let u = display units
          b = showMoneyUnit bal units
      in (b, u)
    Just f -> do
      rateD <- getRateByFiatD BTC f
      pure $ splitDynPure $ do
        bal <- balD
        rate <- rateD
        let unrated = (showMoneyUnit bal units, display units)
        pure $ case rate of
          Nothing -> unrated
          Just r -> (showMoneyRated bal r, showt f)

ergBalancesRatedWidget :: MonadFront t m => m (Dynamic t Text, Dynamic t Text)
ergBalancesRatedWidget = pure (pure "", pure "")

balanceRatedOnlyWidget :: MonadFront t m => Currency -> m (Dynamic t (Maybe Text))
balanceRatedOnlyWidget cur = case cur of
  BTC  -> btcBalanceRatedOnlyWidget
  ERGO -> ergBalanceRatedOnlyWidget

btcBalanceRatedOnlyWidget :: MonadFront t m => m (Dynamic t (Maybe Text))
btcBalanceRatedOnlyWidget = do
  mRateSymbolD <- (fmap . fmap) settingsFiatCurr getSettingsD
  fmap join $ networkHoldDyn $ ffor mRateSymbolD $ \case
    Nothing -> pure $ pure Nothing
    Just rs -> do
      balD <- balancesWidget BTC
      rateD <- getRateByFiatD BTC rs
      pure $ do
        bal <- balD
        mRate <- rateD
        pure $ case mRate of
          Nothing -> Nothing
          Just r -> Just $ showMoneyRated bal r <> " " <> showt rs

ergBalanceRatedOnlyWidget :: MonadFront t m => m (Dynamic t (Maybe Text))
ergBalanceRatedOnlyWidget = pure (pure Nothing)

balanceTitleWidget :: MonadFront t m => Currency -> m (Dynamic t Text)
balanceTitleWidget cur = case cur of
  BTC  -> btcBalanceTitleWidget
  ERGO -> ergBalanceTitleWidget

btcBalanceTitleWidget :: MonadFront t m => m (Dynamic t Text)
btcBalanceTitleWidget = do
  bal <- balancesWidget BTC
  units <- getSettingsUnitBtc
  titleText <- localized HistoryBalance
  let titleVal = ffor bal (`showMoneyUnit` units)
      curSymbol = display units
      title = zipDynWith (\x y -> x <> ": " <> y <> " " <> curSymbol) titleText titleVal
  pure title

ergBalanceTitleWidget :: MonadFront t m => m (Dynamic t Text)
ergBalanceTitleWidget = do
  bal <- balancesWidget ERGO
  units <- getSettingsUnitErg
  titleText <- localized HistoryBalance
  let titleVal = ffor bal (`showMoneyUnit` units)
      curSymbol = display units
      title = zipDynWith (\x y -> x <> ": " <> y <> " " <> curSymbol) titleText titleVal
  pure title

balanceTitleWidgetSimple :: MonadFront t m => Currency -> m (Dynamic t Text)
balanceTitleWidgetSimple cur = case cur of
  BTC  -> btcBalanceTitleWidgetSimple
  ERGO -> ergBalanceTitleWidgetSimple

btcBalanceTitleWidgetSimple :: MonadFront t m => m (Dynamic t Text)
btcBalanceTitleWidgetSimple = do
  bal <- balancesWidget BTC
  units <- getSettingsUnitBtc
  let titleVal = ffor bal (`showMoneyUnit` units)
      curSymbol = display units
      title = (\x -> x <> " " <> curSymbol) <$> titleVal
  pure title

ergBalanceTitleWidgetSimple :: MonadFront t m => m (Dynamic t Text)
ergBalanceTitleWidgetSimple = do
  bal <- balancesWidget ERGO
  units <- getSettingsUnitErg
  let titleVal = ffor bal (`showMoneyUnit` units)
      curSymbol = display units
      title = (\x -> x <> " " <> curSymbol) <$> titleVal
  pure title
